install.packages("pacman")
library(pacman)
#Install packages
pacman::p_load(sf, terra, dplyr, future.apply, forestTools, nngeo, ggplot2, RStoolbox, raster, rgeos, lidR, EBImage, caTools, FNN)

# Set working paths
cir_dir <- "D:/Wurzburg/CIR/"
dsm_dir <- "D:/Wurzburg/DSM/"
dtm_dir <- "D:/Wurzburg/DTM/"
output_dir <- "D:/Wurzburg/Segments/ProcessedTiles/"

# Get list of CIR tiles
cir_files <- list.files(cir_dir, pattern = "\\.tif$", full.names = TRUE)

# Create output dir if needed
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Tile-wise processing function
process_tile <- function(cir_path, dsm="D:/Wurzburg/DSM/", dtm="D:/Wurzburg/DTM/") { 
  tile_id <- tools::file_path_sans_ext(basename(cir_path))
  cat("Processing:", tile_id, "\n")
  # Infer corresponding DSM/DTM file names
  dsm_file <- file.path(dsm_dir, paste0("32", tile_id, "_20_DOM.tif"))
  dtm_file <- file.path(dtm_dir, paste0(tile_id, ".tif"))
  
  if (!file.exists(dsm_file) || !file.exists(dtm_file)) {
    warning("Missing DSM or DTM for: ", tile_id)
    return(NULL)
  }
  
  # Load rasters
  cir <- rast(cir_path)
  dsm <- rast(dsm_file)
  dtm <- rast(dtm_file)
  
  # NDVI
  red_band <- cir[[2]]
  nir_band <- cir[[1]]
  ndvi <- (nir_band - red_band) / (nir_band + red_band)
  ndvi[is.nan(ndvi)] <- NA
  ndvi_filtered <- focal(ndvi, w = matrix(1, 3, 3), fun = median, na.policy = "omit")
  ndvi_mask <- ndvi_filtered > 0.1
  
  # Resample DTM to DSM
  dtm_resampled <- resample(dtm, dsm)
  
  # Canopy Height Model
  chm <- dsm - dtm_resampled
  chm[chm < 0] <- 0
  chm <- chm * ndvi_mask
  
  # Smoothing and thresholding
  chm_smoothed <- focal(chm, w = matrix(1, 3, 3), fun = median, na.rm = TRUE)
  chm_threshold <- classify(chm_smoothed, matrix(c(-Inf, 2, NA), ncol=3, byrow=TRUE))
  
  # Tree top detection
  tree_tops <- tryCatch({
    locate_trees(chm_threshold, lmf(ws = 5, hmin = 1, shape = "circular"))
  }, error = function(e) return(NULL))
  
  if (is.null(tree_tops)) {
    warning("Tree detection failed for: ", tile_id)
    return(NULL)
  }
  
  # Segmentation
  silva <- silva2016(chm_threshold, tree_tops, max_cr_factor = 0.5, exclusion = 0.3)
  crowns <- silva()
  
  #crowns_poly<-crowns
  
  # Polygonize
  crowns_poly <- as.polygons(crowns, dissolve = TRUE) %>%
    st_as_sf() %>%
    st_cast("MULTIPOLYGON")%>%
    st_cast("POLYGON")
  
  # Add unique ID
  crowns_poly$tile <- tile_id
  
  # Height estimation
  pcth <- function(x, p=0.90, na.rm = TRUE) { quantile(x, p, na.rm = na.rm) }
  crowns_poly$height <- (terra::extract(chm_threshold, crowns_poly, fun=pcth))[,2]
  
  # Area
  crowns_poly$area <- st_area(crowns_poly)
  crowns_poly$perimeter <- st_length(st_cast(crowns_poly, "MULTILINESTRING"))
  
  # Diameter
  centroids <- st_centroid(crowns_poly)
  TotalTrees <- length(centroids[[1]])
  centroid_coords <- st_coordinates(centroids)
  min_lengths <- numeric(TotalTrees)
  mean_lengths <- numeric(TotalTrees)
  
  pb <- txtProgressBar(min = 1, max = TotalTrees, style = 3)
  for (i in 1:TotalTrees) {
    PolPoints <- st_cast(crowns_poly[i,], "POINT")
    Lengthvector <- sapply(1:length(PolPoints[[1]]), function(j) {
      coords <- matrix(c(
        st_coordinates(PolPoints[j,][1])[1], centroid_coords[i,1],
        st_coordinates(PolPoints[j,][1])[2], centroid_coords[i,2]
      ), ncol = 2)
      st_length(st_linestring(coords) %>% st_sfc() %>% st_set_crs(st_crs(centroids)))
    })
    min_lengths[i] <- round(min(Lengthvector), 2)
    mean_lengths[i] <- round(mean(Lengthvector), 2)
    setTxtProgressBar(pb, i)
  }
  close(pb)
  crowns_poly$diam <- mean_lengths * 2
  
  # Save outputs
  #out_file <- file.path(output_dir, paste0("crowns_", tile_id, ".tif"))
  #writeRaster(crowns_poly, out_file, filetype= "GTiff", overwrite = TRUE)
  out_file <- file.path(output_dir, paste0("crowns_", tile_id, ".gpkg"))
  st_write(crowns_poly, out_file, quiet = TRUE)
  return(out_file)
}

#plan(multisession)  # Adjust to multicore if on Linux
output_files <- future_lapply(cir_files, process_tile)
#output_files <- list.files(output_dir, pattern = "\\.gpkg$", full.names = TRUE)


# Merge all processed polygons into one
crowns_all <- do.call(rbind, lapply(output_files, st_read))
# Ensure polygons have a unique ID
crowns_all$ID <- seq_len(nrow(crowns_all))  # Create unique IDs for polygons



# Load the hausumringe, buildings footprints
hausumringe<-st_read("D:/Wurzburg/hausumringe.shp")
hausumringe <- st_simplify(hausumringe, dTolerance=0.1)
hausringe_union <- st_union(hausumringe)  # Merge hausringe polygons into one geometry

# Compute intersection areas with `hausringe`
intersection <- st_intersection(crowns_all, hausringe_union)  # Get intersection polygons

# Calculate areas
crowns_all$area_total <- st_area(crowns_all)  # Total area of each polygon
intersection$area_intersect <- st_area(intersection)  # Intersection area

# Match intersection areas to the original polygons
intersection_summary <- intersection %>% 
  st_drop_geometry() %>%  # Remove geometry to work with data
  group_by(ID) %>%        # Group by polygon ID
  summarise(area_intersect = sum(area_intersect))  # Sum intersection areas for each polygon

# Merge back the intersection summary with the original polygons
crowns_all <- merge(crowns_all, intersection_summary, by = "ID", all.x = TRUE)

# Replace NA values in area_intersect (no intersection) with 0
crowns_all$area_intersect[is.na(crowns_all$area_intersect)] <- units::set_units(0, m^2)

# Calculate intersection ratio 
crowns_all$intersection_ratio <- as.numeric(crowns_all$area_intersect) / as.numeric(crowns_all$area_total)
crowns_filtered <- crowns_all[crowns_all$intersection_ratio <= 0.75, ]# Keep polygons with ≤80% overlap
crowns_filtered <- crowns_filtered[crowns_filtered$area_total >= units::set_units(2, m^2), ]

st_write(crowns_filtered,  "D:/Wurzburg/Segments/merged_crowns_all.gpkg")

