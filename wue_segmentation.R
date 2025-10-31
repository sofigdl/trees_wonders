install.packages("pacman")

#Install packages
pacman::p_load(car, sf, terra, dplyr, ggplot2, RStoolbox, raster, rgeos, lidR, EBImage, caTools, FNN)

################################################################################
#                              Tree segmentation
################################################################################
# Define the folder containing the images
folder_path <- "D:/Wurzburg/CIR"  # Replace with your folder path

# List all files in the folder that start with "dop20datenabgabe" and have a ".tif" extension
file_list <- list.files(folder_path, pattern = "^dop20datenabgabe_.*\\.tif$", full.names = TRUE)

# Dynamically load rasters and assign unique variable names
raster_list <- list()
for (i in seq_along(file_list)) {
  # Load the raster
  raster <- rast(file_list[i])
  
  # Assign a unique name to the raster (e.g., cir_6, cir_5, etc.)
  raster_name <- paste0("cir_", i)
  assign(raster_name, raster)  # Assign the raster to a variable in the environment
  
  # Store the raster in a list for merging
  raster_list[[i]] <- raster
}

# Merge all rasters
cir <- do.call(merge, raster_list)
plot(cir, col=rev(terrain.colors(100)), main="CIR")

# Assuming the CIR bands are: Red = Band 1, NIR = Band 4
red_band <- cir[[2]]  # Red band
nir_band <- cir[[1]]  # NIR band

# Calculate NDVI
ndvi <- (nir_band - red_band) / (nir_band + red_band)
plot(ndvi, col=rev(terrain.colors(100)), main="NDVI")

ndvi_filter<-as.numeric(ndvi>0.1)
plot(ndvi_filter, col=rev(terrain.colors(100)), main="NDVI")


#-------------------------------------------------------------------------------

# Define the folder containing the DSM rasters
folder_path <- "D:/Wurzburg/DSM"

# List all DSM files in the folder matching the pattern (e.g., "32567_*_20_DOM.tif")
dsm_file_list <- list.files(folder_path, pattern = "^325.*_20_DOM\\.tif$", full.names = TRUE)

# Dynamically load DSM rasters and assign unique variable names
dsm_raster_list <- list()
for (i in seq_along(dsm_file_list)) {
  # Load the raster
  raster <- rast(dsm_file_list[i])
  
  # Assign a unique name to the raster (e.g., dsm_1, dsm_2, etc.)
  raster_name <- paste0("dsm_", i)
  assign(raster_name, raster)  # Assign the raster to a variable in the environment
  
  # Store the raster in a list for merging
  dsm_raster_list[[i]] <- raster
}

# Merge all DSM rasters
dsm <- do.call(merge, dsm_raster_list)
plot(dsm, col=rev(terrain.colors(100)), main="DSM")
#------------------------------------------------------------------------------
# Define the folder containing the DTM rasters
folder_path <- "D:/Wurzburg/DTM"

# List all DSM files in the folder matching the pattern (e.g., "32567_*_20_DOM.tif")
dtm_file_list <- list.files(folder_path, pattern = "^56.*\\.tif$", full.names = TRUE)

# Dynamically load DSM rasters and assign unique variable names
dtm_raster_list <- list()
for (i in seq_along(dtm_file_list)) {
  # Load the raster
  raster <- rast(dtm_file_list[i])
  
  # Assign a unique name to the raster (e.g., dsm_1, dsm_2, etc.)
  raster_name <- paste0("dtm_", i)
  assign(raster_name, raster)  # Assign the raster to a variable in the environment
  
  # Store the raster in a list for merging
  dtm_raster_list[[i]] <- raster
}

# Merge all DSM rasters
dtm <- do.call(merge, dtm_raster_list)


#-------------------------------------------------------------------------------

# Find the intersection of their extents
common_extent <- intersect(ext(dtm), intersect(ext(dsm), ext(ndvi)))

# Crop the rasters to the common extent
dtm_clipped <- crop(dtm, common_extent)
dsm_clipped <- crop(dsm, common_extent)
ndvi_clipped <- crop(ndvi_filter, common_extent)

# Resample the DTM to match the DSM resolution
dtm_resampled <- resample(dtm_clipped, dsm_clipped, method="bilinear")  # Choose "bilinear" for smoother results

# Calculate the Canopy Height Model (CHM)
ndsm <- dsm_clipped - dtm_resampled
plot(ndsm)

chm<-ndsm*ndvi_clipped

plot(chm,  col=rev(terrain.colors(100)), main="CHM")

# Apply the threshold
chm_threshold <- classify(chm, matrix(c(-Inf, 2, NA), ncol=3, byrow=TRUE))

# Plot the thresholded CHM
plot(chm_smoothed, col=terrain.colors(100), main="CHM (Threshold > 2 m)")

#We smooth the chm
kernel <- matrix(1,3,3)
chm_smoothed <- terra::focal(chm_threshold, w = kernel, fun = median, na.rm = TRUE)
writeRaster(chm_smoothed, "D:/Wurzburg/Segments/chm_smoothed_wue.tif")
chm_smoothed <- rast("D:/Wurzburg/Segments/chm_smoothed_wue.tif")

#locate the tree tops
ttops_2<-locate_trees(chm_smoothed, lmf(ws=5, hmin=2, shape="circular"))
plot(ttops_2, add=TRUE)


#Segment the image
silva<-silva2016(chm_smoothed, ttops_2, max_cr_factor = 0.5, exclusion = 0.3)
silva_crowns<-silva()

plot(silva_crowns, col=pastel.colors(200))
#writeRaster(silva_crowns, "D:/Wurzburg/Segments/silva_crowns_t4.tif")

#polygonize

silva_polygons <- as.polygons(silva_crowns, dissolve=TRUE)  # Convert raster to polygons
silva_polygons <- st_as_sf(silva_polygons)  # Convert to sf object for easier handling

# Step 2: Convert Multi-Part to Single-Part
silva_polygons <- st_cast(silva_polygons, "POLYGON")  # Split multi-part polygons into single parts

# Add unique I
plot(silva_polygons)

# Load the hausumringe, buildings footprints
hausumringe<-st_read("D:/Wurzburg/hausumringe.shp")
hausumringe <- st_simplify(hausumringe, dTolerance=0.1)


# Ensure polygons have a unique ID
silva_polygons$ID <- seq_len(nrow(silva_polygons))  # Create unique IDs for polygons

# Step 2: Compute intersection areas with `hausringe`
hausringe_union <- st_union(hausumringe)  # Merge hausringe polygons into one geometry
intersection <- st_intersection(silva_polygons, hausringe_union)  # Get intersection polygons

# Calculate areas
silva_polygons$area_total <- st_area(silva_polygons)  # Total area of each polygon
intersection$area_intersect <- st_area(intersection)  # Intersection area

# Match intersection areas to the original polygons
intersection_summary <- intersection %>% 
  st_drop_geometry() %>%  # Remove geometry to work with data
  group_by(ID) %>%        # Group by polygon ID
  summarise(area_intersect = sum(area_intersect))  # Sum intersection areas for each polygon

# Merge back the intersection summary with the original polygons
silva_polygons <- merge(silva_polygons, intersection_summary, by = "ID", all.x = TRUE)

# Replace NA values in area_intersect (no intersection) with 0
silva_polygons$area_intersect[is.na(silva_polygons$area_intersect)] <- units::set_units(0, m^2)

# Calculate intersection ratio 
silva_polygons$intersection_ratio <- as.numeric(silva_polygons$area_intersect) / as.numeric(silva_polygons$area_total)
silva_filtered <- silva_polygons[silva_polygons$intersection_ratio <= 0.75, ]# Keep polygons with ≤80% overlap
silva_filtered <- silva_filtered[silva_filtered$area_total >= units::set_units(2, m^2), ]


plot(rgb, col=rev(terrain.colors(100)), main="RGB")
plot(silva_filtered, add=TRUE)



################################################################################
#                                  Height
################################################################################

pcth <- function(x, p=0.90, na.rm = TRUE) { quantile(x, p, na.rm = na.rm) }
silva_filtered$height <- (terra::extract(chm_threshold, silva_filtered, fun=pcth))[2]


names(silva_filtered)

################################################################################
#                              Area and perimeter
################################################################################

silva_filtered$area <- st_area(silva_filtered)  # Automatically adds units (e.g., m^2)

# Calculate perimeter
silva_filtered$perimeter <- st_length(st_cast(silva_filtered, "MULTILINESTRING"))

################################################################################
#                                  Diameter
################################################################################

# Create centroids
centroids <- st_centroid(silva_filtered)

# Count total of shapes
TotalTrees <- length(centroids[[1]])

# Precompute centroid coordinates
centroid_coords <- st_coordinates(centroids)

# Initialize vectors to store results
min_lengths <- numeric(TotalTrees)
med_lengths <- numeric(TotalTrees)
mean_lengths <- numeric(TotalTrees)
max_lengths <- numeric(TotalTrees)

# Create progress bar
pb <- txtProgressBar(min = 1, max = TotalTrees, style = 3)

# Loop over all available tree shapes
for (i in 1:TotalTrees) {
  
  # Convert polygon into points
  PolPoints <- st_cast(silva_filtered[i,], "POINT")
  
  # Initialize variables to store lengths
  Lengthvector <- numeric(length(PolPoints[[1]]))
  
  # Loop over all polygon points to create the lines between centroid and polygon edges
  for (j in 1:length(PolPoints[[1]])) {
    # Define a set of coordinates to create line from the two points
    coordinates <- matrix(c(
      st_coordinates(PolPoints[j,][1])[1], centroid_coords[i,][1],
      st_coordinates(PolPoints[j,][1])[2], centroid_coords[i,][2]
    ), ncol = 2)
    
    # Create an sf LineString object
    line <- st_linestring(coordinates) %>% st_sfc() %>% st_set_crs(st_crs(centroids))
    
    # Calculate length of line
    Length <- st_length(line)
    
    # Store the length in the vector
    Lengthvector[j] <- Length
  }
  
  # Calculate fields from the lines and store them in vectors
  min_lengths[i] <- round(min(Lengthvector), 2)
  med_lengths[i] <- round(median(Lengthvector), 2)
  mean_lengths[i] <- round(mean(Lengthvector), 2)
  max_lengths[i] <- round(max(Lengthvector), 2)
  
  # Increment progress bar
  setTxtProgressBar(pb, i)
  
  # Close progress bar when done
  if(i == TotalTrees){
    close(pb)
  }
}

# Assign the vectors to the centroids data frame
centroids$min <- min_lengths
centroids$med <- med_lengths
centroids$mean <- mean_lengths
centroids$max <- max_lengths

silva_filtered$diam<-(centroids$mean*2)

st_write(silva_filtered, "D:/Wurzburg/Segments/ringpark_1.shp")

