#Test for the central area of Würzburg
install.packages("pacman")
install.packages('BiocManager')
library(lidR)
#Install packages
pacman::p_load(car, sf, terra, dplyr, ggplot2, RStoolbox, raster, lidR, EBImage, caTools, FNN, RANN)

################################################################################
#                              Tree segmentation
################################################################################

#Load rasters
cir_6<-rast("D:/Wurzburg/CIR/566_5515.tif")
cir_5<-rast("D:/Wurzburg/CIR/566_5516.tif")
cir_1<-rast("D:/Wurzburg/CIR/567_5515.tif")
cir_12<-rast("D:/Wurzburg/CIR/567_5516.tif")

# Create the mosaic
cir <- merge(cir_6, cir_5, cir_1, cir_12)
plot(cir, col=rev(terrain.colors(100)), main="CIR")

# Assuming the CIR bands are: Red = Band 1, NIR = Band 4
red_band <- cir[[2]]  # Red band
nir_band <- cir[[1]]  # NIR band

# Calculate NDVI
ndvi <- (nir_band - red_band) / (nir_band + red_band)
ndvi[is.nan(ndvi)] <- NA  # Remove NaN values
plot(ndvi, col=rev(terrain.colors(100)), main="NDVI")

# Apply a median filter to reduce NDVI noise
ndvi_filtered <- focal(ndvi, w = matrix(1, 3, 3), fun = median, na.policy = "omit")
plot(ndvi_filtered, col=rev(terrain.colors(100)), main="Smoothed NDVI")

# Apply threshold to NDVI
ndvi_mask <- ndvi_filtered > 0.1
plot(ndvi_mask, main="NDVI Mask (Trees)")


#-------------------------------------------------------------------------------

# Load the DSM rasters:
dsm_6 <- rast("D:/Wurzburg/DSM/32567_5516_20_DOM.tif")
dsm_5 <- rast("D:/Wurzburg/DSM/32567_5515_20_DOM.tif")
dsm_12 <- rast("D:/Wurzburg/DSM/32566_5516_20_DOM.tif")
dsm_1 <- rast("D:/Wurzburg/DSM/32566_5515_20_DOM.tif")
# Create the mosaic
dsm <- merge(dsm_6, dsm_5, dsm_1, dsm_12)

#-------------------------------------------------------------------------------

# Load the DSM rasters:
dtm_6 <- rast("D:/Wurzburg/DTM/566_5516.tif")
dtm_5 <- rast("D:/Wurzburg/DTM/566_5515.tif")
dtm_1 <- rast("D:/Wurzburg/DTM/567_5516.tif")
dtm_12 <- rast("D:/Wurzburg/DTM/567_5515.tif")
# Create the mosaic
dtm <- merge(dtm_6, dtm_5, dtm_12, dtm_1)

#-------------------------------------------------------------------------------
# Ensure DSM and DTM are aligned and have the same resolution
dtm_resampled <- resample(dtm, dsm)

# Calculate the Canopy Height Model (CHM)
ndsm <- dsm - dtm_resampled
ndsm[ndsm < 0] <- 0  # Remove negative values
plot(ndsm)

#-------------------------------------------------------------------------------

chm<-ndsm*ndvi_mask
plot(chm,  col=rev(terrain.colors(100)), main="CHM")


#test 1 #
# Apply the threshold
chm_threshold <- classify(chm, matrix(c(-Inf, 2, NA), ncol=3, byrow=TRUE))
# Plot the thresholded CHM
plot(chm_threshold, col=terrain.colors(100), main="CHM (Threshold > 2 m)")

#writeRaster(chm_threshold, "D:/Wurzburg/Segments/chm_test1.tif")


#test 2 #
# Smooth CHM using focal mean filter to remove small artifacts
chm_smoothed <- focal(chm, w = matrix(1, 3, 3), fun = mean, na.policy = "omit")
plot(chm_smoothed, col=terrain.colors(100), main="Smoothed CHM")

# Apply the threshold
chm_threshold <- classify(chm_smoothed, matrix(c(-Inf, 2, NA), ncol=3, byrow=TRUE))

# Plot the thresholded CHM
plot(chm_threshold, col=terrain.colors(100), main="CHM (Threshold > 2 m)")

#writeRaster(chm_threshold, "D:/Wurzburg/Segments/chm_test2.tif")


#test 3#
#We smooth the chm
kernel <- matrix(1,3,3)
chm_smoothed <- terra::focal(chm, w = kernel, fun = median, na.rm = TRUE)
# Apply the threshold
chm_threshold <- classify(chm_smoothed, matrix(c(-Inf, 2, NA), ncol=3, byrow=TRUE))
#writeRaster(chm_threshold, "D:/Wurzburg/Segments/chm_test3.tif")

#test 4#
#We smooth the chm
kernel <- matrix(1,3,3)
chm_smoothed <- terra::focal(chm, w = kernel, fun = median, na.rm = TRUE)
# Apply the threshold
chm_threshold <- classify(chm_smoothed, matrix(c(-Inf, 1.5, NA), ncol=3, byrow=TRUE))
#writeRaster(chm_threshold, "D:/Wurzburg/Segments/chm_test4.tif")

#------------------------------------------------------------------------------------------
# Define the VWF function
vwf_function <- function(chm_height) {
  a <- 2  # Minimum radius (in meters)
  b <- 0.5  # Scaling factor per meter of height
  return(a + b * chm_height)
}


tree_tops <- locate_trees(chm_threshold, lmf(ws = vwf_function,  hmin = 1, shape = "circular"))

# Apply Local Maxima Filtering (LMF) to detect tree tops
tree_tops <- locate_trees(chm_threshold, lmf(ws = 5, hmin = 1, shape = "circular"))
st_write(tree_tops, "D:/Wurzburg/Segments/Test_1/TreeTops_t5.gpkg")

# Plot CHM with detected tree tops
plot(chm_threshold, col = terrain.colors(100), main = "Tree Tops Detection")
points(tree_tops, col = "red", pch = 16)

# Segment trees using the watershed algorithm
silva <- silva2016(chm_threshold, tree_tops, max_cr_factor = 0.5, exclusion = 0.3)
silva_crowns<-silva()
plot(silva_crowns, col=pastel.colors(200))
#writeRaster(silva_crowns, "D:/Wurzburg/Segments/Test_1/silva_crowns_t3.tif")


# Test DalPonte - did not work 

#dalponte<-dalponte2016(chm_threshold, tree_tops, th_tree=1.5, th_seed = 0.2, th_cr = 0.25,)
#dalponte_crowns<-dalponte()
#plot(dalponte_crowns, col=pastel.colors(200))

#--------------------------------------------------------------------------------

#polygonize

silva_polygons <- as.polygons(silva_crowns, dissolve=TRUE)  # Convert raster to polygons
silva_polygons <- st_as_sf(silva_polygons)  # Convert to sf object for easier handling

# Step 2: Convert Multi-Part to Single-Part
silva_polygons <- st_cast(silva_polygons, "POLYGON")  # Split multi-part polygons into single parts

#st_write(silva_polygons, "D:/Wurzburg/Segments/Test_1/silva_polygons_t3.gpkg")
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
silva_filtered <- silva_polygons[silva_polygons$intersection_ratio <= 0.75, ]# Keep polygons with ≤75% overlap
silva_filtered <- silva_filtered[silva_filtered$area_total >= units::set_units(1, m^2), ]


plot(cir$`566_5515_1`, col=rev(terrain.colors(100)), main="RGB")
plot(silva_filtered, add=TRUE)


################################################################################
#                              Height
################################################################################

pcth <- function(x, p=0.90, na.rm = TRUE) { quantile(x, p, na.rm = na.rm) }
silva_filtered$height <- (terra::extract(chm_threshold, silva_filtered, fun=pcth))[,2]
test<-terra::extract(chm_threshold, silva_filtered, fun=pcth)

names(silva_filtered)

################################################################################
#                              Area and perimeter
################################################################################

silva_filtered$area <- st_area(silva_filtered)  # Automatically adds units (e.g., m^2)

# Calculate perimeter
silva_filtered$perimeter <- st_length(st_cast(silva_filtered, "MULTILINESTRING"))

################################################################################
#                              Diameter
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

st_write(silva_filtered, "D:/Wurzburg/Segments/wue_center_t2_full.shp")


################################################################################
#                           Accuracy assessment
################################################################################
#tree_tops<-st_read("D:/Wurzburg/Segments/Test_1/TreeTops_t4.gpkg")
silva_filtered<-st_read("D:/Wurzburg/Segments/wue_center_t1_full.shp")
cadastre<-st_read("D:/Wurzburg/cadastre_test.gpkg")

#-------------------------------------------------------------------------------
#Count trees in each dataset

count_polygons<- nrow(silva_filtered)
count_cadastre<- nrow(cadastre)

#-------------------------------------------------------------------------------
#Estimate nearest distance#
# Extract centroids of silva_crowns polygons
silva_centroids <- st_centroid(silva_filtered)

# Convert centroids to coordinates
silva_coords <- st_coordinates(silva_centroids)

# Convert coordinates to matrices for nn search
cadastre_coords <- st_coordinates(cadastre)

# Find the nearest cadastre point for each silva polygon
nn_index <- nn2(cadastre_coords, silva_coords, k = 1)  # k = 1 ensures only one match

# Extract the corresponding cadastre points
nearest_cadastre <- cadastre[nn_index$nn.idx, ]

# Add nearest cadastre ID and distance to silva_filtered
silva_filtered$nearest_cadastre_id <- nearest_cadastre$cadastre_id  # Assuming cadastre has an ID column
silva_filtered$nearest_distance <- nn_index$nn.dists  # Store distances

# Define match threshold (e.g., 2m)
threshold <- 2

# Identify one-to-one matches
cadastre$matched_tree_tops <- cadastre$nn_distance_tree_tops <= threshold

# Count matches
one_to_one_matches_tree_tops <- sum(cadastre$matched_tree_tops)

cat("One-to-one matches (tree_tops):", one_to_one_matches_tree_tops, "\n")

# Summary statistics
summary_stats <- summary(cadastre$nn_distance)

#--------------------------------------------------------------------------------
#Tree metrics

#I joined the cadastre and the rs crowns in qgis qith counting points and join by location only for the one-to-one matches
one_to_one<-st_read("D:/Wurzburg/Validation/one-to-one-mit-kat.gpkg")

ggplot(one_to_one, aes(x = height, y = as.numeric(baumhoehe))) +
  geom_point(color = "#2BC38A", size = 3, alpha = 0.7) +  # Customize point size and transparency
  geom_smooth(method = "lm", color = "#1F7A5C", fill = "#A7E6CE", se = TRUE, linetype = "solid") +
  labs(#title = "Remote sensing height vs. Cadastre height",
       x = "RS height (m)",
       y = "Cadastre height (m)") +
  theme_minimal(base_size = 14) +  # Clean theme with larger text
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))  # Centered and bold title


ggplot(one_to_one, aes(x = diam, y = as.numeric(kronenbrei))) +
  geom_point(color = "#00658B", size = 3, alpha = 0.7) +  # Customize point size and transparency
  geom_smooth(method = "lm", color = "#1F7A5C", fill = "#A7E6CE", se = TRUE, linetype = "solid") +
  labs(#title = "Remote sensing diameter vs. Cadastre diameter",
       x = "RS diameter (m)",
       y = "Cadastre diameter (m)") +
  theme_minimal(base_size = 14) +  # Clean theme with larger text
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))  # Centered and bold title

#--------------------------------------------------------------------------------
# Intersection over union
buffer<-st_read("D:/Wurzburg/Validation/kat_buffered.gpkg")

buffer$area_kat<-st_area(buffer)

# Compute intersection areas
intersections <- st_intersection(silva_filtered, buffer) %>%
  mutate(intersection_area = st_area(.))

intersections$union_areas<- as.numeric(intersections$area)+as.numeric(intersections$area_kat)-as.numeric(intersections$intersection_area)

intersections$iou_ttl<- intersections$intersection_area/intersections$union_areas

intersections$kat_percent<-100*intersections$intersection_area / intersections$area_kat

intersections$rs_percent<-100*intersections$intersection_area / intersections$area


st_write(intersections, "D:/Wurzburg/Validation/intersections.shp")
#-------------------------------------------------------------------------------



