library(lidR)
library(raster)
#Read the point cloud and name it "las"
las <- readLAS("test/565_5516.laz")

#Establish the epsg of the las
epsg(las) <- 25832

#check the las
head(las)
las_check(las)

#plot the pints in 3D
plot(las, color = "Classification", size = 3, bg = "white")

#Estimate the DTM with Triangular irregular network

dtm_tin <- rasterize_terrain(las, res=1 , algorithm = tin())

#Plot it
plot_dtm3d(dtm_tin, bg = "white") 

nlas <- las - dtm_tin
plot(nlas, size = 4, bg = "white")

chm <- rasterize_canopy(nlas, res = 1, algorithm = dsmtin())
plot(chm)


#------------------------------------------------------------------------------------------


# Create an empty list to store CHM rasters
chm_list <- list()

# Directory containing LAS files
las_dir <- "D:/Ancillary_data_Wue/aria2/output"

# List all LAS files in the directory
las_files <- list.files(las_dir, pattern = "\\.laz$", full.names = TRUE)

# Loop through LAS files
for (las_file in las_files) {
  # Read the LAS file
  las <- readLAS(las_file)
  
  # Set the EPSG (if it's the same for all files)
  epsg(las) <- 25832
  
  # Estimate DTM with Triangular Irregular Network
  dtm_tin <- rasterize_terrain(las, res = 1, algorithm = tin())
  
  # Subtract DTM from LAS
  nlas <- las - dtm_tin
  
  # Estimate Canopy Height Model (CHM)
  chm <- rasterize_canopy(nlas, res = 1, algorithm = dsmtin())
  
  # Append the CHM to the list
  chm_list[[length(chm_list) + 1]] <- chm
}

# Combine all CHMs into one using mosaic
chm_combined <- do.call(mosaic, chm_list)

# Plot the final CHM
plot(chm_combined, main = "Merged Canopy Height Model")
plot(aoi, add=TRUE)

aoi_2<-st_transform(aoi, crs = crs(chm_combined))

# Save the final CHM as a raster file if needed
writeRaster(chm_combined, "D:/Ancillary_data_Wue/merged_chm.tif")
