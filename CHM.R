library(lidR)
#Read the point cloud and name it "las"
las <- readLAS("D:/Ancillary_data_Wue/565_5516.laz")

#Establish the epsg of the las
epsg(las) <- 25832

#check the las
head(las)
las_check(las)

#plot the pints in 3D
plot(las, color = "Classification", size = 3, bg = "white")

#Estimate the DTM with Triangular irregular network

dtm_tin <- rasterize_terrain(las, res=0.1 , algorithm = tin())

#Plot it
plot_dtm3d(dtm_tin, bg = "white") 

nlas <- las - dtm_tin
plot(nlas, size = 4, bg = "white")

chm_raster <- raster(chm)