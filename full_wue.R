install.packages("BiocManager")
BiocManager::install("EBImage")
install.packages("devtools")

#Install packages
library(car)
library(terra)
library(RStoolbox)
library(raster)
library(rgeos)
library(sf)
library(lidR)
library(EBImage)
library(caTools)
library(remotes)
library(devtools)
install_github("bleutner/RStoolbox")

install.packages("velox")
remotes::install_github("bleutner/RStoolbox")
############# 1. The first step is doing the pansharpening

############# 2. Then we make a mosaic from the rasters 

# Load the two rasters
raster1 <- rast("D:/Preprocessed/Pan-sharpened/QGIS/444898/pan-20JUL23101818-M2AS_R1C1-22EUSI-1785-02-1.tif")
raster2 <- rast("D:/Preprocessed/Pan-sharpened/QGIS/444898/pan-20JUL23101818-M2AS_R2C1-22EUSI-1785-02-1.tif")

# mosaic the two rasters
mosaic <- mosaic(raster1, raster2, fun=max)

#plot the mosaic
plot(mosaic)

# write the mosaic to a new file
writeRaster(mosaic, "D:/Preprocessed/Test_wue/mosaic_20JUL_WU_1.tif", overwrite=TRUE)

#--------------------------------------------------------------------------------

# Load the two rasters
raster3 <- rast("D:/Preprocessed/Pan-sharpened/QGIS/444918/pan-20JUL04101824-M2AS_R1C1-22EUSI-1785-02-2.tif")
raster4 <- rast("D:/Preprocessed/Pan-sharpened/QGIS/444918/pan-20JUL04101824-M2AS_R2C1-22EUSI-1785-02-2.tif")

# mosaic the two rasters
mosaic_0922 <- mosaic(raster3, raster4, fun=max)

#plot the mosaic
plot(mosaic_0922)

# write the mosaic to a new file
writeRaster(mosaic_0922, "D:/Preprocessed/Test_wue/mosaic_20JUL_WU_2.tif", overwrite=TRUE)

############# 3. Then we will crop all the mosaics to match the extent of the image from July, 2019

#Load all the rasters

wu_j20_1<- rast("D:/Preprocessed/Test_wue/mosaic_20JUL_WU_1.tif")
wu_j20_2<- rast("D:/Preprocessed/Test_wue/mosaic_20JUL_WU_2.tif")

cropped_wu_j20_2 <- crop(wu_j20_2, ext(wu_j20_1))

plot(cropped_wu_j20_2)

writeRaster(cropped_wu_j20_2, "D:/Preprocessed/Test_wue/cropped_wu_j20_2.tif", overwrite=TRUE)

############# 4. The next step is doing the coregistration in QGIS

############# 5. Estimate Vegetation Indices
#Load co-registered raster
mr_s21<- rast("D:/Preprocessed/Test_wue/cor_21SEP_MR.tif")
#mr_s22<- rast("D:/Preprocessed/Test_wue/cor_22SEP_MR.tif") #the reference and target images did not have enough tie points 
mr_j19<- rast("D:/Preprocessed/Clipped/clip-19JUL-WV2-middle.tif")
mr_j19<-resample(mr_j19, mr_s21)


#Create the functions for the indices
#NDVI
ndvi<-function(s){
  nir=subset(s, 7) #pay attention to the band order in WV2
  red=subset(s, 5) 
  
  (nir-red)/(nir+red)
}

#apply the ndvi
ndvi_wu_j20_1<- ndvi(mr_j19)
ndvi_mrs21<- ndvi(mr_s21)
#ndvi_mrs22<- ndvi(mr_s22)

#--------------------------------------------------------------------------------

#NDRE: Normalized Difference Red Edge Index
ndre<- function(s){
  rededge=subset(s, 6)
  red=subset(s, 5)
  
  
  (rededge-red)/(rededge+red)
}

#apply the evi
ndre_wu_j20_1<- ndre(mr_j19)
ndre_mrs21<- ndre(mr_s21)
#ndre_mrs22<- ndre(mr_s22)

#--------------------------------------------------------------------------------

#RVI: Ratio Vegetation Index
rvi<- function(s){
  nir=subset(s, 7) #pay attention to the band order in WV2
  red=subset(s, 5)
  
  
  red/nir
}

#apply the evi
RVI_wu_j20_1<- rvi(mr_j19)
RVI_mrs21<- rvi(mr_s21)

############# 5.5. Correct errors in the indices

ndvi_mrs21[ndvi_mrs21 > 1]<- NA
ndvi_mrs21[ndvi_mrs21 < -1]<- NA

ndre_mrs21[ndre_mrs21 > 1]<- NA
ndre_mrs21[ndre_mrs21 < -1]<- NA
############# 6. Load and preprocess the other covariates (CHM and LU)

chm<-rast("D:/nDSM_Muc.tif")
chm<-resample(chm, ndvi_mrs21)
plot(chm)

lu_raster<-ndvi_mrs21
lu<-st_read("C:/Users/ang58gl/Documents/Data/LU_dissolved.shp")
lu_raster<-rasterize(x=lu, y=lu_raster, field="Cat_pat")
lu_raster
plot(lu_raster)

############# 7. Prepare the ndvi filter
#Apply vegetation and buildings filter
j19_ndvi_filter<-ndvi_wu_j20_1>0.3
s21_ndvi_filter<-ndvi_mrs21>0.3

#make a filter using pixels that are above the threshold in any date 
ndvi_filter<-(j19_ndvi_filter+s21_ndvi_filter)>0


############# 8. Prepare layers for modelling

#resample
#chm<-resample(chm, ndvi_wu_j20_1)
#lu_raster<-resample(lu_raster, ndvi_wu_j20_1)
#RVI_mrs21<-resample(RVI_mrs21, ndvi_wu_j20_1)
#ndre_mrs21<-resample(ndre_mrs21, ndvi_wu_j20_1)
#ndvi_mrs21<-resample(ndvi_mrs21, ndvi_wu_j20_1)
#mr_s21<-resample(mr_s21, ndvi_wu_j20_1)


#Change extent
ext(mr_s21)<-ext(ndvi_wu_j20_1)
ext(mr_j19)<-ext(ndvi_wu_j20_1)
ext(RVI_wu_j20_1)<-ext(ndvi_wu_j20_1)
ext(ndre_mrs21)<-ext(ndvi_wu_j20_1)
ext(ndre_wu_j20_1)<-ext(ndvi_wu_j20_1)
ext(RVI_mrs21)<-ext(ndvi_wu_j20_1)
ext(ndvi_mrs21)<-ext(ndvi_wu_j20_1)
ext(chm)<-ext(ndvi_wu_j20_1)
ext(lu_raster)<-ext(ndvi_wu_j20_1)

#compare geometries
compareGeom(lu_raster, ndvi_wu_j20_1)


chm1<-chm*ndvi_filter
lu_raster1<-lu_raster*ndvi_filter
mr_j191<-mr_j19*ndvi_filter
mr_s211<-mr_s21*ndvi_filter

#create raster stack
input_layers_trees<-rast(list(RVI_wu_j20_1, RVI_mrs21, ndvi_wu_j20_1, ndvi_mrs21, ndre_wu_j20_1, ndre_mrs21, chm1, lu_raster1, mr_j191, mr_s211))
writeRaster(input_layers, "D:/Preprocessed/Test_wue/covariates.tif")

#check the raster stack
plot(input_layers)
summary(input_layers)

#Filter the images to classify vegetation
input_layers_masked<-mask(input_layers,ndvi_filter)

#input_layers_masked<-rast("D:/Preprocessed/Test_wue/covariates_masked.tif")
writeRaster(input_layers_masked, "D:/Preprocessed/Test_wue/covariates_masked.tif")
plot(input_layers_trees)

input_layers_masked<-rast("D:/Preprocessed/Test_wue/covariates_masked.tif")
############# 9. Prepare training data
#It comes from Training.R
#We select the columns that are interesting for us
trees_sinbg <- fin2[c('coords.x','coords.y', 'trees_bg.species')]
#change their names
names(trees_sinbg) <- c("x", "y", "species" )

#Transform into spatial objects by assigning coordinates and projection
coordinates(trees_sinbg)<-~x+y
crs(trees_sinbg)<- crs(input_layers_masked)

#--------------------------------------------------------------------------------

#It comes from Training.R
#We select the columns that are interesting for us
trees_bg <- fin[c('coords.x','coords.y', 'trees_bg.species')]
#change their names
names(trees_bg) <- c("x", "y", "species" )

#Transform into spatial objects by assigning coordinates and projection
coordinates(trees_bg)<-~x+y
crs(trees_bg)<- crs(input_layers_masked)


#Extract 

trees_checked<-data.frame(coordinates(trees_sinbg),
                          trees_sinbg$species,
                          extract(input_layers_masked, vect(trees_sinbg)))

plot(trees_checked)
summary(trees_checked)


############# 10. Run pixel-based classifier

#set directory
setwd("D:/Modeling")

#make it reproducible
set.seed(23)
#--------------------------------------------------------------------------------
#Accuracy:0.05
#Fit classifier test 5. Applying filters and using the background points

sc_v5_bg<-superClass(input_layers_trees, 
                     trainData=trees_bg,
                     trainPartition = 0.7,
                     responseCol= "species",
                     filename= "MRgenus_class_sc_v5_bg.tif",
)

plot(sc_v5_bg$map)

sc_v5_bg$validation$performance

#--------------------------------------------------------------------------------
#Accuracy:0,3169
#Fit classifier test 4. Applying filters and not using the background points, but KNN
sc_v4_sinbg<-superClass(input_layers_trees, 
                        trainData=trees_sinbg,
                        trainPartition = 0.7,
                        responseCol= "species",
                        model="knn",
                        filename= "MRgenus_class_sc_v4_sinbg.tif",
)

plot(sc_v4_sinbg$map)

sc_v4_sinbg$validation$performance

#--------------------------------------------------------------------------------

#Accuracy: 0,4483
#Fit classifier test 3. Applying filters and not using the background points IT DID NOT WORK, so I changed the NA to 0
sc_v3_sinbg<-superClass(input_layers_trees, 
                        trainData=trees_sinbg,
                        trainPartition = 0.7,
                        responseCol= "species",
                        filename= "MRgenus_class_sc_v3_sinbg.tif"
)

plot(sc_v3_sinbg$map)

sc_v3_sinbg$validation$performance

#--------------------------------------------------------------------------------
#0.4147 of accuracy
#Fit classifier test 2. Not applying filters and not using the background points
sc_v2_sinbg<-superClass(input_layers, 
                        trainData=trees_sinbg,
                        trainPartition = 0.7,
                        responseCol= "species",
                        filename= "MRgenus_class_sc_v2_sinbg.tif"
)

plot(sc_v2_sinbg$map)

sc_v2_sinbg$validation$performance #0.4147 of accuracy

#--------------------------------------------------------------------------------

input_layers_raster<-stack(input_layers_masked)
env<- extract(input_layers_raster, trees_sinbg)
pa_data<-data.frame(env, occurrence=trees_sinbg$species)

boxplot(subset(data.frame(pa_data), occurrence==1, selectc(1:20)))
boxplot(subset(data.frame(pa_data), occurrence==2, selectc(1:20)))
boxplot(subset(data.frame(pa_data), occurrence==3, selectc(1:20)))

scatterplotMatrix(~.| occurrence, data = pa_data, smoother = FALSE, reg.line = FALSE)
panel.cor <- function (x, y, digits =9, cor_thresh=0.7, col=c("black", "yellow", "red", "green", "blue", "orange", "pink", "purple", "darkgreen"), ...) {
  usr<-par("usr")
  on.exit(par(usr))
  par(usr = c(0,1,0,1))
  r<-abs(cor(x,y))
  txt<-format(c(r, 0.123456789), digits = digits)[1]
  text(0.5, 0.5, txt, xol = ifelse(r< cor_thresh, col[1], col[2]))
}

scatterplotMatrix(~.| occurrence, data = pa_data, smoother = FALSE, reg.line = FALSE, upper.panel = panel.cor)


#################################################################################
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------
#Now we can start with the object-based classification
#We will use the same images from the pixel-based
#--------------------------------------------------------------------------------
#--------------------------------------------------------------------------------


############# 1. The first step is the tree segmentation. 

#We load and prepare the data 
input_layers<-rast("D:/Preprocessed/Test_wue/covariates.tif")
tree_filter<-raster("D:/Classifications/2.2_trees_MR_nobuildings.tif")

plot(tree_filter)

tree_filter<-resample(tree_filter, input_layers)

#Filter the images to classify vegetation
input_layers_masked<-mask(input_layers, tree_filter)


writeRaster(covariates_smoothed,"D:/Segments/lidR_data/smoothed_cov.tif")
writeRaster(input_layers_masked, "D:/Segments/lidR_data/cov_masked.tif")


# We will use the lidR package for that   

kernel <- matrix(1,3,3)

covariates_smoothed <- terra::focal(input_layers_masked, w = kernel, fun = median, na.rm = TRUE)


#We reload the data manually because locate_trees is not working

#-------------------------------------------------------------------------------
chm <- raster("D:/nDSM_Muc.tif")
chm <- resample(chm, tree_filter)
chm <- mask(chm, tree_filter)
chm <- readAll(chm)
#writeRaster(chm, "D:/Preprocessed/chm.tif")
#Detect trees

ttops_covariates<-locate_trees(chm, lmf(ws=7, hmin=3, shape="circular"))

#Plot the trees 
par(mfrow=c(1,2))
col<- height.colors(50)
plot(chm, main= "Covariates", col=col); plot(sf::st_geometry(ttops_covariates), add=T, pch=3)
#plot(covariates_smoothed, main= "Covariates smoothed", col=col); plot(sf::st_geometry(ttops_covariates_smoothed), add=T, pch=3)

#segment trees
algo<-dalponte2016(chm, ttops_covariates,   th_tree = 2,
                   th_seed = 0.45,
                   th_cr = 0.55,
                   max_cr = 15)
crowns<-algo()

plot(crowns, col=pastel.colors(200))

writeRaster(crowns, "D:/Segments/lidR_data/crowns_3.tif")

#---------------------------------------------------------------

crowns_waters<-watershed(chm)()
writeRaster(crowns, "D:/Segments/lidR_data/crown_watershed_1.tif")

#---------------------------------------------------------------

silva<-silva2016(chm, ttops_covariates, max_cr_factor = 0.5, exclusion = 0.3)
silva_crowns<-silva()

plot(silva_crowns, col=pastel.colors(200))
writeRaster(silva_crowns, "D:/Segments/lidR_data/silva_crowns_3.tif")

write.csv(ttops_covariates, "D:/Segments/lidR_data/treetops.csv")


# We smooth the polygons in QGIS and filter the ones with areas smaller than 2 sqm

############# 2. We extract zonal statistics from our covariates


