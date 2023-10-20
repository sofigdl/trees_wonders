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
library(randomForest)
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

#crop raster to match the extent
cropped_wu_j20_2 <- crop(wu_j20_2, ext(wu_j20_1))

plot(cropped_wu_j20_2)

writeRaster(cropped_wu_j20_2, "D:/Preprocessed/Test_wue/cropped_wu_j20_2.tif", overwrite=TRUE)

############# 4. The next step is doing the coregistration in QGIS

############# 5. Estimate Vegetation Indices
#Load co-registered raster
wu_j20_1<- rast("D:/Preprocessed/Test_wue/mosaic_20JUL_WU_1.tif")

wu_j20_2<- rast("D:/Preprocessed/Test_wue/cor_wu_j20_2.tif")

wu_j20_1<-resample(wu_j20_1, wu_j20_2)


#Create the functions for the indices
#NDVI
ndvi<-function(s){
  nir=subset(s, 7) #pay attention to the band order in WV2
  red=subset(s, 5) 
  
  (nir-red)/(nir+red)
}

#apply the ndvi
ndvi_wu_j20_1<- ndvi(wu_j20_1)
ndvi_wu_j20_2<- ndvi(wu_j20_2)


#--------------------------------------------------------------------------------

#NDRE: Normalized Difference Red Edge Index
ndre<- function(s){
  rededge=subset(s, 6)
  red=subset(s, 5)
  
  
  (rededge-red)/(rededge+red)
}

#apply the evi
ndre_wu_j20_1<- ndre(wu_j20_1)
ndre_wu_j20_2<- ndre(wu_j20_2)
#ndre_mrs22<- ndre(mr_s22)

#--------------------------------------------------------------------------------

#RVI: Ratio Vegetation Index
rvi<- function(s){
  nir=subset(s, 7) #pay attention to the band order in WV2
  red=subset(s, 5)
  
  
  red/nir
}

#apply the evi
RVI_wu_j20_1<- rvi(wu_j20_1)
RVI_wu_j20_2<- rvi(wu_j20_2)

############# 5.5. Correct errors in the indices

ndvi_wu_j20_2[ndvi_wu_j20_2 > 1]<- NA
ndvi_wu_j20_2[ndvi_wu_j20_2 < -1]<- NA

ndre_wu_j20_2[ndre_wu_j20_2 > 1]<- NA
ndre_wu_j20_2[ndre_wu_j20_2 < -1]<- NA
############# 6. Load and preprocess the other covariates (CHM and LU)

chm<-rast("D:/Ancillary_data_Wue/merged_chm.tif")
#crop raster to match the extent
chm<-project(chm, crs(wu_j20_1))

chm <- crop(chm, ext(wu_j20_1))

chm<-resample(chm, wu_j20_1)
plot(chm)

lu_raster<-ndvi_wu_j20_2
lu<-st_read("D:/Preprocessed/Test_wue/LU_dissolved.gpkg")
lu<-st_transform(lu, crs = crs(chm))
lu_raster<-rasterize(x=lu, y=lu_raster, field="Cat_pat")

lu_raster

plot(lu_raster)

############# 7. Prepare the ndvi filter for the vegetation classification

#Apply vegetation and buildings filter
j20_1_ndvi_filter<-ndvi_wu_j20_1>0.3
j20_2_ndvi_filter<-ndvi_wu_j20_2>0.3

#make a filter using pixels that are above the threshold in any date 
ndvi_filter<-(j20_1_ndvi_filter+j20_2_ndvi_filter)>0
ndvi_filter <- crop(ndvi_filter, ext(wu_j20_1))
plot(ndvi_filter)

#Write rasters
writeRaster(ndvi_filter, "D:/Classifications/Wue/1_vegetation_MR.tif", overwrite=TRUE)

############################### Trees classification ###########################
#height threshold for the canopy height model
#load the vegetation layer from last step
ndvi_filter<-rast("D:/Classifications/Wue/1_vegetation_MR.tif")

#Apply a threshold
chm_filter<- chm > 1.3

plot(chm_filter)

trees<-ndvi_filter*chm_filter
plot(trees)

#load the symmetrical difference of the buildings for 
symdif_buildings<-st_read("D:/Ancillary_data_Wue/symdif_wue.gpkg")
plot(symdif_buildings, add=TRUE)
symdif_buildings<-st_transform(symdif_buildings, crs(wu_j20_1))

#crop out the built-up areas
trees<-crop(trees, symdif_buildings)
trees<-terra::mask(trees, symdif_buildings)
plot(trees)

##Write rasters
writeRaster(trees, "D:/Classifications/Wue/2.2_trees_Wue_nb.tif", overwrite=TRUE)


########################### Classify the patterns ##############################


trees_lu<-raster("D:/Classifications/2.2_trees_MR_nobuildings.tif")


# Rasterize the Land Use classes

lu_raster<-rasterize(x=lu, y=trees_lu, field="Cat_pat")

plot(lu_raster)


trees_lu_class<-lu_raster*trees_lu
plot(trees_lu_class)




############# 8. Prepare layers for modelling

#resample
#chm<-resample(chm, ndvi_wu_j20_1)
#lu_raster<-resample(lu_raster, ndvi_wu_j20_1)
#RVI_wu_j20_2<-resample(RVI_wu_j20_2, ndvi_wu_j20_1)
#ndre_wu_j20_2<-resample(ndre_wu_j20_2, ndvi_wu_j20_1)
#ndvi_wu_j20_2<-resample(ndvi_wu_j20_2, ndvi_wu_j20_1)
#wu_j20_2<-resample(wu_j20_2, ndvi_wu_j20_1)


#Change extent
ext(wu_j20_2)<-ext(ndvi_wu_j20_1)
ext(wu_j20_1)<-ext(ndvi_wu_j20_1)
ext(RVI_wu_j20_1)<-ext(ndvi_wu_j20_1)
ext(ndre_wu_j20_2)<-ext(ndvi_wu_j20_1)
ext(ndre_wu_j20_1)<-ext(ndvi_wu_j20_1)
ext(RVI_wu_j20_2)<-ext(ndvi_wu_j20_1)
ext(ndvi_wu_j20_2)<-ext(ndvi_wu_j20_1)
ext(chm)<-ext(ndvi_wu_j20_1)
ext(lu_raster)<-ext(ndvi_wu_j20_1)

#compare geometries
compareGeom(wu_j20_2, ndvi_wu_j20_1)


chm1<-chm*ndvi_filter
lu_raster1<-lu_raster*ndvi_filter
wu_j20_11<-wu_j20_1*ndvi_filter
wu_j20_21<-wu_j20_2*ndvi_filter

#create raster stack
input_layers_trees<-rast(list( wu_j20_11, wu_j20_21, ndvi_wu_j20_1, ndvi_wu_j20_2, chm1, lu_raster1))
writeRaster(input_layers_trees, "D:/Preprocessed/Test_wue/covariates.tif")


############# 9. Prepare training data
#It comes from Training.R
#We select the columns that are interesting for us
#trees_sinbg <- fin2[c('coords.x','coords.y', 'trees_bg.species')]
#change their names
names(trees_sinbg) <- c("coords.x", "coords.y", "species" )

#Transform into spatial objects by assigning coordinates and projection
coordinates(trees_sinbg)<-~x+y
crs(trees_sinbg)<- crs(input_layers_masked)


######################################################################################################################################################################################
######################################################################################################################################################################################
######################################################################################################################################################################################

#Load vector
TBK_trees<-st_read("D:/Preprocessed/Test_wue/trees-sampled.gpkg")

TBK_trees$coords.x<-st_coordinates(TBK_trees)[,1]
TBK_trees$coords.y<-st_coordinates(TBK_trees)[,2]

head(TBK_trees)

TBK_trees <- TBK_trees %>%
  mutate(gattung = sub("^(\\w+).*", "\\1", baumart_la))


TBK_trees<-as.data.frame((TBK_trees))
  
# Bands to subset
bands_to_subset <- c("source_id", "coords.x", "coords.y", "baumart", "gattung", "kronenbrei", "baumhoehe", "CO_1", "CO_2", "CO_3", "CO_4", "CO_5", "CO_6", "CO_7", "CO_8", "CO_9", "CO_10",
                     "CO_11", "CO_12", "CO_13", "CO_14", "CO_15", "CO_16", "CO_17", "CO_18", "CO_19", "CO_20", "geom")

#Subset the raster bands
TBK_trees <- TBK_trees[ , bands_to_subset]


names(TBK_trees)<-c("source_id", "coords.x", "coords.y", "baumart", "gattung", "kronenbrei", "baumhoehe",   
                    "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8",
                    "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", 
                    "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm", "lu_raster", "geom")



################################################################################

# Column name for frequency ordering
column_name <- "gattung"

# Extract unique values and their frequencies
value_counts <- as.data.frame(table(TBK_trees[[column_name]]))

# Rename the columns
colnames(value_counts) <- c("Value", "Frequency")

# Sort the data frame by Frequency column
sorted_value_counts <- value_counts[order(value_counts$Frequency), ]

# Print the sorted value counts table
print(sorted_value_counts)


################################################################################

#Load raster
covariates<-stack("D:/Preprocessed/Test_wue/covariates.tif")

# Bands to subset
bands_to_subset <- c( "pan.20JUL23101818.M2AS_R1C1.22EUSI.1785.02.1_1.1", "pan.20JUL23101818.M2AS_R1C1.22EUSI.1785.02.1_2.1", "pan.20JUL23101818.M2AS_R1C1.22EUSI.1785.02.1_3.1",
                      "pan.20JUL23101818.M2AS_R1C1.22EUSI.1785.02.1_4.1", "pan.20JUL23101818.M2AS_R1C1.22EUSI.1785.02.1_5.1", "pan.20JUL23101818.M2AS_R1C1.22EUSI.1785.02.1_6.1",
                      "pan.20JUL23101818.M2AS_R1C1.22EUSI.1785.02.1_7.1", "pan.20JUL23101818.M2AS_R1C1.22EUSI.1785.02.1_8.1", "pan.20JUL04101824.M2AS_R1C1.22EUSI.1785.02.2_1.1",
                      "pan.20JUL04101824.M2AS_R1C1.22EUSI.1785.02.2_2.1", "pan.20JUL04101824.M2AS_R1C1.22EUSI.1785.02.2_3.1", "pan.20JUL04101824.M2AS_R1C1.22EUSI.1785.02.2_4.1",
                      "pan.20JUL04101824.M2AS_R1C1.22EUSI.1785.02.2_5.1", "pan.20JUL04101824.M2AS_R1C1.22EUSI.1785.02.2_6.1", "pan.20JUL04101824.M2AS_R1C1.22EUSI.1785.02.2_7.1",
                      "pan.20JUL04101824.M2AS_R1C1.22EUSI.1785.02.2_8.1", "pan.20JUL23101818.M2AS_R1C1.22EUSI.1785.02.1_7.2", "pan.20JUL04101824.M2AS_R1C1.22EUSI.1785.02.2_7.2",
                      "Z", "Cat_pat"  )

# Subset the raster bands
subset_raster <- subset(covariates, bands_to_subset)

subset_raster <- projectRaster(subset_raster, crs= 32632)

names(subset_raster)<-c("wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8",
"wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", 
"ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm", "lu_raster")

covariates2<-subset_raster

#########################################################################################

#Prepare the first two classes
TBK_trees_2<-TBK_trees
TBK_trees_2$genus<-ifelse(TBK_trees$gattung == "Acer", 1,
                            ifelse(TBK_trees$gattung == "Quercus", 2, 3))



# Divide the trees according to the land use
TBK_trees_streets_2<-subset(TBK_trees_2, lu_raster == 2)
TBK_trees_parks_2<-subset(TBK_trees_2, lu_raster == 1)
TBK_trees_resi_2<-subset(TBK_trees_2, lu_raster == 3)
TBK_trees_otro_2<-subset(TBK_trees_2, lu_raster == 4)

# Streets ---------------------------------------------------------------------------------------------------------------------------

#Transform it to data frame
trees_2_st <-as.data.frame(TBK_trees_streets_2)[c("coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8",
"wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", 
"ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_2_st$class<-factor(trees_2_st$genus)


#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_2_st<-na.omit(trees_2_st)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_2_st$class, SplitRatio=0.7)

#Create the new data sets
train_2_st<-subset(trees_clean_2_st, sample==TRUE)
test_2_st<-subset(trees_clean_2_st, sample==FALSE)


#Prepare model

rfmodel_2_st <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                               wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                               wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_2_st, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_2_st

#varImpPlot(rfmodel_2_st, scale = FALSE)#, type=1, scale=TRUE)

#Prediction
pred_rf_2_st<-predict(covariates2, rfmodel_2_st, 
                      filename = "D:/Classifications/Wue/RF/rf_2_st_v1.tif",
                      format ="GTiff",
                      overwrite=TRUE)



#subset the 
species_2_st<-test_2_st[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_2_st <- st_as_sf(species_2_st, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_2_st) <- st_crs(crs(covariates2))



#extract predictions
pred_2_st <- as.factor(extract(pred_rf_2_st, species_2_st))

obs_2_st <- as.factor(species_2_st$genus)


cm_2_st<-confusionMatrix(pred_2_st, reference = obs_2_st)

cm_2_st
cm_2_st$byClass



# Woodlands ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_2_prk <-as.data.frame(TBK_trees_parks_2)[c("coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_2_prk$class<-factor(trees_2_prk$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_2_prk<-na.omit(trees_2_prk)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_2_prk$class, SplitRatio=0.7)

#Create the new data sets
train_2_prk<-subset(trees_clean_2_prk, sample==TRUE)
test_2_prk<-subset(trees_clean_2_prk, sample==FALSE)

#Check the datasets
table(train_2_prk$genus)
table(test_2_prk$genus)

#Prepare model

rfmodel_2_prk <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_2_prk, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_2_prk


#Prediction
pred_rf_2_prk<-predict(covariates2, rfmodel_2_prk, 
                       filename = "D:/Classifications/Wue/RF/rf_2_prk_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_2_prk<-test_2_prk[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_2_prk <- st_as_sf(species_2_prk, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_2_prk) <- st_crs(crs(covariates2))


#extract predictions
pred_2_prk <- as.factor(extract(pred_rf_2_prk, species_2_prk))

obs_2_prk <- as.factor(species_2_prk$genus)


cm_2_prk<-confusionMatrix(pred_2_prk, reference = obs_2_prk)

cm_2_prk
cm_2_prk$byClass

# Residential ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_2_resi <-as.data.frame(TBK_trees_resi_2)[c("coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_2_resi$class<-factor(trees_2_resi$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_2_resi<-na.omit(trees_2_resi)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_2_resi$class, SplitRatio=0.7)

#Create the new data sets
train_2_resi<-subset(trees_clean_2_resi, sample==TRUE)
test_2_resi<-subset(trees_clean_2_resi, sample==FALSE)

#Check the datasets
table(train_2_resi$genus)
table(test_2_resi$genus)

#Prepare model

rfmodel_2_resi <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                 wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                 wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_2_resi, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_2_resi


#Prediction
pred_rf_2_resi<-predict(covariates2, rfmodel_2_resi, 
                        filename = "D:/Classifications/Wue/RF/rf_2_resi_v1.tif",
                        format ="GTiff",
                        overwrite=TRUE)



species_2_resi<-test_2_resi[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_2_resi <- st_as_sf(species_2_resi, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_2_resi) <- st_crs(crs(covariates2))

#extract predictions
pred_2_resi <- as.factor(extract(pred_rf_2_resi, species_2_resi))

obs_2_resi <- as.factor(species_2_resi$genus)


cm_2_resi<-confusionMatrix(pred_2_resi, reference = obs_2_resi)

cm_2_resi
cm_2_resi$byClass

#Other ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_2_oth <-as.data.frame(TBK_trees_otro_2)[c("coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_2_oth$class<-factor(trees_2_oth$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_2_oth<-na.omit(trees_2_oth)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_2_oth$class, SplitRatio=0.7)

#Create the new data sets
train_2_oth<-subset(trees_clean_2_oth, sample==TRUE)
test_2_oth<-subset(trees_clean_2_oth, sample==FALSE)

#Check the datasets
table(train_2_oth$genus)
table(test_2_oth$genus)

#Prepare model

rfmodel_2_oth <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_2_oth, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_2_oth


#Prediction
pred_rf_2_oth<-predict(covariates2, rfmodel_2_oth, 
                       filename = "D:/Classifications/Wue/RF/rf_2_oth_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_2_oth<-test_2_oth[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_2_oth <- st_as_sf(species_2_oth, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_2_oth) <- st_crs(crs(covariates2))


#extract predictions
pred_2_oth <- as.factor(extract(pred_rf_2_oth, species_2_oth))

obs_2_oth <- as.factor(species_2_oth$genus)


cm_2_oth<-confusionMatrix(pred_2_oth, reference = obs_2_oth)

cm_2_oth
cm_2_oth$byClass

######################################################################################

#Prepare the first three classes
TBK_trees_3<-TBK_trees
TBK_trees_3$genus<-ifelse(TBK_trees$gattung == "Acer", 1,
                            ifelse(TBK_trees$gattung == "Quercus", 2,
                                   ifelse(TBK_trees$gattung == "Carpinus", 3 , 4)))



# Divide the trees according to the land use
TBK_trees_streets_3<-subset(TBK_trees_3, lu_raster == 2)
TBK_trees_parks_3<-subset(TBK_trees_3, lu_raster == 1)
TBK_trees_resi_3<-subset(TBK_trees_3, lu_raster == 3)
TBK_trees_otro_3<-subset(TBK_trees_3, lu_raster == 4)

# Streets ---------------------------------------------------------------------------------------------------------------------------

#Transform it to data frame
trees_3_st <-as.data.frame(TBK_trees_streets_3)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_3_st$class<-factor(trees_3_st$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_3_st<-na.omit(trees_3_st)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_3_st$class, SplitRatio=0.7)

#Create the new data sets
train_3_st<-subset(trees_clean_3_st, sample==TRUE)
test_3_st<-subset(trees_clean_3_st, sample==FALSE)

#Check the datasets
table(train_3_st$genus)
table(test_3_st$genus)

#Prepare model

rfmodel_3_st <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                               wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                               wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_3_st, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_3_st

#varImpPlot(rfmodel_3_st, scale = FALSE)#, type=1, scale=TRUE)

#Prediction
pred_rf_3_st<-predict(covariates2, rfmodel_3_st, 
                      filename = "D:/Classifications/Wue/RF/rf_3_st_v1.tif",
                      format ="GTiff",
                      overwrite=TRUE)



species_3_st<-test_3_st[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_3_st <- st_as_sf(species_3_st, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_3_st) <- st_crs(crs(covariates2))


#extract predictions
pred_3_st <- as.factor(extract(pred_rf_3_st, species_3_st))

obs_3_st <- as.factor(species_3_st$genus)


cm_3_st<-confusionMatrix(pred_3_st, reference = obs_3_st)

cm_3_st
cm_3_st$byClass


# Woodlands ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_3_prk <-as.data.frame(TBK_trees_parks_3)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_3_prk$class<-factor(trees_3_prk$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_3_prk<-na.omit(trees_3_prk)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_3_prk$class, SplitRatio=0.7)

#Create the new data sets
train_3_prk<-subset(trees_clean_3_prk, sample==TRUE)
test_3_prk<-subset(trees_clean_3_prk, sample==FALSE)

#Check the datasets
table(train_3_prk$genus)
table(test_3_prk$genus)

#Prepare model

rfmodel_3_prk <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_3_prk, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_3_prk


#Prediction
pred_rf_3_prk<-predict(covariates2, rfmodel_3_prk, 
                       filename = "D:/Classifications/Wue/RF/rf_3_prk_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_3_prk<-test_3_prk[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_3_prk <- st_as_sf(species_3_prk, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_3_prk) <- st_crs(crs(covariates2))


#extract predictions
pred_3_prk <- as.factor(extract(pred_rf_3_prk, species_3_prk))

obs_3_prk <- as.factor(species_3_prk$genus)


cm_3_prk<-confusionMatrix(pred_3_prk, reference = obs_3_prk)

cm_3_prk
cm_3_prk$byClass

# Residential ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_3_resi <-as.data.frame(TBK_trees_resi_3)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]
trees_3_resi$class<-factor(trees_3_resi$gattung)
trees_3_resi$class<-factor(trees_3_resi$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_3_resi<-na.omit(trees_3_resi)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_3_resi$class, SplitRatio=0.7)

#Create the new data sets
train_3_resi<-subset(trees_clean_3_resi, sample==TRUE)
test_3_resi<-subset(trees_clean_3_resi, sample==FALSE)

#Check the datasets
table(train_3_resi$genus)
table(test_3_resi$genus)

#Prepare model

rfmodel_3_resi <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                 wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                 wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_3_resi, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_3_resi


#Prediction
pred_rf_3_resi<-predict(covariates2, rfmodel_3_resi, 
                        filename = "D:/Classifications/Wue/RF/rf_3_resi_v1.tif",
                        format ="GTiff",
                        overwrite=TRUE)



species_3_resi<-test_3_resi[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_3_resi <- st_as_sf(species_3_resi, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_3_resi) <- st_crs(crs(covariates2))



#extract predictions
pred_3_resi <- as.factor(extract(pred_rf_3_resi, species_3_resi))

obs_3_resi <- as.factor(species_3_resi$genus)


cm_3_resi<-confusionMatrix(pred_3_resi, reference = obs_3_resi)

cm_3_resi


#Other ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_3_oth <-as.data.frame(TBK_trees_otro_3)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_3_oth$class<-factor(trees_3_oth$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_3_oth<-na.omit(trees_3_oth)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_3_oth$class, SplitRatio=0.7)

#Create the new data sets
train_3_oth<-subset(trees_clean_3_oth, sample==TRUE)
test_3_oth<-subset(trees_clean_3_oth, sample==FALSE)

#Check the datasets
table(train_3_oth$genus)
table(test_3_oth$genus)

#Prepare model

rfmodel_3_oth <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_3_oth, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_3_oth


#Prediction
pred_rf_3_oth<-predict(covariates2, rfmodel_3_oth, 
                       filename = "D:/Classifications/Wue/RF/rf_3_oth_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_3_oth<-test_3_oth[c("coords.x", "coords.y","genus")]


# Convert the data frame to a spatial object (sf)
species_3_oth <- st_as_sf(species_3_oth, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_3_oth) <- st_crs(crs(covariates2))


#extract predictions
pred_3_oth <- as.factor(extract(pred_rf_3_oth, species_3_oth))

obs_3_oth <- as.factor(species_3_oth$genus)


cm_3_oth<-confusionMatrix(pred_3_oth, reference = obs_3_oth)

cm_3_oth


######################################################################################

#Prepare the first two classes
TBK_trees_4<-TBK_trees


TBK_trees_4$genus<-ifelse(TBK_trees_4$gattung == "Acer", 1,
                          ifelse(TBK_trees_4$gattung == "Quercus", 2, 
                                 ifelse(TBK_trees_4$gattung == "Carpinus", 3,
                                        ifelse(TBK_trees_4$gattung == "Fraxinus", 4, 5 ))))


# Divide the trees according to the land use
TBK_trees_streets_4<-subset(TBK_trees_4, lu_raster == 2)
TBK_trees_parks_4<-subset(TBK_trees_4, lu_raster == 1)
TBK_trees_resi_4<-subset(TBK_trees_4, lu_raster == 3)
TBK_trees_otro_4<-subset(TBK_trees_4, lu_raster == 4)


# Streets ---------------------------------------------------------------------------------------------------------------------------

#Transform it to data frame
trees_4_st <-as.data.frame(TBK_trees_streets_4)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_4_st$class<-factor(trees_4_st$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_4_st<-na.omit(trees_4_st)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_4_st$class, SplitRatio=0.7)

#Create the new data sets
train_4_st<-subset(trees_clean_4_st, sample==TRUE)
test_4_st<-subset(trees_clean_4_st, sample==FALSE)

#Check the datasets
table(train_4_st$genus)
table(test_4_st$genus)

#Prepare model

rfmodel_4_st <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                               wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                               wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_4_st, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_4_st

#varImpPlot(rfmodel_4_st, scale = FALSE)#, type=1, scale=TRUE)

#Prediction
pred_rf_4_st<-predict(covariates2, rfmodel_4_st, 
                      filename = "D:/Classifications/Wue/RF/rf_4_st_v1.tif",
                      format ="GTiff",
                      overwrite=TRUE)



species_4_st<-test_4_st[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_4_st <- st_as_sf(species_4_st, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_4_st) <- st_crs(crs(covariates2))


#extract predictions
pred_4_st <- as.factor(extract(pred_rf_4_st, species_4_st))

obs_4_st <- as.factor(species_4_st$genus)


cm_4_st<-confusionMatrix(pred_4_st, reference = obs_4_st)

cm_4_st
cm_4_st$byClass

# Woodlands ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_4_prk <-as.data.frame(TBK_trees_parks_4)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_4_prk$class<-factor(trees_4_prk$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_4_prk<-na.omit(trees_4_prk)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_4_prk$class, SplitRatio=0.7)

#Create the new data sets
train_4_prk<-subset(trees_clean_4_prk, sample==TRUE)
test_4_prk<-subset(trees_clean_4_prk, sample==FALSE)

#Check the datasets
table(train_4_prk$genus)
table(test_4_prk$genus)

#Prepare model

rfmodel_4_prk <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_4_prk, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_4_prk


#Prediction
pred_rf_4_prk<-predict(covariates2, rfmodel_4_prk, 
                       filename = "D:/Classifications/Wue/RF/rf_4_prk_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_4_prk<-test_4_prk[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_4_prk <- st_as_sf(species_4_prk, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_4_prk) <- st_crs(crs(covariates2))


#extract predictions
pred_4_prk <- as.factor(extract(pred_rf_4_prk, species_4_prk))

obs_4_prk <- as.factor(species_4_prk$genus)


cm_4_prk<-confusionMatrix(pred_4_prk, reference = obs_4_prk)

cm_4_prk
cm_4_prk$byClass

# Residential ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_4_resi <-as.data.frame(TBK_trees_resi_4)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_4_resi$class<-factor(trees_4_resi$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_4_resi<-na.omit(trees_4_resi)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_4_resi$class, SplitRatio=0.7)

#Create the new data sets
train_4_resi<-subset(trees_clean_4_resi, sample==TRUE)
test_4_resi<-subset(trees_clean_4_resi, sample==FALSE)

#Check the datasets
table(train_4_resi$genus)
table(test_4_resi$genus)

#Prepare model

rfmodel_4_resi <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                 wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                 wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_4_resi, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_4_resi


#Prediction
pred_rf_4_resi<-predict(covariates2, rfmodel_4_resi, 
                        filename = "D:/Classifications/Wue/RF/rf_4_resi_v1.tif",
                        format ="GTiff",
                        overwrite=TRUE)



species_4_resi<-test_4_resi[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_4_resi <- st_as_sf(species_4_resi, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_4_resi) <- st_crs(crs(covariates2))


#extract predictions
pred_4_resi <- as.factor(extract(pred_rf_4_resi, species_4_resi))

obs_4_resi <- as.factor(species_4_resi$genus)


cm_4_resi<-confusionMatrix(pred_4_resi, reference = obs_4_resi)

cm_4_resi

cm_4_resi$byClass

#Other ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_4_oth <-as.data.frame(TBK_trees_otro_4)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_4_oth$class<-factor(trees_4_oth$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_4_oth<-na.omit(trees_4_oth)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_4_oth$class, SplitRatio=0.7)

#Create the new data sets
train_4_oth<-subset(trees_clean_4_oth, sample==TRUE)
test_4_oth<-subset(trees_clean_4_oth, sample==FALSE)

#Check the datasets
table(train_4_oth$genus)
table(test_4_oth$genus)

#Prepare model

rfmodel_4_oth <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_4_oth, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_4_oth


#Prediction
pred_rf_4_oth<-predict(covariates2, rfmodel_4_oth, 
                       filename = "D:/Classifications/Wue/RF/rf_4_oth_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_4_oth<-test_4_oth[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_4_oth <- st_as_sf(species_4_oth, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_4_oth) <- st_crs(crs(covariates2))


#extract predictions
pred_4_oth <- as.factor(extract(pred_rf_4_oth, species_4_oth))

obs_4_oth <- as.factor(species_4_oth$genus)


cm_4_oth<-confusionMatrix(pred_4_oth, reference = obs_4_oth)

cm_4_oth
cm_4_oth$byClass

######################################################################################

#Prepare the first two classes
TBK_trees_5<-TBK_trees


TBK_trees_5$genus<-ifelse(TBK_trees_5$gattung == "Acer", 1,
                          ifelse(TBK_trees_5$gattung == "Quercus", 2, 
                                 ifelse(TBK_trees_5$gattung == "Carpinus", 3,
                                        ifelse(TBK_trees_5$gattung == "Fraxinus", 4,
                                               ifelse(TBK_trees_5$gattung == "Tilia", 5, 6 )))))


# Divide the trees according to the land use
TBK_trees_streets_5<-subset(TBK_trees_5, lu_raster == 2)
TBK_trees_parks_5<-subset(TBK_trees_5, lu_raster == 1)
TBK_trees_resi_5<-subset(TBK_trees_5, lu_raster == 3)
TBK_trees_otro_5<-subset(TBK_trees_5, lu_raster == 4)


# Streets ---------------------------------------------------------------------------------------------------------------------------

#Transform it to data frame
trees_5_st <-as.data.frame(TBK_trees_streets_5)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_5_st$class<-factor(trees_5_st$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_5_st<-na.omit(trees_5_st)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_5_st$class, SplitRatio=0.7)

#Create the new data sets
train_5_st<-subset(trees_clean_5_st, sample==TRUE)
test_5_st<-subset(trees_clean_5_st, sample==FALSE)

#Check the datasets
table(train_5_st$genus)
table(test_5_st$genus)

#Prepare model

rfmodel_5_st <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                               wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                               wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_5_st, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_5_st

#varImpPlot(rfmodel_5_st, scale = FALSE)#, type=1, scale=TRUE)

#Prediction
pred_rf_5_st<-predict(covariates2, rfmodel_5_st, 
                      filename = "D:/Classifications/Wue/RF/rf_5_st_v1.tif",
                      format ="GTiff",
                      overwrite=TRUE)



species_5_st<-test_5_st[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_5_st <- st_as_sf(species_5_st, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_5_st) <- st_crs(crs(covariates2))

#extract predictions
pred_5_st <- as.factor(extract(pred_rf_5_st, species_5_st))

obs_5_st <- as.factor(species_5_st$genus)


cm_5_st<-confusionMatrix(pred_5_st, reference = obs_5_st)

cm_5_st
cm_5_st$byClass

# Woodlands ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_5_prk <-as.data.frame(TBK_trees_parks_5)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_5_prk$class<-factor(trees_5_prk$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_5_prk<-na.omit(trees_5_prk)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_5_prk$class, SplitRatio=0.7)

#Create the new data sets
train_5_prk<-subset(trees_clean_5_prk, sample==TRUE)
test_5_prk<-subset(trees_clean_5_prk, sample==FALSE)

#Check the datasets
table(train_5_prk$genus)
table(test_5_prk$genus)

#Prepare model

rfmodel_5_prk <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_5_prk, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_5_prk


#Prediction
pred_rf_5_prk<-predict(covariates2, rfmodel_5_prk, 
                       filename = "D:/Classifications/Wue/RF/rf_5_prk_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_5_prk<-test_5_prk[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_5_prk <- st_as_sf(species_5_prk, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_5_prk) <- st_crs(crs(covariates2))


#extract predictions
pred_5_prk <- as.factor(extract(pred_rf_5_prk, species_5_prk))

obs_5_prk <- as.factor(species_5_prk$genus)


cm_5_prk<-confusionMatrix(pred_5_prk, reference = obs_5_prk)

cm_5_prk
cm_5_prk$byClass

# Residential ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_5_resi <-as.data.frame(TBK_trees_resi_5)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_5_resi$class<-factor(trees_5_resi$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_5_resi<-na.omit(trees_5_resi)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_5_resi$class, SplitRatio=0.7)

#Create the new data sets
train_5_resi<-subset(trees_clean_5_resi, sample==TRUE)
test_5_resi<-subset(trees_clean_5_resi, sample==FALSE)

#Check the datasets
table(train_5_resi$genus)
table(test_5_resi$genus)

#Prepare model

rfmodel_5_resi <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                 wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                 wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_5_resi, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_5_resi


#Prediction
pred_rf_5_resi<-predict(covariates2, rfmodel_5_resi, 
                        filename = "D:/Classifications/Wue/RF/rf_5_resi_v1.tif",
                        format ="GTiff",
                        overwrite=TRUE)



species_5_resi<-test_5_resi[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_5_resi <- st_as_sf(species_5_resi, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_5_resi) <- st_crs(crs(covariates2))


#extract predictions
pred_5_resi <- as.factor(extract(pred_rf_5_resi, species_5_resi))

obs_5_resi <- as.factor(species_5_resi$genus)

cm_5_resi<-confusionMatrix(pred_5_resi, reference = obs_5_resi)

cm_5_resi
cm_5_resi$byClass

#Other ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_5_oth <-as.data.frame(TBK_trees_otro_5)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_5_oth$class<-factor(trees_5_oth$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_5_oth<-na.omit(trees_5_oth)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_5_oth$class, SplitRatio=0.7)

#Create the new data sets
train_5_oth<-subset(trees_clean_5_oth, sample==TRUE)
test_5_oth<-subset(trees_clean_5_oth, sample==FALSE)

#Check the datasets
table(train_5_oth$genus)
table(test_5_oth$genus)

#Prepare model

rfmodel_5_oth <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_5_oth, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_5_oth


#Prediction
pred_rf_5_oth<-predict(covariates2, rfmodel_5_oth, 
                       filename = "D:/Classifications/Wue/RF/rf_5_oth_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_5_oth<-test_5_oth[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_5_oth <- st_as_sf(species_5_oth, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_5_oth) <- st_crs(crs(covariates2))

#extract predictions
pred_5_oth <- as.factor(extract(pred_rf_5_oth, species_5_oth))

obs_5_oth <- as.factor(species_5_oth$genus)


cm_5_oth<-confusionMatrix(pred_5_oth, reference = obs_5_oth)

cm_5_oth
cm_5_oth$byClass

######################################################################################

#Prepare the first two classes
TBK_trees_6<-TBK_trees

TBK_trees_6$genus<-ifelse(TBK_trees_6$gattung == "Acer", 1,
                          ifelse(TBK_trees_6$gattung == "Quercus", 2, 
                                 ifelse(TBK_trees_6$gattung == "Carpinus", 3,
                                        ifelse(TBK_trees_6$gattung == "Fraxinus", 4,
                                               ifelse(TBK_trees_6$gattung == "Tilia", 5,
                                                      ifelse(TBK_trees_6$gattung == "Aesculus", 6, 7 ))))))


# Divide the trees according to the land use
TBK_trees_streets_6<-subset(TBK_trees_6, lu_raster == 2)
TBK_trees_parks_6<-subset(TBK_trees_6, lu_raster == 1)
TBK_trees_resi_6<-subset(TBK_trees_6, lu_raster == 3)
TBK_trees_otro_6<-subset(TBK_trees_6, lu_raster == 4)


# Streets ---------------------------------------------------------------------------------------------------------------------------

#Transform it to data frame
trees_6_st <-as.data.frame(TBK_trees_streets_6)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_6_st$class<-factor(trees_6_st$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_6_st<-na.omit(trees_6_st)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_6_st$class, SplitRatio=0.7)

#Create the new data sets
train_6_st<-subset(trees_clean_6_st, sample==TRUE)
test_6_st<-subset(trees_clean_6_st, sample==FALSE)

#Check the datasets
table(train_6_st$genus)
table(test_6_st$genus)

#Prepare model

rfmodel_6_st <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                               wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                               wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_6_st, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_6_st

#varImpPlot(rfmodel_6_st, scale = FALSE)#, type=1, scale=TRUE)

#Prediction
pred_rf_6_st<-predict(covariates2, rfmodel_6_st, 
                      filename = "D:/Classifications/Wue/RF/rf_6_st_v1.tif",
                      format ="GTiff",
                      overwrite=TRUE)



species_6_st<-test_6_st[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_6_st <- st_as_sf(species_6_st, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_6_st) <- st_crs(crs(covariates2))


#extract predictions
pred_6_st <- as.factor(extract(pred_rf_6_st, species_6_st))

obs_6_st <- as.factor(species_6_st$genus)


cm_6_st<-confusionMatrix(pred_6_st, reference = obs_6_st)

cm_6_st
cm_6_st$byClass

# Woodlands ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_6_prk <-as.data.frame(TBK_trees_parks_6)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_6_prk$class<-factor(trees_6_prk$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_6_prk<-na.omit(trees_6_prk)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_6_prk$class, SplitRatio=0.7)

#Create the new data sets
train_6_prk<-subset(trees_clean_6_prk, sample==TRUE)
test_6_prk<-subset(trees_clean_6_prk, sample==FALSE)

#Check the datasets
table(train_6_prk$genus)
table(test_6_prk$genus)

#Prepare model

rfmodel_6_prk <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_6_prk, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_6_prk


#Prediction
pred_rf_6_prk<-predict(covariates2, rfmodel_6_prk, 
                       filename = "D:/Classifications/Wue/RF/rf_6_prk_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_6_prk<-test_6_prk[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_6_prk <- st_as_sf(species_6_prk, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_6_prk) <- st_crs(crs(covariates2))


#extract predictions
pred_6_prk <- as.factor(extract(pred_rf_6_prk, species_6_prk))

obs_6_prk <- as.factor(species_6_prk$genus)


cm_6_prk<-confusionMatrix(pred_6_prk, reference = obs_6_prk)

cm_6_prk
cm_6_prk$byClass

# Residential ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_6_resi <-as.data.frame(TBK_trees_resi_6)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_6_resi$class<-factor(trees_6_resi$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_6_resi<-na.omit(trees_6_resi)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_6_resi$class, SplitRatio=0.7)

#Create the new data sets
train_6_resi<-subset(trees_clean_6_resi, sample==TRUE)
test_6_resi<-subset(trees_clean_6_resi, sample==FALSE)

#Check the datasets
table(train_6_resi$genus)
table(test_6_resi$genus)

#Prepare model

rfmodel_6_resi <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                 wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                 wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_6_resi, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_6_resi


#Prediction
pred_rf_6_resi<-predict(covariates2, rfmodel_6_resi, 
                        filename = "D:/Classifications/Wue/RF/rf_6_resi_v1.tif",
                        format ="GTiff",
                        overwrite=TRUE)



species_6_resi<-test_6_resi[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_6_resi <- st_as_sf(species_6_resi, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_6_resi) <- st_crs(crs(covariates2))

#extract predictions
pred_6_resi <- as.factor(extract(pred_rf_6_resi, species_6_resi))

obs_6_resi <- as.factor(species_6_resi$genus)


cm_6_resi<-confusionMatrix(pred_6_resi, reference = obs_6_resi)

cm_6_resi


#Other ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_6_oth <-as.data.frame(TBK_trees_otro_6)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_6_oth$class<-factor(trees_6_oth$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_6_oth<-na.omit(trees_6_oth)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_6_oth$class, SplitRatio=0.7)

#Create the new data sets
train_6_oth<-subset(trees_clean_6_oth, sample==TRUE)
test_6_oth<-subset(trees_clean_6_oth, sample==FALSE)

#Check the datasets
table(train_6_oth$genus)
table(test_6_oth$genus)

#Prepare model

rfmodel_6_oth <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_6_oth, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_6_oth


#Prediction
pred_rf_6_oth<-predict(covariates2, rfmodel_6_oth, 
                       filename = "D:/Classifications/Wue/RF/rf_6_oth_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_6_oth<-test_6_oth[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_6_oth <- st_as_sf(species_6_oth, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_6_oth) <- st_crs(crs(covariates2))

#extract predictions
pred_6_oth <- as.factor(extract(pred_rf_6_oth, species_6_oth))

obs_6_oth <- as.factor(species_6_oth$genus)


cm_6_oth<-confusionMatrix(pred_6_oth, reference = obs_6_oth)

cm_6_oth
cm_6_oth$byClass

######################################################################################

#Prepare the first two classes
TBK_trees_7<-TBK_trees

TBK_trees_7$genus<-ifelse(TBK_trees_7$gattung == "Acer", 1,
                          ifelse(TBK_trees_7$gattung == "Quercus", 2, 
                                 ifelse(TBK_trees_7$gattung == "Carpinus", 3,
                                        ifelse(TBK_trees_7$gattung == "Fraxinus", 4,
                                               ifelse(TBK_trees_7$gattung == "Tilia", 5,
                                                      ifelse(TBK_trees_7$gattung == "Aesculus", 6, 
                                                             ifelse(TBK_trees_7$gattung == "Platanus",7 , 8)))))))


# Divide the trees according to the land use
TBK_trees_streets_7<-subset(TBK_trees_7, lu_raster == 2)
TBK_trees_parks_7<-subset(TBK_trees_7, lu_raster == 1)
TBK_trees_resi_7<-subset(TBK_trees_7, lu_raster == 3)
TBK_trees_otro_7<-subset(TBK_trees_7, lu_raster == 4)


# Streets ---------------------------------------------------------------------------------------------------------------------------

#Transform it to data frame
trees_7_st <-as.data.frame(TBK_trees_streets_7)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_7_st$class<-factor(trees_7_st$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_7_st<-na.omit(trees_7_st)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_7_st$class, SplitRatio=0.7)

#Create the new data sets
train_7_st<-subset(trees_clean_7_st, sample==TRUE)
test_7_st<-subset(trees_clean_7_st, sample==FALSE)

#Check the datasets
table(train_7_st$genus)
table(test_7_st$genus)

#Prepare model

rfmodel_7_st <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                               wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                               wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_7_st, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_7_st

#varImpPlot(rfmodel_7_st, scale = FALSE)#, type=1, scale=TRUE)

#Prediction
pred_rf_7_st<-predict(covariates2, rfmodel_7_st, 
                      filename = "D:/Classifications/Wue/RF/rf_7_st_v1.tif",
                      format ="GTiff",
                      overwrite=TRUE)



species_7_st<-test_7_st[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_7_st <- st_as_sf(species_7_st, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_7_st) <- st_crs(crs(covariates2))


#extract predictions
pred_7_st <- as.factor(extract(pred_rf_7_st, species_7_st))

obs_7_st <- as.factor(species_7_st$genus)


cm_7_st<-confusionMatrix(pred_7_st, reference = obs_7_st)

cm_7_st
cm_7_st$byClass

# Woodlands ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_7_prk <-as.data.frame(TBK_trees_parks_7)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_7_prk$class<-factor(trees_7_prk$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_7_prk<-na.omit(trees_7_prk)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_7_prk$class, SplitRatio=0.7)

#Create the new data sets
train_7_prk<-subset(trees_clean_7_prk, sample==TRUE)
test_7_prk<-subset(trees_clean_7_prk, sample==FALSE)

#Check the datasets
table(train_7_prk$genus)
table(test_7_prk$genus)

#Prepare model

rfmodel_7_prk <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_7_prk, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_7_prk


#Prediction
pred_rf_7_prk<-predict(covariates2, rfmodel_7_prk, 
                       filename = "D:/Classifications/Wue/RF/rf_7_prk_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_7_prk<-test_7_prk[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_7_prk <- st_as_sf(species_7_prk, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_7_prk) <- st_crs(crs(covariates2))


#extract predictions
pred_7_prk <- as.factor(extract(pred_rf_7_prk, species_7_prk))

obs_7_prk <- as.factor(species_7_prk$genus)


cm_7_prk<-confusionMatrix(pred_7_prk, reference = obs_7_prk)

cm_7_prk
cm_7_prk$byClass

# Residential ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_7_resi <-as.data.frame(TBK_trees_resi_7)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_7_resi$class<-factor(trees_7_resi$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_7_resi<-na.omit(trees_7_resi)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_7_resi$class, SplitRatio=0.7)

#Create the new data sets
train_7_resi<-subset(trees_clean_7_resi, sample==TRUE)
test_7_resi<-subset(trees_clean_7_resi, sample==FALSE)

#Check the datasets
table(train_7_resi$genus)
table(test_7_resi$genus)

#Prepare model

rfmodel_7_resi <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                 wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                 wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_7_resi, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_7_resi


#Prediction
pred_rf_7_resi<-predict(covariates2, rfmodel_7_resi, 
                        filename = "D:/Classifications/Wue/RF/rf_7_resi_v1.tif",
                        format ="GTiff",
                        overwrite=TRUE)



species_7_resi<-test_7_resi[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_7_resi <- st_as_sf(species_7_resi, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_7_resi) <- st_crs(crs(covariates2))

#extract predictions
pred_7_resi <- as.factor(extract(pred_rf_7_resi, species_7_resi))

obs_7_resi <- as.factor(species_7_resi$genus)


cm_7_resi<-confusionMatrix(pred_7_resi, reference = obs_7_resi)

cm_7_resi
cm_7_resi$byClass

#Other ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_7_oth <-as.data.frame(TBK_trees_otro_7)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_7_oth$class<-factor(trees_7_oth$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_7_oth<-na.omit(trees_7_oth)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_7_oth$class, SplitRatio=0.7)

#Create the new data sets
train_7_oth<-subset(trees_clean_7_oth, sample==TRUE)
test_7_oth<-subset(trees_clean_7_oth, sample==FALSE)

#Check the datasets
table(train_7_oth$genus)
table(test_7_oth$genus)

#Prepare model

rfmodel_7_oth <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_7_oth, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_7_oth


#Prediction
pred_rf_7_oth<-predict(covariates2, rfmodel_7_oth, 
                       filename = "D:/Classifications/Wue/RF/rf_7_oth_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_7_oth<-test_7_oth[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_7_oth <- st_as_sf(species_7_oth, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_7_oth) <- st_crs(crs(covariates2))


#extract predictions
pred_7_oth <- as.factor(extract(pred_rf_7_oth, species_7_oth))

obs_7_oth <- as.factor(species_7_oth$genus)


cm_7_oth<-confusionMatrix(pred_7_oth, reference = obs_7_oth)

cm_7_oth

cm_7_oth$byClass

######################################################################################

#Prepare the first two classes
TBK_trees_8<-TBK_trees


TBK_trees_8$genus<-ifelse(TBK_trees_8$gattung == "Acer", 1,
                          ifelse(TBK_trees_8$gattung == "Quercus", 2, 
                                 ifelse(TBK_trees_8$gattung == "Carpinus", 3,
                                        ifelse(TBK_trees_8$gattung == "Fraxinus", 4,
                                               ifelse(TBK_trees_8$gattung == "Tilia", 5,
                                                      ifelse(TBK_trees_8$gattung == "Aesculus", 6, 
                                                             ifelse(TBK_trees_8$gattung == "Platanus",7 ,
                                                                    ifelse(TBK_trees_8$gattung == "Fagus", 8, 9))))))))


# Divide the trees according to the land use
TBK_trees_streets_8<-subset(TBK_trees_8, lu_raster == 2)
TBK_trees_parks_8<-subset(TBK_trees_8, lu_raster == 1)
TBK_trees_resi_8<-subset(TBK_trees_8, lu_raster == 3)
TBK_trees_otro_8<-subset(TBK_trees_8, lu_raster == 4)


# Streets ---------------------------------------------------------------------------------------------------------------------------

#Transform it to data frame
trees_8_st <-as.data.frame(TBK_trees_streets_8)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_8_st$class<-factor(trees_8_st$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_8_st<-na.omit(trees_8_st)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_8_st$class, SplitRatio=0.7)

#Create the new data sets
train_8_st<-subset(trees_clean_8_st, sample==TRUE)
test_8_st<-subset(trees_clean_8_st, sample==FALSE)

#Check the datasets
table(train_8_st$genus)
table(test_8_st$genus)

#Prepare model

rfmodel_8_st <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                               wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                               wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_8_st, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_8_st

#varImpPlot(rfmodel_8_st, scale = FALSE)#, type=1, scale=TRUE)

#Prediction
pred_rf_8_st<-predict(covariates2, rfmodel_8_st, 
                      filename = "D:/Classifications/Wue/RF/rf_8_st_v1.tif",
                      format ="GTiff",
                      overwrite=TRUE)



species_8_st<-test_8_st[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_8_st <- st_as_sf(species_8_st, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_8_st) <- st_crs(crs(covariates2))

#extract predictions
pred_8_st <- as.factor(extract(pred_rf_8_st, species_8_st))

obs_8_st <- as.factor(species_8_st$genus)

cm_8_st<-confusionMatrix(pred_8_st, reference = obs_8_st)

cm_8_st
cm_8_st$byClass

# Woodlands ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_8_prk <-as.data.frame(TBK_trees_parks_8)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_8_prk$class<-factor(trees_8_prk$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_8_prk<-na.omit(trees_8_prk)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_8_prk$class, SplitRatio=0.7)

#Create the new data sets
train_8_prk<-subset(trees_clean_8_prk, sample==TRUE)
test_8_prk<-subset(trees_clean_8_prk, sample==FALSE)

#Check the datasets
table(train_8_prk$genus)
table(test_8_prk$genus)

#Prepare model

rfmodel_8_prk <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_8_prk, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_8_prk


#Prediction
pred_rf_8_prk<-predict(covariates2, rfmodel_8_prk, 
                       filename = "D:/Classifications/Wue/RF/rf_8_prk_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_8_prk<-test_8_prk[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_8_prk <- st_as_sf(species_8_prk, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_8_prk) <- st_crs(crs(covariates2))

#extract predictions
pred_8_prk <- as.factor(extract(pred_rf_8_prk, species_8_prk))

obs_8_prk <- as.factor(species_8_prk$genus)


cm_8_prk<-confusionMatrix(pred_8_prk, reference = obs_8_prk)

cm_8_prk
cm_8_prk$byClass

# Residential ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_8_resi <-as.data.frame(TBK_trees_resi_8)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_8_resi$class<-factor(trees_8_resi$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_8_resi<-na.omit(trees_8_resi)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_8_resi$class, SplitRatio=0.7)

#Create the new data sets
train_8_resi<-subset(trees_clean_8_resi, sample==TRUE)
test_8_resi<-subset(trees_clean_8_resi, sample==FALSE)

#Check the datasets
table(train_8_resi$genus)
table(test_8_resi$genus)

#Prepare model

rfmodel_8_resi <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                 wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                 wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_8_resi, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_8_resi


#Prediction
pred_rf_8_resi<-predict(covariates2, rfmodel_8_resi, 
                        filename = "D:/Classifications/Wue/RF/rf_8_resi_v1.tif",
                        format ="GTiff",
                        overwrite=TRUE)



species_8_resi<-test_8_resi[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_8_resi <- st_as_sf(species_8_resi, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_8_resi) <- st_crs(crs(covariates2))


#extract predictions
pred_8_resi <- as.factor(extract(pred_rf_8_resi, species_8_resi))

obs_8_resi <- as.factor(species_8_resi$genus)


cm_8_resi<-confusionMatrix(pred_8_resi, reference = obs_8_resi)

cm_8_resi
cm_8_resi$byClass

#Other ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_8_oth <-as.data.frame(TBK_trees_otro_8)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_8_oth$class<-factor(trees_8_oth$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_8_oth<-na.omit(trees_8_oth)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_8_oth$class, SplitRatio=0.7)

#Create the new data sets
train_8_oth<-subset(trees_clean_8_oth, sample==TRUE)
test_8_oth<-subset(trees_clean_8_oth, sample==FALSE)

#Check the datasets
table(train_8_oth$genus)
table(test_8_oth$genus)

#Prepare model

rfmodel_8_oth <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_8_oth, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_8_oth


#Prediction
pred_rf_8_oth<-predict(covariates2, rfmodel_8_oth, 
                       filename = "D:/Classifications/Wue/RF/rf_8_oth_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_8_oth<-test_8_oth[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_8_oth <- st_as_sf(species_8_oth, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_8_oth) <- st_crs(crs(covariates2))


#extract predictions
pred_8_oth <- as.factor(extract(pred_rf_8_oth, species_8_oth))

obs_8_oth <- as.factor(species_8_oth$genus)


cm_8_oth<-confusionMatrix(pred_8_oth, reference = obs_8_oth)

cm_8_oth
cm_8_oth$byClass

######################################################################################

#Prepare the first two classes
TBK_trees_9<-TBK_trees

TBK_trees_9$genus<-ifelse(TBK_trees_9$gattung == "Acer", 1,
                          ifelse(TBK_trees_9$gattung == "Quercus", 2, 
                                 ifelse(TBK_trees_9$gattung == "Carpinus", 3,
                                        ifelse(TBK_trees_9$gattung == "Fraxinus", 4,
                                               ifelse(TBK_trees_9$gattung == "Tilia", 5,
                                                      ifelse(TBK_trees_9$gattung == "Aesculus", 6, 
                                                             ifelse(TBK_trees_9$gattung == "Platanus",7 ,
                                                                    ifelse(TBK_trees_9$gattung == "Fagus", 8,
                                                                           ifelse(TBK_trees_9$gattung == "Prunus", 9, 10)))))))))


# Divide the trees according to the land use
TBK_trees_streets_9<-subset(TBK_trees_9, lu_raster == 2)
TBK_trees_parks_9<-subset(TBK_trees_9, lu_raster == 1)
TBK_trees_resi_9<-subset(TBK_trees_9, lu_raster == 3)
TBK_trees_otro_9<-subset(TBK_trees_9, lu_raster == 4)


# Streets ---------------------------------------------------------------------------------------------------------------------------

#Transform it to data frame
trees_9_st <-as.data.frame(TBK_trees_streets_9)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_9_st$class<-factor(trees_9_st$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_9_st<-na.omit(trees_9_st)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_9_st$class, SplitRatio=0.7)

#Create the new data sets
train_9_st<-subset(trees_clean_9_st, sample==TRUE)
test_9_st<-subset(trees_clean_9_st, sample==FALSE)

#Check the datasets
table(train_9_st$genus)
table(test_9_st$genus)

#Prepare model

rfmodel_9_st <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                               wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                               wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_9_st, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_9_st

#varImpPlot(rfmodel_9_st, scale = FALSE)#, type=1, scale=TRUE)

#Prediction
pred_rf_9_st<-predict(covariates2, rfmodel_9_st, 
                      filename = "D:/Classifications/Wue/RF/rf_9_st_v1.tif",
                      format ="GTiff",
                      overwrite=TRUE)



species_9_st<-test_9_st[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_9_st <- st_as_sf(species_9_st, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_9_st) <- st_crs(crs(covariates2))


#extract predictions
pred_9_st <- as.factor(extract(pred_rf_9_st, species_9_st))

obs_9_st <- as.factor(species_9_st$genus)


cm_9_st<-confusionMatrix(pred_9_st, reference = obs_9_st)

cm_9_st
cm_9_st$byClass

# Woodlands ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_9_prk <-as.data.frame(TBK_trees_parks_9)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_9_prk$class<-factor(trees_9_prk$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_9_prk<-na.omit(trees_9_prk)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_9_prk$class, SplitRatio=0.7)

#Create the new data sets
train_9_prk<-subset(trees_clean_9_prk, sample==TRUE)
test_9_prk<-subset(trees_clean_9_prk, sample==FALSE)

#Check the datasets
table(train_9_prk$genus)
table(test_9_prk$genus)

#Prepare model

rfmodel_9_prk <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_9_prk, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_9_prk


#Prediction
pred_rf_9_prk<-predict(covariates2, rfmodel_9_prk, 
                       filename = "D:/Classifications/Wue/RF/rf_9_prk_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_9_prk<-test_9_prk[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_9_prk <- st_as_sf(species_9_prk, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_9_prk) <- st_crs(crs(covariates2))


#extract predictions
pred_9_prk <- as.factor(extract(pred_rf_9_prk, species_9_prk))

obs_9_prk <- as.factor(species_9_prk$genus)


cm_9_prk<-confusionMatrix(pred_9_prk, reference = obs_9_prk)

cm_9_prk
cm_9_prk$byClass

# Residential ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_9_resi <-as.data.frame(TBK_trees_resi_9)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_9_resi$class<-factor(trees_9_resi$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_9_resi<-na.omit(trees_9_resi)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_9_resi$class, SplitRatio=0.7)

#Create the new data sets
train_9_resi<-subset(trees_clean_9_resi, sample==TRUE)
test_9_resi<-subset(trees_clean_9_resi, sample==FALSE)

#Check the datasets
table(train_9_resi$genus)
table(test_9_resi$genus)

#Prepare model

rfmodel_9_resi <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                 wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                 wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_9_resi, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_9_resi


#Prediction
pred_rf_9_resi<-predict(covariates2, rfmodel_9_resi, 
                        filename = "D:/Classifications/Wue/RF/rf_9_resi_v1.tif",
                        format ="GTiff",
                        overwrite=TRUE)



species_9_resi<-test_9_resi[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_9_resi <- st_as_sf(species_9_resi, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_9_resi) <- st_crs(crs(covariates2))


#extract predictions
pred_9_resi <- as.factor(extract(pred_rf_9_resi, species_9_resi))

obs_9_resi <- as.factor(species_9_resi$genus)


cm_9_resi<-confusionMatrix(pred_9_resi, reference = obs_9_resi)

cm_9_resi
cm_9_resi$byClass

#Other ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_9_oth <-as.data.frame(TBK_trees_otro_9)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_9_oth$class<-factor(trees_9_oth$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_9_oth<-na.omit(trees_9_oth)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_9_oth$class, SplitRatio=0.7)

#Create the new data sets
train_9_oth<-subset(trees_clean_9_oth, sample==TRUE)
test_9_oth<-subset(trees_clean_9_oth, sample==FALSE)

#Check the datasets
table(train_9_oth$genus)
table(test_9_oth$genus)

#Prepare model

rfmodel_9_oth <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_9_oth, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_9_oth


#Prediction
pred_rf_9_oth<-predict(covariates2, rfmodel_9_oth, 
                       filename = "D:/Classifications/Wue/RF/rf_9_oth_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_9_oth<-test_9_oth[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_9_oth <- st_as_sf(species_9_oth, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_9_oth) <- st_crs(crs(covariates2))


#extract predictions
pred_9_oth <- as.factor(extract(pred_rf_9_oth, species_9_oth))

obs_9_oth <- as.factor(species_9_oth$genus)


cm_9_oth<-confusionMatrix(pred_9_oth, reference = obs_9_oth)

cm_9_oth
cm_9_oth$byClass

######################################################################################

#Prepare the first two classes
TBK_trees_10<-TBK_trees


TBK_trees_10$genus<-ifelse(TBK_trees_10$gattung == "Acer", 1,
                           ifelse(TBK_trees_10$gattung == "Quercus", 2, 
                                  ifelse(TBK_trees_10$gattung == "Carpinus", 3,
                                         ifelse(TBK_trees_10$gattung == "Fraxinus", 4,
                                                ifelse(TBK_trees_10$gattung == "Tilia", 5,
                                                       ifelse(TBK_trees_10$gattung == "Aesculus", 6, 
                                                              ifelse(TBK_trees_10$gattung == "Platanus",7 ,
                                                                     ifelse(TBK_trees_10$gattung == "Fagus", 8,
                                                                            ifelse(TBK_trees_10$gattung == "Prunus", 9,
                                                                                   ifelse(TBK_trees_10$gattung == "Pinus", 10, 11))))))))))


# Divide the trees according to the land use
TBK_trees_streets_10<-subset(TBK_trees_10, lu_raster == 2)
TBK_trees_parks_10<-subset(TBK_trees_10, lu_raster == 1)
TBK_trees_resi_10<-subset(TBK_trees_10, lu_raster == 3)
TBK_trees_otro_10<-subset(TBK_trees_10, lu_raster == 4)


# Streets ---------------------------------------------------------------------------------------------------------------------------

#Transform it to data frame
trees_10_st <-as.data.frame(TBK_trees_streets_10)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_10_st$class<-factor(trees_10_st$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_10_st<-na.omit(trees_10_st)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_10_st$class, SplitRatio=0.7)

#Create the new data sets
train_10_st<-subset(trees_clean_10_st, sample==TRUE)
test_10_st<-subset(trees_clean_10_st, sample==FALSE)

#Check the datasets
table(train_10_st$genus)
table(test_10_st$genus)

#Prepare model

rfmodel_10_st <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_10_st, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_10_st

#varImpPlot(rfmodel_10_st, scale = FALSE)#, type=1, scale=TRUE)

#Prediction
pred_rf_10_st<-predict(covariates2, rfmodel_10_st, 
                       filename = "D:/Classifications/Wue/RF/rf_10_st_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_10_st<-test_10_st[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_10_st <- st_as_sf(species_10_st, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_10_st) <- st_crs(crs(covariates2))


#extract predictions
pred_10_st <- as.factor(extract(pred_rf_10_st, species_10_st))

obs_10_st <- as.factor(species_10_st$genus)


cm_10_st<-confusionMatrix(pred_10_st, reference = obs_10_st)

cm_10_st
cm_10_st$byClass

# Woodlands ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_10_prk <-as.data.frame(TBK_trees_parks_10)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_10_prk$class<-factor(trees_10_prk$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_10_prk<-na.omit(trees_10_prk)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_10_prk$class, SplitRatio=0.7)

#Create the new data sets
train_10_prk<-subset(trees_clean_10_prk, sample==TRUE)
test_10_prk<-subset(trees_clean_10_prk, sample==FALSE)

#Check the datasets
table(train_10_prk$genus)
table(test_10_prk$genus)

#Prepare model

rfmodel_10_prk <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                 wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                 wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_10_prk, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_10_prk


#Prediction
pred_rf_10_prk<-predict(covariates2, rfmodel_10_prk, 
                        filename = "D:/Classifications/Wue/RF/rf_10_prk_v1.tif",
                        format ="GTiff",
                        overwrite=TRUE)



species_10_prk<-test_10_prk[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_10_prk <- st_as_sf(species_10_prk, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_10_prk) <- st_crs(crs(covariates2))


#extract predictions
pred_10_prk <- as.factor(extract(pred_rf_10_prk, species_10_prk))

obs_10_prk <- as.factor(species_10_prk$genus)


cm_10_prk<-confusionMatrix(pred_10_prk, reference = obs_10_prk)

cm_10_prk
cm_10_prk$byClass

# Residential ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_10_resi <-as.data.frame(TBK_trees_resi_10)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_10_resi$class<-factor(trees_10_resi$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_10_resi<-na.omit(trees_10_resi)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_10_resi$class, SplitRatio=0.7)

#Create the new data sets
train_10_resi<-subset(trees_clean_10_resi, sample==TRUE)
test_10_resi<-subset(trees_clean_10_resi, sample==FALSE)

#Check the datasets
table(train_10_resi$genus)
table(test_10_resi$genus)

#Prepare model

rfmodel_10_resi <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                  wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                  wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_10_resi, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_10_resi


#Prediction
pred_rf_10_resi<-predict(covariates2, rfmodel_10_resi, 
                         filename = "D:/Classifications/Wue/RF/rf_10_resi_v1.tif",
                         format ="GTiff",
                         overwrite=TRUE)



species_10_resi<-test_10_resi[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_10_resi <- st_as_sf(species_10_resi, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_10_resi) <- st_crs(crs(covariates2))


#extract predictions
pred_10_resi <- as.factor(extract(pred_rf_10_resi, species_10_resi))

obs_10_resi <- as.factor(species_10_resi$genus)


cm_10_resi<-confusionMatrix(pred_10_resi, reference = obs_10_resi)

cm_10_resi
cm_10_resi$byClass

#Other ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_10_oth <-as.data.frame(TBK_trees_otro_10)[c( "coords.x", "coords.y", "gattung", "genus", "wu_j20_1_b1", "wu_j20_1_b2", "wu_j20_1_b3", "wu_j20_1_b4", "wu_j20_1_b5", "wu_j20_1_b6", "wu_j20_1_b7", "wu_j20_1_b8", "wu_j20_2_b1", "wu_j20_2_b2", "wu_j20_2_b3", "wu_j20_2_b4", "wu_j20_2_b5", "wu_j20_2_b6", "wu_j20_2_b7", "wu_j20_2_b8", "ndvi_wu_j20_1", "ndvi_wu_j20_2", "chm" )]

trees_10_oth$class<-factor(trees_10_oth$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_10_oth<-na.omit(trees_10_oth)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_10_oth$class, SplitRatio=0.7)

#Create the new data sets
train_10_oth<-subset(trees_clean_10_oth, sample==TRUE)
test_10_oth<-subset(trees_clean_10_oth, sample==FALSE)

#Check the datasets
table(train_10_oth$genus)
table(test_10_oth$genus)

#Prepare model

rfmodel_10_oth <- randomForest(class ~ wu_j20_1_b1 + wu_j20_1_b2 + wu_j20_1_b3 + wu_j20_1_b4 + wu_j20_1_b5 + wu_j20_1_b6 + wu_j20_1_b7 + 
                                 wu_j20_1_b8 + wu_j20_2_b1 + wu_j20_2_b2 + wu_j20_2_b3 + wu_j20_2_b4 + wu_j20_2_b5 + wu_j20_2_b6 + wu_j20_2_b7 + 
                                 wu_j20_2_b8 + ndvi_wu_j20_1 + ndvi_wu_j20_2 + chm, data=train_10_oth, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_10_oth


#Prediction
pred_rf_10_oth<-predict(covariates2, rfmodel_10_oth, 
                        filename = "D:/Classifications/Wue/RF/rf_10_oth_v1.tif",
                        format ="GTiff",
                        overwrite=TRUE)



species_10_oth<-test_10_oth[c("coords.x", "coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_10_oth <- st_as_sf(species_10_oth, coords = c("coords.x", "coords.y"))

# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_10_oth) <- st_crs(crs(covariates2))


#extract predictions
pred_10_oth <- as.factor(extract(pred_rf_10_oth, species_10_oth))

obs_10_oth <- as.factor(species_10_oth$genus)


cm_10_oth<-confusionMatrix(pred_10_oth, reference = obs_10_oth)

cm_10_oth
cm_10_oth$byClass
