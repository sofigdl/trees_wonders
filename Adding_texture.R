#Load libraries
pacman::p_load(raster, terra, glcm, caret, sf, RStoolbox, randomForest, caTools, car, corrplot, rfUtilities)



############# 1. The first step is doing the pansharpening

############# 2. Then we load the rasters 

# Load the three rasters
raster_sep21 <- rast("D:/Preprocessed/Pan-sharpened/Gram-Schmitt/pan21SEP03100248M2AS22EUSI178511.tif")

raster_sep22 <- rast("D:/Preprocessed/Pan-sharpened/Gram-Schmitt/pan22SEP22102317M2AS22EUSI178511.tif")

raster_jul19<- rast("D:/Preprocessed/Clipped/clip-19JUL-WV2-middle.tif")

cropped_mrs21 <- crop(raster_sep21, ext(raster_jul19))
cropped_mrs22 <- crop(raster_sep22, ext(raster_jul19))

plot(cropped_mrs22)

############# 4. The next step is doing the coregistration in QGIS


############# 5. Estimate Vegetation Indices
#Load co-registered raster
mr_s21<- cropped_mrs21
mr_s22<- resample(cropped_mrs22, mr_s21)
mr_j19<-resample(raster_jul19, mr_s21)


#Create the functions for the indices
#NDVI
ndvi<-function(s){
  nir=subset(s, 7) #pay attention to the band order in WV2
  red=subset(s, 5) 
  
  (nir-red)/(nir+red)
}

#NDVI
ndvi_pan<-function(s){
  nir=subset(s, 4) #pay attention to the band order in WV2
  red=subset(s, 1) 
  
  (nir-red)/(nir+red)
}

#apply the ndvi
ndvi_mrj19<- ndvi(mr_j19)
ndvi_mrs21<- ndvi_pan(mr_s21)
ndvi_mrs22<- ndvi_pan(mr_s22)

#--------------------------------------------------------------------------------

#NDRE: Normalized Difference Red Edge Index
ndre<- function(s){
  rededge=subset(s, 6)
  red=subset(s, 5)
  
  
  (rededge-red)/(rededge+red)
}

#apply the evi
ndre_mrj19<- ndre(mr_j19)

#--------------------------------------------------------------------------------

#RVI: Ratio Vegetation Index
rvi<- function(s){
  nir=subset(s, 7) #pay attention to the band order in WV2
  red=subset(s, 5)
  
  
  red/nir
}

#RVI: Ratio Vegetation Index
rvi_pan<- function(s){
  nir=subset(s, 4) #pay attention to the band order in WV2
  red=subset(s, 1)
  
  
  red/nir
}

#apply the evi
RVI_mrj19<- rvi(mr_j19)
RVI_mrs21<- rvi_pan(mr_s21)
RVI_mrs22<- rvi_pan(mr_s22)
#--------------------------------------------------------------------------------

# EVI: Enhanced Vegetation Index
evi <- function(s) {
  # Band 7: Near-Infrared (NIR1)
  # Band 5: Red
  # Band 2: Blue
  nir <- s[[7]]
  red <- s[[5]]
  blue <- s[[2]]
  
  # Constants for EVI calculation
  G <- 2.5
  C1 <- 6
  C2 <- 7.5
  L <- 1
  
  # Calculate EVI
  G * (nir - red) / (nir + C1 * red - C2 * blue + L)
}

evi_pan <- function(s) {
  # Band 7: Near-Infrared (NIR1)
  # Band 5: Red
  # Band 2: Blue
  nir <- s[[4]]
  red <- s[[1]]
  blue <- s[[3]]
  
  # Constants for EVI calculation
  G <- 2.5
  C1 <- 6
  C2 <- 7.5
  L <- 1
  
  # Calculate EVI
  G * (nir - red) / (nir + C1 * red - C2 * blue + L)
}

# Apply the EVI function to the WorldView images
evi_mrj19 <- evi(mr_j19)
evi_mrs21 <- evi_pan(mr_s21)
evi_mrs22 <- evi_pan(mr_s22)

#--------------------------------------------------------------------------------

#SAVI: Soil-Adjusted Vegetation Index

savi <- function(s) {
  # Band 7: Near-Infrared (NIR1)
  # Band 5: Red
  nir <- s[[7]]
  red <- s[[5]]
  
  # Soil adjustment factor
  L <- 0.5
  
  # Calculate SAVI
  (nir - red) / (nir + red + L) * (1 + L)
}


savi_pan <- function(s) {
  # Band 7: Near-Infrared (NIR1)
  # Band 5: Red
  nir <- s[[4]]
  red <- s[[1]]
  
  # Soil adjustment factor
  L <- 0.5
  
  # Calculate SAVI
  (nir - red) / (nir + red + L) * (1 + L)
}
# Apply the SAVI function to the WorldView images
savi_mrj19 <- savi(mr_j19)
savi_mrs21 <- savi_pan(mr_s21)
savi_mrs22 <- savi_pan(mr_s22)

############# 6. Load and preprocess the other covariates (CHM and LU)

chm<-rast("D:/nDSM_Muc.tif")
chm<-resample(chm, ndvi_mrs21)
plot(chm)

lu_raster<-ndvi_mrs21
lu<-st_read("C:/Users/ang58gl/Documents/Data/LU_dissolved.shp")
lu_raster<-rasterize(x=lu, y=lu_raster, field="Cat")
lu_raster
plot(lu_raster)

############# 7. Estimate Texture Indices

convert_to_raster_layers <- function(spat_raster) {
  raster_layers <- lapply(1:nlyr(spat_raster), function(i) {
    raster::raster(spat_raster[[i]])
  })
  return(raster_layers)
}

# Define a function to compute texture features for a given band
compute_texture_features <- function(band, window_size = 5) {
  textures <- glcm::glcm(
    band, 
    window = c(window_size, window_size),
    statistics = c("mean", "variance", "homogeneity", "contrast", "dissimilarity", "entropy", "correlation")
  )
  return(textures)
}

# Convert the SpatRaster to a list of RasterLayer objects
raster_layers <- convert_to_raster_layers(mr_j19)

# Apply the function to each band in the covariates raster
texture_features_list <- lapply(raster_layers, compute_texture_features)

# Optionally, you can combine the texture features back into a single SpatRaster object
texture_features_rasters <- lapply(texture_features_list, function(x) {
  stack(x)
})
texture_features_stack <- stack(texture_features_rasters)
text_19 <- rast(texture_features_stack)
#--------------------------------------------------------------------------
# Convert the SpatRaster to a list of RasterLayer objects
raster_layers_21 <- convert_to_raster_layers(mr_s21)

# Apply the function to each band in the covariates raster
texture_features_list_21 <- lapply(raster_layers_21, compute_texture_features)

# Optionally, you can combine the texture features back into a single SpatRaster object
texture_features_rasters_21 <- lapply(texture_features_list_21, function(x) {
  stack(x)
})
texture_features_stack_21 <- stack(texture_features_rasters_21)

text_21 <- rast(texture_features_stack_21)
#--------------------------------------------------------------------------

# Convert the SpatRaster to a list of RasterLayer objects
raster_layers_22 <- convert_to_raster_layers(mr_s22)

# Apply the function to each band in the covariates raster
texture_features_list_22 <- lapply(raster_layers_22, compute_texture_features)

# Optionally, you can combine the texture features back into a single SpatRaster object
texture_features_rasters_22 <- lapply(texture_features_list_22, function(x) {
  stack(x)
})
texture_features_stack_22 <- stack(texture_features_rasters_22)

text_22 <- rast(texture_features_stack_22)

#---------------------------------------------------------------------------
#compare geometries
compareGeom(chm, mr_j19)


input_layers_trees<-rast(list(mr_j19, mr_s21, mr_s22, evi_mrj19, evi_mrs21, evi_mrs22, ndre_mrj19, ndvi_mrj19, ndvi_mrs21, ndvi_mrs22, chm,
                              RVI_mrj19, RVI_mrs21, RVI_mrs22, savi_mrj19, savi_mrs21, savi_mrs22, text_19, text_21, lu_raster))


############# 9. Prepare training data


data_points<-TBK_trees

head(data_points)
#imbalance on training data
table(data_points$species)

fin=NULL
for (i in unique(data_points$y)) {
  sub=subset(data_points, y==i)
  sam=sub[sample(nrow(sub), 500), ]
  fin=rbind(fin, sam)}




both<- ovun.sample(species~.,
                   data = data_points,
                   method = "both")


sub0<-subset(data_points, species==0)
sub1<-subset(data_points, species==1)
sub2<-subset(data_points, species==2)
sub3<-subset(data_points, species==3)
sub4<-subset(data_points, species==4)
sub5<-subset(data_points, species==5)
sub6<-subset(data_points, species==6)
sub7<-subset(data_points, species==7)
sub8<-subset(data_points, species==8)
sub9<-subset(data_points, species==9)

sam0<-sub0[sample(nrow(sub0), 3300), ]
sam1<-sub1[sample(nrow(sub1), 7778), ]
sam8<-sub8[sample(nrow(sub8), 6065), ]
sam9<-sub9[sample(nrow(sub9), 5562), ]

fin2<-rbind( sam1, sub2, sub3, sub4, sub5, sub6, sub7, sam8, sam9)


trees_bg <- fin2[c('coords.x','coords.y', 'species', 'geom')]
#change their names
names(trees_bg) <- c("x", "y", "species", "geom" )

#Transform into spatial objects by assigning coordinates and projection
coordinates(trees_bg)<-~x+y
crs(trees_bg)<- crs(input_layers_trees)


#Extract 

trees_checked<-data.frame(coordinates(trees_sinbg),
                          trees_sinbg$species,
                          extract(input_layers_masked, vect(trees_sinbg)))

plot(trees_checked)
summary(trees_checked)



############# 10. Run pixel-based classifier

#set directory
setwd("D:/Paper_1/Classification_T")

#make it reproducible
set.seed(23)
#--------------------------------------------------------------------------------



#Fit classifier test 5. Applying filters and using the background points
names(input_layers_trees)<-c("19JUL-WV2_b1", "19JUL-WV2_b2", "19JUL-WV2_b3", "19JUL-WV2_b4", "19JUL-WV2_b5", "19JUL-WV2_b6", "19JUL-WV2_b7", "19JUL-WV2_b8", "21SEP-WV2_b1", "21SEP-WV2_b2", "21SEP-WV2_b3",
                             "21SEP-WV2_b4", "22SEP-WV2_b1", "22SEP-WV2_b2", "22SEP-WV2_b3", "22SEP-WV2_b4", "19JUL-EVI", "21SEP-EVI", "22SEP-EVI", "19JUL-NDRE", "19JUL-NDVI", "21SEP-NDVI", "22SEP-NDVI", 
                             "CHM", "19JUL-RVI", "21SEP-RVI", "22SEP-RVI", "19JUL-SAVI", "21SEP-SAVI", "22SEP-SAVI", "19JUL_mean_b1", "19JUL_variance_b1", "19JUL_homogeneity_b1", "19JUL_constrast_b1", 
                             "19JUL_dissimilarity_b1", "19JUL_entropy_b1", "19JUL_correlation_b1", "19JUL_mean_b2", "19JUL_variance_b2", "19JUL_homogeneity_b2", "19JUL_constrast_b2", 
                             "19JUL_dissimilarity_b2", "19JUL_entropy_b2", "19JUL_correlation_b2", "19JUL_mean_b3", "19JUL_variance_b3", "19JUL_homogeneity_b3", "19JUL_constrast_b3", 
                             "19JUL_dissimilarity_b3", "19JUL_entropy_b3", "19JUL_correlation_b3", "19JUL_mean_b4", "19JUL_variance_b4", "19JUL_homogeneity_b4", "19JUL_constrast_b4", 
                             "19JUL_dissimilarity_b4", "19JUL_entropy_b4", "19JUL_correlation_b4", "19JUL_mean_b5", "19JUL_variance_b5", "19JUL_homogeneity_b5", "19JUL_constrast_b5", 
                             "19JUL_dissimilarity_b5", "19JUL_entropy_b5", "19JUL_correlation_b5", "19JUL_mean_b6", "19JUL_variance_b6", "19JUL_homogeneity_b6", "19JUL_constrast_b6", 
                             "19JUL_dissimilarity_b6", "19JUL_entropy_b6", "19JUL_correlation_b6", "19JUL_mean_b7", "19JUL_variance_b7", "19JUL_homogeneity_b7", "19JUL_constrast_b7", 
                             "19JUL_dissimilarity_b7", "19JUL_entropy_b7", "19JUL_correlation_b7", "19JUL_mean_b8", "19JUL_variance_b8", "19JUL_homogeneity_b8", "19JUL_constrast_b8", 
                             "19JUL_dissimilarity_b8", "19JUL_entropy_b8", "19JUL_correlation_b8", "21SEP_mean_b1", "21SEP_variance_b1", "21SEP_homogeneity_b1", "21SEP_constrast_b1", 
                             "21SEP_dissimilarity_b1", "21SEP_entropy_b1", "21SEP_correlation_b1", "21SEP_mean_b2", "21SEP_variance_b2", "21SEP_homogeneity_b2", "21SEP_constrast_b2", 
                             "21SEP_dissimilarity_b2", "21SEP_entropy_b2", "21SEP_correlation_b2", "21SEP_mean_b3", "21SEP_variance_b3", "21SEP_homogeneity_b3", "21SEP_constrast_b3", 
                             "21SEP_dissimilarity_b3", "21SEP_entropy_b3", "21SEP_correlation_b3", "21SEP_mean_b4", "21SEP_variance_b4", "21SEP_homogeneity_b4", "21SEP_constrast_b4", 
                             "21SEP_dissimilarity_b4", "21SEP_entropy_b4", "21SEP_correlation_b4", #"22SEP_mean_b1", "22SEP_variance_b1", "22SEP_homogeneity_b1", "22SEP_constrast_b1", 
                            # "22SEP_dissimilarity_b1", "22SEP_entropy_b1", "22SEP_correlation_b1", "22SEP_mean_b2", "22SEP_variance_b2", "22SEP_homogeneity_b2", "22SEP_constrast_b2", 
                            # "22SEP_dissimilarity_b2", "22SEP_entropy_b2", "22SEP_correlation_b2", "22SEP_mean_b3", "22SEP_variance_b3", "22SEP_homogeneity_b3", "22SEP_constrast_b3", 
                           #  "22SEP_dissimilarity_b3", "22SEP_entropy_b3", "22SEP_correlation_b3", "22SEP_mean_b4", "22SEP_variance_b4", "22SEP_homogeneity_b4", "22SEP_constrast_b4", 
                            # "22SEP_dissimilarity_b4", "22SEP_entropy_b4", "22SEP_correlation_b4",
                           "Land_Use")
                                       


extracted_values <- terra::extract(input_layers_trees, fin2)

# Combine the extracted values with the original point data
fin2_with_values <- cbind(fin2, extracted_values)

# Print the combined data
print(fin2_with_values)

fin2_with_values$class<-factor(fin2_with_values$species)

#Make it replicable
set.seed(23)

#Clean the dataset
fin2_with_values<-na.omit(fin2_with_values)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-caTools::sample.split(fin2_with_values$class, SplitRatio = 0.7)

#Create the new data sets
train_rf<-subset(fin2_with_values, sample==TRUE)
test_rf<-subset(fin2_with_values, sample==FALSE)

#Check the datasets
table(train_rf$class)
table(test_rf$class)

#Prepare model

rfmodel_gen <- randomForest(class ~ X19JUL.WV2_b1 + X19JUL.WV2_b2 + X19JUL.WV2_b3 + X19JUL.WV2_b4 + X19JUL.WV2_b5 + X19JUL.WV2_b6 + X19JUL.WV2_b7 + X19JUL.WV2_b8 + X21SEP.WV2_b1 + X21SEP.WV2_b2 + X21SEP.WV2_b3 +
                            X21SEP.WV2_b4 + X22SEP.WV2_b1 + X22SEP.WV2_b2 + X22SEP.WV2_b3 + X22SEP.WV2_b4 + X19JUL.EVI + X21SEP.EVI + X22SEP.EVI + X19JUL.NDRE + X19JUL.NDVI + X21SEP.NDVI + X22SEP.NDVI + 
                            CHM + X19JUL.RVI + X21SEP.RVI + X22SEP.RVI + X19JUL.SAVI + X21SEP.SAVI + X22SEP.SAVI + X19JUL_mean_b1 + X19JUL_variance_b1 + X19JUL_homogeneity_b1 + X19JUL_constrast_b1 + 
                            X19JUL_dissimilarity_b1 + X19JUL_entropy_b1 +  X19JUL_mean_b2 + X19JUL_variance_b2 + X19JUL_homogeneity_b2 + X19JUL_constrast_b2 +
                            X19JUL_dissimilarity_b2 + X19JUL_entropy_b2 +  X19JUL_mean_b3 + X19JUL_variance_b3 + X19JUL_homogeneity_b3 + X19JUL_constrast_b3 +
                            X19JUL_dissimilarity_b3 + X19JUL_entropy_b3 +  X19JUL_mean_b4 + X19JUL_variance_b4 + X19JUL_homogeneity_b4 + X19JUL_constrast_b4 +
                            X19JUL_dissimilarity_b4 + X19JUL_entropy_b4 +  X19JUL_mean_b5 + X19JUL_variance_b5 + X19JUL_homogeneity_b5 + X19JUL_constrast_b5 +
                            X19JUL_dissimilarity_b5 + X19JUL_entropy_b5 +  X19JUL_mean_b6 + X19JUL_variance_b6 + X19JUL_homogeneity_b6 + X19JUL_constrast_b6 +
                            X19JUL_dissimilarity_b6 + X19JUL_entropy_b6 +  X19JUL_mean_b7 + X19JUL_variance_b7 + X19JUL_homogeneity_b7 + X19JUL_constrast_b7 +
                            X19JUL_dissimilarity_b7 + X19JUL_entropy_b7 +  X19JUL_mean_b8 + X19JUL_variance_b8 + X19JUL_homogeneity_b8 + X19JUL_constrast_b8 +
                            X19JUL_dissimilarity_b8 + X19JUL_entropy_b8 +  X21SEP_mean_b1 + X21SEP_variance_b1 + X21SEP_homogeneity_b1 + X21SEP_constrast_b1 +
                            X21SEP_dissimilarity_b1 + X21SEP_entropy_b1 +  X21SEP_mean_b2 + X21SEP_variance_b2 + X21SEP_homogeneity_b2 + X21SEP_constrast_b2 +
                            X21SEP_dissimilarity_b2 + X21SEP_entropy_b2 +  X21SEP_mean_b3 + X21SEP_variance_b3 + X21SEP_homogeneity_b3 + X21SEP_constrast_b3 +
                            X21SEP_dissimilarity_b3 + X21SEP_entropy_b3 +  X21SEP_mean_b4 + X21SEP_variance_b4 + X21SEP_homogeneity_b4 + X21SEP_constrast_b4 + 
                            X21SEP_dissimilarity_b4 + X21SEP_entropy_b4 +  Land_Use, data=train_rf, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_gen


varImpPlot(rfmodel_gen, scale = FALSE)#, type=1, scale=TRUE)

#Prediction
pred_rf_10_st<-predict(covariates2, rfmodel_10_st, 
                       filename = "D:/Paper_1/st_class.tif",
                       format ="GTiff")



species_10_st<-test_10_st[c("x","y","genus")]
coordinates(species_10_st)<-~x+y
projection(species_10_st)<- projection(TBK_trees)


#extract predictions
pred_10_st <- as.factor(extract(pred_rf_10_st, species_10_st))

obs_10_st <- as.factor(species_10_st$genus)


cm_10_st<-confusionMatrix(pred_10_st, reference = obs_10_st)

cm_10_st
