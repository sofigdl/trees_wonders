install.packages("sf")
library (car)
library(sf)
library(corrplot)
library(randomForest)
library(caTools)
library(raster)
library(RStoolbox)
library(rfUtilities)
library(terra)
library(caret)
library(dplyr)

#Load vector
TBK_trees<-st_read("D:/Trees_data/Baum_kataster_Munich_32N_sampled.gpkg")

TBK_trees$coords.x<-st_coordinates(TBK_trees)[,1]
TBK_trees$coords.y<-st_coordinates(TBK_trees)[,2]



names(TBK_trees)<-c("OBJECTID", "BAUM_NUMMER", "BAUM_EXISTIERT", "STRASSE_HNR", "PLATZFLAECHE", "STANDORTMERKMAL", "BODENVOLUMEN", "OBERFLAECHENSTRUKTUR",   
                    "GATTUNG", "ART", "SORTE", "BAUMART_LAT", "BAUMART_DE", "VERKEHRSBELASTUNG", "HOEHE_M", "HOEHENKLASSE", "STAMMFORM",
                    "STAMMDURCHM_1_CM", "STAMMDURCHM_2_CM", "STAMMDURCHM_3_CM", "STAMMDURCHM_4_CM", "STAMMDURCHM_5_CM", "STAMMDURCHM_BEMERK", "STAMMUMFANG_1_BERECHNET",
                    "STAMMUMFANG_2_BERECHNET", "STAMMUMFANG_3_BERECHNET", "STAMMUMFANG_4_BERECHNET", "STAMMUMFANG_5_BERECHNET",
                    "KRONE_M", "KRONEN_KLASSE", "VITALITAET", "BAUMALTER", "STAMMSCHAEDEN", "RINDENNEKROSEN", "MECHAN_SCHAEDEN", "STAMMFUSSSCHADEN",       
                    "FAULHERDE", "KRANKHEITEN_SCHAEDLINGE", "ESCHENTRIEBSTERBEN", "KASTANIENMINIERMOTTE", "PILZFRUCHTKOERPER", "BLATTNEKROSEN", "BEMERKUNG_KRANKHEITEN",
                    "BIOTOP", "NATURDENKMAL", "X_UTM", "Y_UTM", "FA_FREIANLAGENKATEGORIE", "REF_STADTBEZIRK", "GLOBALID", "RVI_mrj19", "RVI_mrs21", "ndvi_mrj19", "ndvi_mrs21", "ndre_mrj19", "ndre_mrs21", "chm", "lu_raster", 
                    "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8",
                    "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "geom", "x", "y")


#_________________________________________________________________________________________________________________________________________________________

Wue_trees<-st_read("D:/Preprocessed/Test_wue/trees-sampled.gpkg")
Wue_trees$coords.x<-st_coordinates(Wue_trees)[,1]
Wue_trees$coords.y<-st_coordinates(Wue_trees)[,2]


Wue_trees <- Wue_trees %>%
  mutate(gattung = sub("^(\\w+).*", "\\1", baumart_la))


Wue_trees<-as.data.frame((Wue_trees))

# Bands to subset
bands_to_subset <- c("source_id", "coords.x", "coords.y", "baumart", "gattung", "kronenbrei", "baumhoehe", "CO_1", "CO_2", "CO_3", "CO_4", "CO_5", "CO_6", "CO_7", "CO_8", "CO_9", "CO_10",
                     "CO_11", "CO_12", "CO_13", "CO_14", "CO_15", "CO_16", "CO_17", "CO_18", "CO_19", "CO_20", "geom")

#Subset the raster bands
Wue_trees <- Wue_trees[ , bands_to_subset]


names(Wue_trees)<-c("source_id", "coords.x", "coords.y", "baumart", "gattung", "kronenbrei", "baumhoehe",   
                    "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8",
                    "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", 
                    "ndvi_mrj19", "ndvi_mrs21", "chm", "lu_raster", "geom")


# Establish the frequency
##############################################################################

# Column name for frequency ordering
column_name <- "GATTUNG"

# Extract unique values and their frequencies
value_counts <- as.data.frame(table(TBK_trees[[column_name]]))

# Rename the columns
colnames(value_counts) <- c("Value", "Frequency")

# Sort the data frame by Frequency column
sorted_value_counts <- value_counts[order(value_counts$Frequency), ]

# Print the sorted value counts table
print(sorted_value_counts)


#########################################################################################

#Load raster
covariates<-stack("D:/Preprocessed/New_preprocessing/covariates_masked.tif")
trees_chm<-raster("D:/Classifications/2.2_trees_MR_nobuildings.tif")
trees_chm<-resample(trees_chm, covariates)
covariates2<-covariates *trees_chm

names(covariates2)<-  c("RVI_mrj19", "RVI_mrs21", "ndvi_mrj19", "ndvi_mrs21", "ndre_mrj19", "ndre_mrs21", "chm", "lu_raster", 
                        "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8",
                        "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8")


#########################################################################################

#Load raster
covariates_wue<-stack("D:/Preprocessed/Test_wue/covariates.tif")

# Bands to subset
bands_to_subset <- c( "pan.20JUL23101818.M2AS_R1C1.22EUSI.1785.02.1_1.1", "pan.20JUL23101818.M2AS_R1C1.22EUSI.1785.02.1_2.1", "pan.20JUL23101818.M2AS_R1C1.22EUSI.1785.02.1_3.1",
                      "pan.20JUL23101818.M2AS_R1C1.22EUSI.1785.02.1_4.1", "pan.20JUL23101818.M2AS_R1C1.22EUSI.1785.02.1_5.1", "pan.20JUL23101818.M2AS_R1C1.22EUSI.1785.02.1_6.1",
                      "pan.20JUL23101818.M2AS_R1C1.22EUSI.1785.02.1_7.1", "pan.20JUL23101818.M2AS_R1C1.22EUSI.1785.02.1_8.1", "pan.20JUL04101824.M2AS_R1C1.22EUSI.1785.02.2_1.1",
                      "pan.20JUL04101824.M2AS_R1C1.22EUSI.1785.02.2_2.1", "pan.20JUL04101824.M2AS_R1C1.22EUSI.1785.02.2_3.1", "pan.20JUL04101824.M2AS_R1C1.22EUSI.1785.02.2_4.1",
                      "pan.20JUL04101824.M2AS_R1C1.22EUSI.1785.02.2_5.1", "pan.20JUL04101824.M2AS_R1C1.22EUSI.1785.02.2_6.1", "pan.20JUL04101824.M2AS_R1C1.22EUSI.1785.02.2_7.1",
                      "pan.20JUL04101824.M2AS_R1C1.22EUSI.1785.02.2_8.1", "pan.20JUL23101818.M2AS_R1C1.22EUSI.1785.02.1_7.2", "pan.20JUL04101824.M2AS_R1C1.22EUSI.1785.02.2_7.2",
                      "Z", "Cat_pat"  )

# Subset the raster bands
subset_raster <- subset(covariates_wue, bands_to_subset)

subset_raster <- projectRaster(subset_raster, crs= 32632)

names(subset_raster)<-c("mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8",
                        "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", 
                        "ndvi_mrj19", "ndvi_mrs21", "chm", "lu_raster")

covariates_wue<-subset_raster



########################################################################################
#Prepare the first two classes
TBK_trees_2<-TBK_trees
TBK_trees_2$species<-ifelse(TBK_trees$GATTUNG == 2, "Acer",
                            ifelse(TBK_trees$GATTUNG == 52, "Tilia", "Other"))

TBK_trees_2$genus<-ifelse(TBK_trees_2$species == "Acer", 1,
                          ifelse(TBK_trees_2$species == "Tilia", 2, 3 ))


# Divide the trees according to the land use
TBK_trees_streets_2<-subset(TBK_trees_2, lu_raster == 2)
TBK_trees_parks_2<-subset(TBK_trees_2, lu_raster == 1)
TBK_trees_resi_2<-subset(TBK_trees_2, lu_raster == 3)
TBK_trees_otro_2<-subset(TBK_trees_2, lu_raster == 4)

#------------------------------------
#  Prepare data for Würzburg
#------------------------------------
#Prepare the first two classes
Wue_trees_2<-Wue_trees

Wue_trees_2$genus<-ifelse(Wue_trees$gattung == "Acer", 1,
                          ifelse(Wue_trees$gattung == "Tilia", 2, 3))


# Divide the trees according to the land use
Wue_trees_streets_2<-subset(Wue_trees_2, lu_raster == 2)
Wue_trees_parks_2<-subset(Wue_trees_2, lu_raster == 1)
Wue_trees_resi_2<-subset(Wue_trees_2, lu_raster == 3)
Wue_trees_otro_2<-subset(Wue_trees_2, lu_raster == 4)



# Streets ---------------------------------------------------------------------------------------------------------------------------

#Transform it to data frame
trees_2_st <-as.data.frame(TBK_trees_streets_2)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_2_st$class<-factor(trees_2_st$species)
trees_2_st$genus_f<-factor(trees_2_st$genus)

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

rfmodel_2_st <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                               mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                               mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_2_st, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_2_st

#------------------------------------
#   APLICAR MODELO A WÜRZBURG
#------------------------------------
#Transform it to data frame
wue_trees_2_st <-as.data.frame(Wue_trees_streets_2)[c( "coords.x","coords.y", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
wue_trees_2_st$class<-factor(wue_trees_2_st$genus)
wue_trees_2_st$genus_f<-factor(wue_trees_2_st$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
wue_trees_clean_2_st<-na.omit(wue_trees_2_st)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(wue_trees_clean_2_st$class, SplitRatio=0.7)

#Create the new data sets
wue_train_2_st<-subset(wue_trees_clean_2_st, sample==TRUE)
wue_test_2_st<-subset(wue_trees_clean_2_st, sample==FALSE)


#Prediction
pred_rf_2_st<-predict(covariates_wue, rfmodel_2_st, 
                      filename = "D:/Classifications/Cruzada/rf_2_st_wue_v1.tif",
                      format ="GTiff",
                      overwrite=TRUE)


species_2_st<-wue_test_2_st[c("coords.x","coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_2_st <- st_as_sf(species_2_st, coords = c("coords.x", "coords.y"))
# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_2_st) <- st_crs(crs(covariates))


#extract predictions
pred_2_st <- as.factor(extract(pred_rf_2_st, species_2_st))

obs_2_st <- as.factor(species_2_st$genus)


cm_2_st<-confusionMatrix(pred_2_st, reference = obs_2_st)

cm_2_st
cm_2_st$byClass


# Woodlands ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_2_prk <-as.data.frame(TBK_trees_parks_2)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_2_prk$class<-factor(trees_2_prk$species)
trees_2_prk$genus_f<-factor(trees_2_prk$genus)

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

rfmodel_2_prk <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                                mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                                mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_2_prk, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_2_prk

#------------------------------------
#   APLICAR MODELO A WÜRZBURG
#------------------------------------
#Transform it to data frame
wue_trees_2_prk <-as.data.frame(Wue_trees_parks_2)[c( "coords.x","coords.y", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
wue_trees_2_prk$class<-factor(wue_trees_2_prk$genus)
wue_trees_2_prk$genus_f<-factor(wue_trees_2_prk$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
wue_trees_clean_2_prk<-na.omit(wue_trees_2_prk)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(wue_trees_clean_2_prk$class, SplitRatio=0.7)

#Create the new data sets
wue_train_2_prk<-subset(wue_trees_clean_2_prk, sample==TRUE)
wue_test_2_prk<-subset(wue_trees_clean_2_prk, sample==FALSE)


#Prediction
pred_rf_2_prk<-predict(covariates_wue, rfmodel_2_prk, 
                       filename = "D:/Classifications/Cruzada/rf_2_prk_wue_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_2_prk<-wue_test_2_prk[c("coords.x","coords.y","genus")]
# Convert the data frame to a spatial object (sf)
species_2_prk <- st_as_sf(species_2_prk, coords = c("coords.x", "coords.y"))
# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_2_prk) <- st_crs(crs(covariates))


#extract predictions
pred_2_prk <- as.factor(extract(pred_rf_2_prk, species_2_prk))

obs_2_prk <- as.factor(species_2_prk$genus)


cm_2_prk<-confusionMatrix(pred_2_prk, reference = obs_2_prk)

cm_2_prk

# Residential ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_2_resi <-as.data.frame(TBK_trees_resi_2)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_2_resi$class<-factor(trees_2_resi$species)
trees_2_resi$genus_f<-factor(trees_2_resi$genus)

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

rfmodel_2_resi <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                                 mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                                 mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_2_resi, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_2_resi

#------------------------------------
#   APLICAR MODELO A WÜRZBURG
#------------------------------------
#Transform it to data frame
wue_trees_2_resi <-as.data.frame(Wue_trees_resi_2)[c( "coords.x","coords.y", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
wue_trees_2_resi$class<-factor(wue_trees_2_resi$genus)
wue_trees_2_resi$genus_f<-factor(wue_trees_2_resi$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
wue_trees_clean_2_resi<-na.omit(wue_trees_2_resi)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(wue_trees_clean_2_resi$class, SplitRatio=0.7)

#Create the new data sets
wue_train_2_resi<-subset(wue_trees_clean_2_resi, sample==TRUE)
wue_test_2_resi<-subset(wue_trees_clean_2_resi, sample==FALSE)


#Prediction
pred_rf_2_resi<-predict(covariates_wue, rfmodel_2_resi, 
                        filename = "D:/Classifications/Cruzada/rf_2_resi_wue_v1.tif",
                        format ="GTiff",
                        overwrite=TRUE)



species_2_resi<-wue_test_2_resi[c("coords.x","coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_2_resi <- st_as_sf(species_2_resi, coords = c("coords.x", "coords.y"))
# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_2_resi) <- st_crs(crs(covariates))


#extract predictions
pred_2_resi <- as.factor(extract(pred_rf_2_resi, species_2_resi))

obs_2_resi <- as.factor(species_2_resi$genus)


cm_2_resi<-confusionMatrix(pred_2_resi, reference = obs_2_resi)

cm_2_resi


#Other ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_2_oth <-as.data.frame(TBK_trees_otro_2)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_2_oth$class<-factor(trees_2_oth$species)
trees_2_oth$genus_f<-factor(trees_2_oth$genus)

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

rfmodel_2_oth <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                                mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                                mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_2_oth, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_2_oth

#------------------------------------
#   APLICAR MODELO A WÜRZBURG
#------------------------------------
#Transform it to data frame
wue_trees_2_oth <-as.data.frame(Wue_trees_otro_2)[c( "coords.x","coords.y", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
wue_trees_2_oth$class<-factor(wue_trees_2_oth$genus)
wue_trees_2_oth$genus_f<-factor(wue_trees_2_oth$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
wue_trees_clean_2_oth<-na.omit(wue_trees_2_oth)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(wue_trees_clean_2_oth$class, SplitRatio=0.7)

#Create the new data sets
wue_train_2_oth<-subset(wue_trees_clean_2_oth, sample==TRUE)
wue_test_2_oth<-subset(wue_trees_clean_2_oth, sample==FALSE)


#Prediction
pred_rf_2_oth<-predict(covariates_wue, rfmodel_2_oth, 
                       filename = "D:/Classifications/Cruzada/rf_2_oth_wue_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)


species_2_oth<-wue_test_2_oth[c("coords.x","coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_2_oth <- st_as_sf(species_2_oth, coords = c("coords.x", "coords.y"))
# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_2_oth) <- st_crs(crs(covariates))


#extract predictions
pred_2_oth <- as.factor(extract(pred_rf_2_oth, species_2_oth))

obs_2_oth <- as.factor(species_2_oth$genus)


cm_2_oth<-confusionMatrix(pred_2_oth, reference = obs_2_oth)

cm_2_oth


########################################################################################
#Prepare the first two classes
TBK_trees_3<-TBK_trees
TBK_trees_3$species<-ifelse(TBK_trees$GATTUNG == 2, "Acer",
                            ifelse(TBK_trees$GATTUNG == 52, "Tilia", 
                                   ifelse(TBK_trees$GATTUNG == 8,"Carpinus", "Other")))

TBK_trees_3$genus<-ifelse(TBK_trees_3$species == "Acer", 1,
                          ifelse(TBK_trees_3$species == "Tilia", 2, 
                                 ifelse(TBK_trees_3$species == "Carpinus", 3, 4 )))


# Divide the trees according to the land use
TBK_trees_streets_3<-subset(TBK_trees_3, lu_raster == 2)
TBK_trees_parks_3<-subset(TBK_trees_3, lu_raster == 1)
TBK_trees_resi_3<-subset(TBK_trees_3, lu_raster == 3)
TBK_trees_otro_3<-subset(TBK_trees_3, lu_raster == 4)

#------------------------------------
#  Prepare data for Würzburg
#------------------------------------
#Prepare the first two classes
Wue_trees_3<-Wue_trees

Wue_trees_3$genus<-ifelse(Wue_trees_3$gattung == "Acer", 1,
                          ifelse(Wue_trees_3$gattung == "Tilia", 2, 
                                 ifelse(Wue_trees_3$gattung == "Carpinus", 3, 4 )))


# Divide the trees according to the land use
Wue_trees_streets_3<-subset(Wue_trees_3, lu_raster == 2)
Wue_trees_parks_3<-subset(Wue_trees_3, lu_raster == 1)
Wue_trees_resi_3<-subset(Wue_trees_3, lu_raster == 3)
Wue_trees_otro_3<-subset(Wue_trees_3, lu_raster == 4)


# Streets ---------------------------------------------------------------------------------------------------------------------------

#Transform it to data frame
trees_3_st <-as.data.frame(TBK_trees_streets_3)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_3_st$class<-factor(trees_3_st$species)
trees_3_st$genus_f<-factor(trees_3_st$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_3_st<-na.omit(trees_3_st)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_3_st$class, SplitRatio=0.7)

#Create the new data sets
train_3_st<-subset(trees_clean_3_st, sample==TRUE)
test_3_st<-subset(trees_clean_3_st, sample==FALSE)


#Prepare model

rfmodel_3_st <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                               mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                               mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_3_st, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_3_st

#------------------------------------
#   APLICAR MODELO A WÜRZBURG
#------------------------------------
#Transform it to data frame
wue_trees_3_st <-as.data.frame(Wue_trees_streets_3)[c( "coords.x","coords.y", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
wue_trees_3_st$class<-factor(wue_trees_3_st$genus)
wue_trees_3_st$genus_f<-factor(wue_trees_3_st$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
wue_trees_clean_3_st<-na.omit(wue_trees_3_st)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(wue_trees_clean_3_st$class, SplitRatio=0.7)

#Create the new data sets
wue_train_3_st<-subset(wue_trees_clean_3_st, sample==TRUE)
wue_test_3_st<-subset(wue_trees_clean_3_st, sample==FALSE)


#Prediction
pred_rf_3_st<-predict(covariates_wue, rfmodel_3_st, 
                      filename = "D:/Classifications/Cruzada/rf_3_st_wue_v2.tif",
                      format ="GTiff",
                      overwrite=TRUE)


species_3_st<-wue_test_3_st[c("coords.x","coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_3_st <- st_as_sf(species_3_st, coords = c("coords.x", "coords.y"))
# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_3_st) <- st_crs(crs(covariates))


#extract predictions
pred_3_st <- as.factor(extract(pred_rf_3_st, species_3_st))

obs_3_st <- as.factor(species_3_st$genus)


cm_3_st<-confusionMatrix(pred_3_st, reference = obs_3_st)

cm_3_st
cm_3_st$byClass


# Woodlands ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_3_prk <-as.data.frame(TBK_trees_parks_3)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_3_prk$class<-factor(trees_3_prk$species)
trees_3_prk$genus_f<-factor(trees_3_prk$genus)

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

rfmodel_3_prk <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                                mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                                mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_3_prk, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_3_prk

#------------------------------------
#   APLICAR MODELO A WÜRZBURG
#------------------------------------
#Transform it to data frame
wue_trees_3_prk <-as.data.frame(Wue_trees_parks_3)[c( "coords.x","coords.y", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
wue_trees_3_prk$class<-factor(wue_trees_3_prk$genus)
wue_trees_3_prk$genus_f<-factor(wue_trees_3_prk$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
wue_trees_clean_3_prk<-na.omit(wue_trees_3_prk)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(wue_trees_clean_3_prk$class, SplitRatio=0.7)

#Create the new data sets
wue_train_3_prk<-subset(wue_trees_clean_3_prk, sample==TRUE)
wue_test_3_prk<-subset(wue_trees_clean_3_prk, sample==FALSE)


#Prediction
pred_rf_3_prk<-predict(covariates_wue, rfmodel_3_prk, 
                       filename = "D:/Classifications/Cruzada/rf_3_prk_wue_v2.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_3_prk<-wue_test_3_prk[c("coords.x","coords.y","genus")]
# Convert the data frame to a spatial object (sf)
species_3_prk <- st_as_sf(species_3_prk, coords = c("coords.x", "coords.y"))
# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_3_prk) <- st_crs(crs(covariates))


#extract predictions
pred_3_prk <- as.factor(extract(pred_rf_3_prk, species_3_prk))

obs_3_prk <- as.factor(species_3_prk$genus)


cm_3_prk<-confusionMatrix(pred_3_prk, reference = obs_3_prk)

cm_3_prk

# Residential ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_3_resi <-as.data.frame(TBK_trees_resi_3)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_3_resi$class<-factor(trees_3_resi$species)
trees_3_resi$genus_f<-factor(trees_3_resi$genus)

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

rfmodel_3_resi <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                                 mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                                 mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_3_resi, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_3_resi

#------------------------------------
#   APLICAR MODELO A WÜRZBURG
#------------------------------------
#Transform it to data frame
wue_trees_3_resi <-as.data.frame(Wue_trees_resi_3)[c( "coords.x","coords.y", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
wue_trees_3_resi$class<-factor(wue_trees_3_resi$genus)
wue_trees_3_resi$genus_f<-factor(wue_trees_3_resi$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
wue_trees_clean_3_resi<-na.omit(wue_trees_3_resi)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(wue_trees_clean_3_resi$class, SplitRatio=0.7)

#Create the new data sets
wue_train_3_resi<-subset(wue_trees_clean_3_resi, sample==TRUE)
wue_test_3_resi<-subset(wue_trees_clean_3_resi, sample==FALSE)


#Prediction
pred_rf_3_resi<-predict(covariates_wue, rfmodel_3_resi, 
                        filename = "D:/Classifications/Cruzada/rf_3_resi_wue_v2.tif",
                        format ="GTiff",
                        overwrite=TRUE)



species_3_resi<-wue_test_3_resi[c("coords.x","coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_3_resi <- st_as_sf(species_3_resi, coords = c("coords.x", "coords.y"))
# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_3_resi) <- st_crs(crs(covariates))


#extract predictions
pred_3_resi <- as.factor(extract(pred_rf_3_resi, species_3_resi))

obs_3_resi <- as.factor(species_3_resi$genus)


cm_3_resi<-confusionMatrix(pred_3_resi, reference = obs_3_resi)

cm_3_resi


#Other ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_3_oth <-as.data.frame(TBK_trees_otro_3)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_3_oth$class<-factor(trees_3_oth$species)
trees_3_oth$genus_f<-factor(trees_3_oth$genus)

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

rfmodel_3_oth <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                                mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                                mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_3_oth, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_3_oth

#------------------------------------
#   APLICAR MODELO A WÜRZBURG
#------------------------------------
#Transform it to data frame
wue_trees_3_oth <-as.data.frame(Wue_trees_otro_3)[c( "coords.x","coords.y", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
wue_trees_3_oth$class<-factor(wue_trees_3_oth$genus)
wue_trees_3_oth$genus_f<-factor(wue_trees_3_oth$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
wue_trees_clean_3_oth<-na.omit(wue_trees_3_oth)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(wue_trees_clean_3_oth$class, SplitRatio=0.7)

#Create the new data sets
wue_train_3_oth<-subset(wue_trees_clean_3_oth, sample==TRUE)
wue_test_3_oth<-subset(wue_trees_clean_3_oth, sample==FALSE)


#Prediction
pred_rf_3_oth<-predict(covariates_wue, rfmodel_3_oth, 
                       filename = "D:/Classifications/Cruzada/rf_3_oth_wue_v2.tif",
                       format ="GTiff",
                       overwrite=TRUE)


species_3_oth<-wue_test_3_oth[c("coords.x","coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_3_oth <- st_as_sf(species_3_oth, coords = c("coords.x", "coords.y"))
# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_3_oth) <- st_crs(crs(covariates))


#extract predictions
pred_3_oth <- as.factor(extract(pred_rf_3_oth, species_3_oth))

obs_3_oth <- as.factor(species_3_oth$genus)


cm_3_oth<-confusionMatrix(pred_3_oth, reference = obs_3_oth)

cm_3_oth


########################################################################################
#Prepare the first two classes
TBK_trees_4<-TBK_trees
TBK_trees_4$species<-ifelse(TBK_trees_4$GATTUNG == 2, "Acer",
                            ifelse(TBK_trees_4$GATTUNG == 52, "Tilia", 
                                   ifelse(TBK_trees_4$GATTUNG == 8,"Carpinus", 
                                          ifelse(TBK_trees_4$GATTUNG == 20, "Fraxinus","Other"))))

TBK_trees_4$genus<-ifelse(TBK_trees_4$species == "Acer", 1,
                          ifelse(TBK_trees_4$species == "Tilia", 2, 
                                 ifelse(TBK_trees_4$species == "Carpinus", 3, 
                                        ifelse(TBK_trees_4$species == "Fraxinus", 4, 5 ))))


# Divide the trees according to the land use
TBK_trees_streets_4<-subset(TBK_trees_4, lu_raster == 2)
TBK_trees_parks_4<-subset(TBK_trees_4, lu_raster == 1)
TBK_trees_resi_4<-subset(TBK_trees_4, lu_raster == 3)
TBK_trees_otro_4<-subset(TBK_trees_4, lu_raster == 4)

#------------------------------------
#  Prepare data for Würzburg
#------------------------------------
#Prepare the first two classes
Wue_trees_4<-Wue_trees

Wue_trees_4$genus<-ifelse(Wue_trees_4$gattung == "Acer", 1,
                          ifelse(Wue_trees_4$gattung == "Tilia", 2, 
                                 ifelse(Wue_trees_4$gattung == "Carpinus", 3, 
                                        ifelse(Wue_trees_4$gattung == "Fraxinus", 4, 5 ))))


# Divide the trees according to the land use
Wue_trees_streets_4<-subset(Wue_trees_4, lu_raster == 2)
Wue_trees_parks_4<-subset(Wue_trees_4, lu_raster == 1)
Wue_trees_resi_4<-subset(Wue_trees_4, lu_raster == 3)
Wue_trees_otro_4<-subset(Wue_trees_4, lu_raster == 4)


# Streets ---------------------------------------------------------------------------------------------------------------------------

#Transform it to data frame
trees_4_st <-as.data.frame(TBK_trees_streets_4)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_4_st$class<-factor(trees_4_st$species)
trees_4_st$genus_f<-factor(trees_4_st$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
trees_clean_4_st<-na.omit(trees_4_st)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(trees_clean_4_st$class, SplitRatio=0.7)

#Create the new data sets
train_4_st<-subset(trees_clean_4_st, sample==TRUE)
test_4_st<-subset(trees_clean_4_st, sample==FALSE)


#Prepare model

rfmodel_4_st <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                               mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                               mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_4_st, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_4_st

#------------------------------------
#   APLICAR MODELO A WÜRZBURG
#------------------------------------
#Transform it to data frame
wue_trees_4_st <-as.data.frame(Wue_trees_streets_4)[c( "coords.x","coords.y", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
wue_trees_4_st$class<-factor(wue_trees_4_st$genus)
wue_trees_4_st$genus_f<-factor(wue_trees_4_st$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
wue_trees_clean_4_st<-na.omit(wue_trees_4_st)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(wue_trees_clean_4_st$class, SplitRatio=0.7)

#Create the new data sets
wue_train_4_st<-subset(wue_trees_clean_4_st, sample==TRUE)
wue_test_4_st<-subset(wue_trees_clean_4_st, sample==FALSE)


#Prediction
pred_rf_4_st<-predict(covariates_wue, rfmodel_4_st, 
                      filename = "D:/Classifications/Cruzada/rf_4_st_wue_v1.tif",
                      format ="GTiff",
                      overwrite=TRUE)


species_4_st<-wue_test_4_st[c("coords.x","coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_4_st <- st_as_sf(species_4_st, coords = c("coords.x", "coords.y"))
# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_4_st) <- st_crs(crs(covariates))


#extract predictions
pred_4_st <- as.factor(extract(pred_rf_4_st, species_4_st))

obs_4_st <- as.factor(species_4_st$genus)


cm_4_st<-confusionMatrix(pred_4_st, reference = obs_4_st)

cm_4_st
cm_4_st$byClass


# Woodlands ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_4_prk <-as.data.frame(TBK_trees_parks_4)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_4_prk$class<-factor(trees_4_prk$species)
trees_4_prk$genus_f<-factor(trees_4_prk$genus)

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

rfmodel_4_prk <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                                mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                                mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_4_prk, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_4_prk

#------------------------------------
#   APLICAR MODELO A WÜRZBURG
#------------------------------------
#Transform it to data frame
wue_trees_4_prk <-as.data.frame(Wue_trees_parks_4)[c( "coords.x","coords.y", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
wue_trees_4_prk$class<-factor(wue_trees_4_prk$genus)
wue_trees_4_prk$genus_f<-factor(wue_trees_4_prk$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
wue_trees_clean_4_prk<-na.omit(wue_trees_4_prk)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(wue_trees_clean_4_prk$class, SplitRatio=0.7)

#Create the new data sets
wue_train_4_prk<-subset(wue_trees_clean_4_prk, sample==TRUE)
wue_test_4_prk<-subset(wue_trees_clean_4_prk, sample==FALSE)


#Prediction
pred_rf_4_prk<-predict(covariates_wue, rfmodel_4_prk, 
                       filename = "D:/Classifications/Cruzada/rf_4_prk_wue_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_4_prk<-wue_test_4_prk[c("coords.x","coords.y","genus")]
# Convert the data frame to a spatial object (sf)
species_4_prk <- st_as_sf(species_4_prk, coords = c("coords.x", "coords.y"))
# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_4_prk) <- st_crs(crs(covariates))


#extract predictions
pred_4_prk <- as.factor(extract(pred_rf_4_prk, species_4_prk))

obs_4_prk <- as.factor(species_4_prk$genus)


cm_4_prk<-confusionMatrix(pred_4_prk, reference = obs_4_prk)

cm_4_prk

# Residential ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_4_resi <-as.data.frame(TBK_trees_resi_4)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_4_resi$class<-factor(trees_4_resi$species)
trees_4_resi$genus_f<-factor(trees_4_resi$genus)

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

rfmodel_4_resi <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                                 mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                                 mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_4_resi, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_4_resi

#------------------------------------
#   APLICAR MODELO A WÜRZBURG
#------------------------------------
#Transform it to data frame
wue_trees_4_resi <-as.data.frame(Wue_trees_resi_4)[c( "coords.x","coords.y", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
wue_trees_4_resi$class<-factor(wue_trees_4_resi$genus)
wue_trees_4_resi$genus_f<-factor(wue_trees_4_resi$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
wue_trees_clean_4_resi<-na.omit(wue_trees_4_resi)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(wue_trees_clean_4_resi$class, SplitRatio=0.7)

#Create the new data sets
wue_train_4_resi<-subset(wue_trees_clean_4_resi, sample==TRUE)
wue_test_4_resi<-subset(wue_trees_clean_4_resi, sample==FALSE)


#Prediction
pred_rf_4_resi<-predict(covariates_wue, rfmodel_4_resi, 
                        filename = "D:/Classifications/Cruzada/rf_4_resi_wue_v1.tif",
                        format ="GTiff",
                        overwrite=TRUE)



species_4_resi<-wue_test_4_resi[c("coords.x","coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_4_resi <- st_as_sf(species_4_resi, coords = c("coords.x", "coords.y"))
# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_4_resi) <- st_crs(crs(covariates))


#extract predictions
pred_4_resi <- as.factor(extract(pred_rf_4_resi, species_4_resi))

obs_4_resi <- as.factor(species_4_resi$genus)


cm_4_resi<-confusionMatrix(pred_4_resi, reference = obs_4_resi)

cm_4_resi


#Other ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_4_oth <-as.data.frame(TBK_trees_otro_4)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_4_oth$class<-factor(trees_4_oth$species)
trees_4_oth$genus_f<-factor(trees_4_oth$genus)

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

rfmodel_4_oth <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                                mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                                mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_4_oth, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_4_oth

#------------------------------------
#   APLICAR MODELO A WÜRZBURG
#------------------------------------
#Transform it to data frame
wue_trees_4_oth <-as.data.frame(Wue_trees_otro_4)[c( "coords.x","coords.y", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
wue_trees_4_oth$class<-factor(wue_trees_4_oth$genus)
wue_trees_4_oth$genus_f<-factor(wue_trees_4_oth$genus)

#Make it replicable
set.seed(23)

#Clean the dataset
wue_trees_clean_4_oth<-na.omit(wue_trees_4_oth)

#Divide the 70% of the dataset as training set and remaining 30% as testing set
sample<-sample.split(wue_trees_clean_4_oth$class, SplitRatio=0.7)

#Create the new data sets
wue_train_4_oth<-subset(wue_trees_clean_4_oth, sample==TRUE)
wue_test_4_oth<-subset(wue_trees_clean_4_oth, sample==FALSE)


#Prediction
pred_rf_4_oth<-predict(covariates_wue, rfmodel_4_oth, 
                       filename = "D:/Classifications/Cruzada/rf_4_oth_wue_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)


species_4_oth<-wue_test_4_oth[c("coords.x","coords.y","genus")]

# Convert the data frame to a spatial object (sf)
species_4_oth <- st_as_sf(species_4_oth, coords = c("coords.x", "coords.y"))
# Set the CRS (Coordinate Reference System) for the data frame
st_crs(species_4_oth) <- st_crs(crs(covariates))


#extract predictions
pred_4_oth <- as.factor(extract(pred_rf_4_oth, species_4_oth))

obs_4_oth <- as.factor(species_4_oth$genus)


cm_4_oth<-confusionMatrix(pred_4_oth, reference = obs_4_oth)

cm_4_oth

