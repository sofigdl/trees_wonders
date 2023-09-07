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



#TBK_trees$species<-ifelse(TBK_trees$GATTUNG == 2, "Acer",
 #                         ifelse(TBK_trees$GATTUNG == 52, "Tilia",
 #                                ifelse(TBK_trees$GATTUNG == 8,"Carpinus",
  #                                      ifelse(TBK_trees$GATTUNG == 20, "Fraxinus",
   #                                            ifelse(TBK_trees$GATTUNG == 46, "Robinia",
    #                                                  ifelse(TBK_trees$GATTUNG == 42, "Prunus",
     #                                                        ifelse(TBK_trees$GATTUNG == 3, "Aesculus",
      #                                                              ifelse(TBK_trees$GATTUNG == 19, "Fagus",
       #                                                                    ifelse(TBK_trees$GATTUNG == 41, "Populus",
        #                                                                          ifelse(TBK_trees$GATTUNG == 45, "Quercus",
         #                                                                                ifelse(TBK_trees$GATTUNG == 40,"Platanus",
          #                                                                                      ifelse(TBK_trees$GATTUNG == 54,"Ulmus",
           #                                                                                            ifelse(TBK_trees$GATTUNG == 47,"Salix",
            #                                                                                                  ifelse(TBK_trees$GATTUNG == 49,"Sorbus",
             #                                                                                                        ifelse(TBK_trees$GATTUNG == 50,"Taxus",
              #                                                                                                              ifelse(TBK_trees$GATTUNG == 15,"Corylus",
               #                                                                                                                    ifelse(TBK_trees$GATTUNG == 32,"Malus",
                #                                                                                                                          ifelse(TBK_trees$GATTUNG == 6,"Betula",
                 #                                                                                                                                ifelse(TBK_trees$GATTUNG == 39,"Pinus",
                  #                                                                                                                                      ifelse(TBK_trees$GATTUNG == 22,"Gleditsia",
                   #                                                                                                                                            ifelse(TBK_trees$GATTUNG == 57,"Picea",
                    #                                                                                                                                                  ifelse(TBK_trees$GATTUNG == 14,"Cornus", "Other"))))))))))))))))))))))
#
#
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

# Bands to subset
bands_to_subset <- c("mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", 
                     "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", 
                     "ndvi_mrj19", "ndvi_mrs21", "chm")

# Subset the raster bands
subset_raster <- subset(covariates, bands_to_subset)

writeRaster(subset_raster, "D:/Preprocessed/New_preprocessing/covariates_new_masked.tif")

#########################################################################################

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

190

#Prepare model

rfmodel_2_st <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                          mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                          mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_2_st, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_2_st

#varImpPlot(rfmodel_2_st, scale = FALSE)#, type=1, scale=TRUE)

#Prediction
pred_rf_2_st<-predict(covariates2, rfmodel_2_st, 
                          filename = "D:/Classifications/Escalonada/RF/rf_2_st_v3.tif",
                          format ="GTiff",
                          overwrite=TRUE)



species_2_st<-test_2_st[c("x","y","genus")]
coordinates(species_2_st)<-~x+y
projection(species_2_st)<- projection(TBK_trees)


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


#Prediction
pred_rf_2_prk<-predict(covariates2, rfmodel_2_prk, 
                      filename = "D:/Classifications/Escalonada/RF/rf_2_prk_v1.tif",
                      format ="GTiff",
                      overwrite=TRUE)



species_2_prk<-test_2_prk[c("x","y","genus")]
coordinates(species_2_prk)<-~x+y
projection(species_2_prk)<- projection(TBK_trees)


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


#Prediction
pred_rf_2_resi<-predict(covariates2, rfmodel_2_resi, 
                       filename = "D:/Classifications/Escalonada/RF/rf_2_resi_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_2_resi<-test_2_resi[c("x","y","genus")]
coordinates(species_2_resi)<-~x+y
projection(species_2_resi)<- projection(TBK_trees)


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


#Prediction
pred_rf_2_oth<-predict(covariates2, rfmodel_2_oth, 
                        filename = "D:/Classifications/Escalonada/RF/rf_2_oth_v3.tif",
                        format ="GTiff",
                        overwrite=TRUE)



species_2_oth<-test_2_oth[c("x","y","genus")]
coordinates(species_2_oth)<-~x+y
projection(species_2_oth)<- projection(TBK_trees)


#extract predictions
pred_2_oth <- as.factor(extract(pred_rf_2_oth, species_2_oth))

obs_2_oth <- as.factor(species_2_oth$genus)


cm_2_oth<-confusionMatrix(pred_2_oth, reference = obs_2_oth)

cm_2_oth


######################################################################################

#Prepare the first three classes
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

#Check the datasets
table(train_3_st$genus)
table(test_3_st$genus)

#Prepare model

rfmodel_3_st <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                               mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                               mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_3_st, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_3_st

#varImpPlot(rfmodel_3_st, scale = FALSE)#, type=1, scale=TRUE)

#Prediction
pred_rf_3_st<-predict(covariates2, rfmodel_3_st, 
                      filename = "D:/Classifications/Escalonada/RF/rf_3_st_v1.tif",
                      format ="GTiff",
                      overwrite=TRUE)



species_3_st<-test_3_st[c("x","y","genus")]
coordinates(species_3_st)<-~x+y
projection(species_3_st)<- projection(TBK_trees)


#extract predictions
pred_3_st <- as.factor(extract(pred_rf_3_st, species_3_st))

obs_3_st <- as.factor(species_3_st$genus)


cm_3_st<-confusionMatrix(pred_3_st, reference = obs_3_st)

cm_3_st



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


#Prediction
pred_rf_3_prk<-predict(covariates2, rfmodel_3_prk, 
                       filename = "D:/Classifications/Escalonada/RF/rf_3_prk_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_3_prk<-test_3_prk[c("x","y","genus")]
coordinates(species_3_prk)<-~x+y
projection(species_3_prk)<- projection(TBK_trees)


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


#Prediction
pred_rf_3_resi<-predict(covariates2, rfmodel_3_resi, 
                        filename = "D:/Classifications/Escalonada/RF/rf_3_resi_v1.tif",
                        format ="GTiff",
                        overwrite=TRUE)



species_3_resi<-test_3_resi[c("x","y","genus")]
coordinates(species_3_resi)<-~x+y
projection(species_3_resi)<- projection(TBK_trees)


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


#Prediction
pred_rf_3_oth<-predict(covariates2, rfmodel_3_oth, 
                       filename = "D:/Classifications/Escalonada/RF/rf_3_oth_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_3_oth<-test_3_oth[c("x","y","genus")]
coordinates(species_3_oth)<-~x+y
projection(species_3_oth)<- projection(TBK_trees)


#extract predictions
pred_3_oth <- as.factor(extract(pred_rf_3_oth, species_3_oth))

obs_3_oth <- as.factor(species_3_oth$genus)


cm_3_oth<-confusionMatrix(pred_3_oth, reference = obs_3_oth)

cm_3_oth


######################################################################################

#Prepare the first two classes
TBK_trees_4<-TBK_trees
TBK_trees_4$species<-ifelse(TBK_trees$GATTUNG == 2, "Acer",
                            ifelse(TBK_trees$GATTUNG == 52, "Tilia",
                                   ifelse(TBK_trees$GATTUNG == 8,"Carpinus", 
                                          ifelse(TBK_trees$GATTUNG == 20, "Fraxinus", "Other"))))


TBK_trees_4$genus<-ifelse(TBK_trees_4$species == "Acer", 1,
                          ifelse(TBK_trees_4$species == "Tilia", 2, 
                                 ifelse(TBK_trees_4$species == "Carpinus", 3,
                                        ifelse(TBK_trees_4$species == "Fraxinus", 4, 5 ))))


# Divide the trees according to the land use
TBK_trees_streets_4<-subset(TBK_trees_4, lu_raster == 2)
TBK_trees_parks_4<-subset(TBK_trees_4, lu_raster == 1)
TBK_trees_resi_4<-subset(TBK_trees_4, lu_raster == 3)
TBK_trees_otro_4<-subset(TBK_trees_4, lu_raster == 4)


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

#Check the datasets
table(train_4_st$genus)
table(test_4_st$genus)

#Prepare model

rfmodel_4_st <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                               mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                               mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_4_st, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_4_st

#varImpPlot(rfmodel_4_st, scale = FALSE)#, type=1, scale=TRUE)

#Prediction
pred_rf_4_st<-predict(covariates2, rfmodel_4_st, 
                      filename = "D:/Classifications/Escalonada/RF/rf_4_st_v1.tif",
                      format ="GTiff",
                      overwrite=TRUE)



species_4_st<-test_4_st[c("x","y","genus")]
coordinates(species_4_st)<-~x+y
projection(species_4_st)<- projection(TBK_trees)


#extract predictions
pred_4_st <- as.factor(extract(pred_rf_4_st, species_4_st))

obs_4_st <- as.factor(species_4_st$genus)


cm_4_st<-confusionMatrix(pred_4_st, reference = obs_4_st)

cm_4_st


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


#Prediction
pred_rf_4_prk<-predict(covariates2, rfmodel_4_prk, 
                       filename = "D:/Classifications/Escalonada/RF/rf_4_prk_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_4_prk<-test_4_prk[c("x","y","genus")]
coordinates(species_4_prk)<-~x+y
projection(species_4_prk)<- projection(TBK_trees)


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


#Prediction
pred_rf_4_resi<-predict(covariates2, rfmodel_4_resi, 
                        filename = "D:/Classifications/Escalonada/RF/rf_4_resi_v1.tif",
                        format ="GTiff",
                        overwrite=TRUE)



species_4_resi<-test_4_resi[c("x","y","genus")]
coordinates(species_4_resi)<-~x+y
projection(species_4_resi)<- projection(TBK_trees)


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


#Prediction
pred_rf_4_oth<-predict(covariates2, rfmodel_4_oth, 
                       filename = "D:/Classifications/Escalonada/RF/rf_4_oth_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_4_oth<-test_4_oth[c("x","y","genus")]
coordinates(species_4_oth)<-~x+y
projection(species_4_oth)<- projection(TBK_trees)


#extract predictions
pred_4_oth <- as.factor(extract(pred_rf_4_oth, species_4_oth))

obs_4_oth <- as.factor(species_4_oth$genus)


cm_4_oth<-confusionMatrix(pred_4_oth, reference = obs_4_oth)

cm_4_oth


######################################################################################

#Prepare the first two classes
TBK_trees_5<-TBK_trees
TBK_trees_5$species<-ifelse(TBK_trees_5$GATTUNG == 2, "Acer",
                            ifelse(TBK_trees_5$GATTUNG == 52, "Tilia",
                                   ifelse(TBK_trees_5$GATTUNG == 8,"Carpinus", 
                                          ifelse(TBK_trees_5$GATTUNG == 20, "Fraxinus",
                                                 ifelse(TBK_trees_5$GATTUNG == 46, "Robinia","Other")))))


TBK_trees_5$genus<-ifelse(TBK_trees_5$species == "Acer", 1,
                          ifelse(TBK_trees_5$species == "Tilia", 2, 
                                 ifelse(TBK_trees_5$species == "Carpinus", 3,
                                        ifelse(TBK_trees_5$species == "Fraxinus", 4,
                                               ifelse(TBK_trees_5$species == "Robinia", 5, 6 )))))


# Divide the trees according to the land use
TBK_trees_streets_5<-subset(TBK_trees_5, lu_raster == 2)
TBK_trees_parks_5<-subset(TBK_trees_5, lu_raster == 1)
TBK_trees_resi_5<-subset(TBK_trees_5, lu_raster == 3)
TBK_trees_otro_5<-subset(TBK_trees_5, lu_raster == 4)


# Streets ---------------------------------------------------------------------------------------------------------------------------

#Transform it to data frame
trees_5_st <-as.data.frame(TBK_trees_streets_5)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_5_st$class<-factor(trees_5_st$species)
trees_5_st$genus_f<-factor(trees_5_st$genus)

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

rfmodel_5_st <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                               mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                               mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_5_st, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_5_st

#varImpPlot(rfmodel_5_st, scale = FALSE)#, type=1, scale=TRUE)

#Prediction
pred_rf_5_st<-predict(covariates2, rfmodel_5_st, 
                      filename = "D:/Classifications/Escalonada/RF/rf_5_st_v2.tif",
                      format ="GTiff",
                      overwrite=TRUE)



species_5_st<-test_5_st[c("x","y","genus")]
coordinates(species_5_st)<-~x+y
projection(species_5_st)<- projection(TBK_trees)


#extract predictions
pred_5_st <- as.factor(extract(pred_rf_5_st, species_5_st))

obs_5_st <- as.factor(species_5_st$genus)


cm_5_st<-confusionMatrix(pred_5_st, reference = obs_5_st)

cm_5_st


# Woodlands ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_5_prk <-as.data.frame(TBK_trees_parks_5)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_5_prk$class<-factor(trees_5_prk$species)
trees_5_prk$genus_f<-factor(trees_5_prk$genus)

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

rfmodel_5_prk <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                                mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                                mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_5_prk, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_5_prk


#Prediction
pred_rf_5_prk<-predict(covariates2, rfmodel_5_prk, 
                       filename = "D:/Classifications/Escalonada/RF/rf_5_prk_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_5_prk<-test_5_prk[c("x","y","genus")]
coordinates(species_5_prk)<-~x+y
projection(species_5_prk)<- projection(TBK_trees)


#extract predictions
pred_5_prk <- as.factor(extract(pred_rf_5_prk, species_5_prk))

obs_5_prk <- as.factor(species_5_prk$genus)


cm_5_prk<-confusionMatrix(pred_5_prk, reference = obs_5_prk)

cm_5_prk

# Residential ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_5_resi <-as.data.frame(TBK_trees_resi_5)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_5_resi$class<-factor(trees_5_resi$species)
trees_5_resi$genus_f<-factor(trees_5_resi$genus)

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

rfmodel_5_resi <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                                 mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                                 mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_5_resi, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_5_resi


#Prediction
pred_rf_5_resi<-predict(covariates2, rfmodel_5_resi, 
                        filename = "D:/Classifications/Escalonada/RF/rf_5_resi_v1.tif",
                        format ="GTiff",
                        overwrite=TRUE)



species_5_resi<-test_5_resi[c("x","y","genus")]
coordinates(species_5_resi)<-~x+y
projection(species_5_resi)<- projection(TBK_trees)


#extract predictions
pred_5_resi <- as.factor(extract(pred_rf_5_resi, species_5_resi))

obs_5_resi <- as.factor(species_5_resi$genus)


cm_5_resi<-confusionMatrix(pred_5_resi, reference = obs_5_resi)

cm_5_resi


#Other ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_5_oth <-as.data.frame(TBK_trees_otro_5)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_5_oth$class<-factor(trees_5_oth$species)
trees_5_oth$genus_f<-factor(trees_5_oth$genus)

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

rfmodel_5_oth <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                                mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                                mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_5_oth, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_5_oth


#Prediction
pred_rf_5_oth<-predict(covariates2, rfmodel_5_oth, 
                       filename = "D:/Classifications/Escalonada/RF/rf_5_oth_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_5_oth<-test_5_oth[c("x","y","genus")]
coordinates(species_5_oth)<-~x+y
projection(species_5_oth)<- projection(TBK_trees)


#extract predictions
pred_5_oth <- as.factor(extract(pred_rf_5_oth, species_5_oth))

obs_5_oth <- as.factor(species_5_oth$genus)


cm_5_oth<-confusionMatrix(pred_5_oth, reference = obs_5_oth)

cm_5_oth

######################################################################################

#Prepare the first two classes
TBK_trees_6<-TBK_trees
TBK_trees_6$species<-ifelse(TBK_trees_6$GATTUNG == 2, "Acer",
                            ifelse(TBK_trees_6$GATTUNG == 52, "Tilia",
                                   ifelse(TBK_trees_6$GATTUNG == 8,"Carpinus", 
                                          ifelse(TBK_trees_6$GATTUNG == 20, "Fraxinus",
                                                 ifelse(TBK_trees_6$GATTUNG == 46, "Robinia",
                                                        ifelse(TBK_trees_6$GATTUNG == 42, "Prunus","Other"))))))


TBK_trees_6$genus<-ifelse(TBK_trees_6$species == "Acer", 1,
                          ifelse(TBK_trees_6$species == "Tilia", 2, 
                                 ifelse(TBK_trees_6$species == "Carpinus", 3,
                                        ifelse(TBK_trees_6$species == "Fraxinus", 4,
                                               ifelse(TBK_trees_6$species == "Robinia", 5,
                                                      ifelse(TBK_trees_6$species == "Prunus", 6, 7 ))))))


# Divide the trees according to the land use
TBK_trees_streets_6<-subset(TBK_trees_6, lu_raster == 2)
TBK_trees_parks_6<-subset(TBK_trees_6, lu_raster == 1)
TBK_trees_resi_6<-subset(TBK_trees_6, lu_raster == 3)
TBK_trees_otro_6<-subset(TBK_trees_6, lu_raster == 4)


# Streets ---------------------------------------------------------------------------------------------------------------------------

#Transform it to data frame
trees_6_st <-as.data.frame(TBK_trees_streets_6)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_6_st$class<-factor(trees_6_st$species)
trees_6_st$genus_f<-factor(trees_6_st$genus)

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

rfmodel_6_st <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                               mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                               mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_6_st, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_6_st

#varImpPlot(rfmodel_6_st, scale = FALSE)#, type=1, scale=TRUE)

#Prediction
pred_rf_6_st<-predict(covariates2, rfmodel_6_st, 
                      filename = "D:/Classifications/Escalonada/RF/rf_6_st_v2.tif",
                      format ="GTiff",
                      overwrite=TRUE)



species_6_st<-test_6_st[c("x","y","genus")]
coordinates(species_6_st)<-~x+y
projection(species_6_st)<- projection(TBK_trees)


#extract predictions
pred_6_st <- as.factor(extract(pred_rf_6_st, species_6_st))

obs_6_st <- as.factor(species_6_st$genus)


cm_6_st<-confusionMatrix(pred_6_st, reference = obs_6_st)

cm_6_st


# Woodlands ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_6_prk <-as.data.frame(TBK_trees_parks_6)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_6_prk$class<-factor(trees_6_prk$species)
trees_6_prk$genus_f<-factor(trees_6_prk$genus)

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

rfmodel_6_prk <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                                mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                                mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_6_prk, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_6_prk


#Prediction
pred_rf_6_prk<-predict(covariates2, rfmodel_6_prk, 
                       filename = "D:/Classifications/Escalonada/RF/rf_6_prk_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_6_prk<-test_6_prk[c("x","y","genus")]
coordinates(species_6_prk)<-~x+y
projection(species_6_prk)<- projection(TBK_trees)


#extract predictions
pred_6_prk <- as.factor(extract(pred_rf_6_prk, species_6_prk))

obs_6_prk <- as.factor(species_6_prk$genus)


cm_6_prk<-confusionMatrix(pred_6_prk, reference = obs_6_prk)

cm_6_prk

# Residential ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_6_resi <-as.data.frame(TBK_trees_resi_6)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_6_resi$class<-factor(trees_6_resi$species)
trees_6_resi$genus_f<-factor(trees_6_resi$genus)

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

rfmodel_6_resi <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                                 mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                                 mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_6_resi, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_6_resi


#Prediction
pred_rf_6_resi<-predict(covariates2, rfmodel_6_resi, 
                        filename = "D:/Classifications/Escalonada/RF/rf_6_resi_v1.tif",
                        format ="GTiff",
                        overwrite=TRUE)



species_6_resi<-test_6_resi[c("x","y","genus")]
coordinates(species_6_resi)<-~x+y
projection(species_6_resi)<- projection(TBK_trees)


#extract predictions
pred_6_resi <- as.factor(extract(pred_rf_6_resi, species_6_resi))

obs_6_resi <- as.factor(species_6_resi$genus)


cm_6_resi<-confusionMatrix(pred_6_resi, reference = obs_6_resi)

cm_6_resi


#Other ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_6_oth <-as.data.frame(TBK_trees_otro_6)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_6_oth$class<-factor(trees_6_oth$species)
trees_6_oth$genus_f<-factor(trees_6_oth$genus)

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

rfmodel_6_oth <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                                mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                                mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_6_oth, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_6_oth


#Prediction
pred_rf_6_oth<-predict(covariates2, rfmodel_6_oth, 
                       filename = "D:/Classifications/Escalonada/RF/rf_6_oth_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_6_oth<-test_6_oth[c("x","y","genus")]
coordinates(species_6_oth)<-~x+y
projection(species_6_oth)<- projection(TBK_trees)


#extract predictions
pred_6_oth <- as.factor(extract(pred_rf_6_oth, species_6_oth))

obs_6_oth <- as.factor(species_6_oth$genus)


cm_6_oth<-confusionMatrix(pred_6_oth, reference = obs_6_oth)

cm_6_oth


######################################################################################

#Prepare the first two classes
TBK_trees_7<-TBK_trees
TBK_trees_7$species<-ifelse(TBK_trees_7$GATTUNG == 2, "Acer",
                            ifelse(TBK_trees_7$GATTUNG == 52, "Tilia",
                                   ifelse(TBK_trees_7$GATTUNG == 8,"Carpinus", 
                                          ifelse(TBK_trees_7$GATTUNG == 20, "Fraxinus",
                                                 ifelse(TBK_trees_7$GATTUNG == 46, "Robinia",
                                                        ifelse(TBK_trees_7$GATTUNG == 42, "Prunus", 
                                                               ifelse(TBK_trees_7$GATTUNG == 3, "Aesculus","Other")))))))


TBK_trees_7$genus<-ifelse(TBK_trees_7$species == "Acer", 1,
                          ifelse(TBK_trees_7$species == "Tilia", 2, 
                                 ifelse(TBK_trees_7$species == "Carpinus", 3,
                                        ifelse(TBK_trees_7$species == "Fraxinus", 4,
                                               ifelse(TBK_trees_7$species == "Robinia", 5,
                                                      ifelse(TBK_trees_7$species == "Prunus", 6, 
                                                             ifelse(TBK_trees_7$species == "Aesculus",7 , 8)))))))


# Divide the trees according to the land use
TBK_trees_streets_7<-subset(TBK_trees_7, lu_raster == 2)
TBK_trees_parks_7<-subset(TBK_trees_7, lu_raster == 1)
TBK_trees_resi_7<-subset(TBK_trees_7, lu_raster == 3)
TBK_trees_otro_7<-subset(TBK_trees_7, lu_raster == 4)


# Streets ---------------------------------------------------------------------------------------------------------------------------

#Transform it to data frame
trees_7_st <-as.data.frame(TBK_trees_streets_7)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_7_st$class<-factor(trees_7_st$species)
trees_7_st$genus_f<-factor(trees_7_st$genus)

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

rfmodel_7_st <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                               mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                               mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_7_st, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_7_st

#varImpPlot(rfmodel_7_st, scale = FALSE)#, type=1, scale=TRUE)

#Prediction
pred_rf_7_st<-predict(covariates2, rfmodel_7_st, 
                      filename = "D:/Classifications/Escalonada/RF/rf_7_st_v2.tif",
                      format ="GTiff",
                      overwrite=TRUE)



species_7_st<-test_7_st[c("x","y","genus")]
coordinates(species_7_st)<-~x+y
projection(species_7_st)<- projection(TBK_trees)


#extract predictions
pred_7_st <- as.factor(extract(pred_rf_7_st, species_7_st))

obs_7_st <- as.factor(species_7_st$genus)


cm_7_st<-confusionMatrix(pred_7_st, reference = obs_7_st)

cm_7_st


# Woodlands ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_7_prk <-as.data.frame(TBK_trees_parks_7)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_7_prk$class<-factor(trees_7_prk$species)
trees_7_prk$genus_f<-factor(trees_7_prk$genus)

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

rfmodel_7_prk <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                                mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                                mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_7_prk, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_7_prk


#Prediction
pred_rf_7_prk<-predict(covariates2, rfmodel_7_prk, 
                       filename = "D:/Classifications/Escalonada/RF/rf_7_prk_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_7_prk<-test_7_prk[c("x","y","genus")]
coordinates(species_7_prk)<-~x+y
projection(species_7_prk)<- projection(TBK_trees)


#extract predictions
pred_7_prk <- as.factor(extract(pred_rf_7_prk, species_7_prk))

obs_7_prk <- as.factor(species_7_prk$genus)


cm_7_prk<-confusionMatrix(pred_7_prk, reference = obs_7_prk)

cm_7_prk

# Residential ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_7_resi <-as.data.frame(TBK_trees_resi_7)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_7_resi$class<-factor(trees_7_resi$species)
trees_7_resi$genus_f<-factor(trees_7_resi$genus)

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

rfmodel_7_resi <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                                 mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                                 mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_7_resi, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_7_resi


#Prediction
pred_rf_7_resi<-predict(covariates2, rfmodel_7_resi, 
                        filename = "D:/Classifications/Escalonada/RF/rf_7_resi_v1.tif",
                        format ="GTiff",
                        overwrite=TRUE)



species_7_resi<-test_7_resi[c("x","y","genus")]
coordinates(species_7_resi)<-~x+y
projection(species_7_resi)<- projection(TBK_trees)


#extract predictions
pred_7_resi <- as.factor(extract(pred_rf_7_resi, species_7_resi))

obs_7_resi <- as.factor(species_7_resi$genus)


cm_7_resi<-confusionMatrix(pred_7_resi, reference = obs_7_resi)

cm_7_resi


#Other ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_7_oth <-as.data.frame(TBK_trees_otro_7)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_7_oth$class<-factor(trees_7_oth$species)
trees_7_oth$genus_f<-factor(trees_7_oth$genus)

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

rfmodel_7_oth <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                                mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                                mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_7_oth, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_7_oth


#Prediction
pred_rf_7_oth<-predict(covariates2, rfmodel_7_oth, 
                       filename = "D:/Classifications/Escalonada/RF/rf_7_oth_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_7_oth<-test_7_oth[c("x","y","genus")]
coordinates(species_7_oth)<-~x+y
projection(species_7_oth)<- projection(TBK_trees)


#extract predictions
pred_7_oth <- as.factor(extract(pred_rf_7_oth, species_7_oth))

obs_7_oth <- as.factor(species_7_oth$genus)


cm_7_oth<-confusionMatrix(pred_7_oth, reference = obs_7_oth)

cm_7_oth


######################################################################################

#Prepare the first two classes
TBK_trees_8<-TBK_trees
TBK_trees_8$species<-ifelse(TBK_trees_8$GATTUNG == 2, "Acer",
                            ifelse(TBK_trees_8$GATTUNG == 52, "Tilia",
                                   ifelse(TBK_trees_8$GATTUNG == 8,"Carpinus", 
                                          ifelse(TBK_trees_8$GATTUNG == 20, "Fraxinus",
                                                 ifelse(TBK_trees_8$GATTUNG == 46, "Robinia",
                                                        ifelse(TBK_trees_8$GATTUNG == 42, "Prunus", 
                                                               ifelse(TBK_trees_8$GATTUNG == 3, "Aesculus",
                                                                      ifelse(TBK_trees_8$GATTUNG == 19, "Fagus","Other"))))))))


TBK_trees_8$genus<-ifelse(TBK_trees_8$species == "Acer", 1,
                          ifelse(TBK_trees_8$species == "Tilia", 2, 
                                 ifelse(TBK_trees_8$species == "Carpinus", 3,
                                        ifelse(TBK_trees_8$species == "Fraxinus", 4,
                                               ifelse(TBK_trees_8$species == "Robinia", 5,
                                                      ifelse(TBK_trees_8$species == "Prunus", 6, 
                                                             ifelse(TBK_trees_8$species == "Aesculus",7 ,
                                                                    ifelse(TBK_trees_8$species == "Fagus", 8, 9))))))))


# Divide the trees according to the land use
TBK_trees_streets_8<-subset(TBK_trees_8, lu_raster == 2)
TBK_trees_parks_8<-subset(TBK_trees_8, lu_raster == 1)
TBK_trees_resi_8<-subset(TBK_trees_8, lu_raster == 3)
TBK_trees_otro_8<-subset(TBK_trees_8, lu_raster == 4)


# Streets ---------------------------------------------------------------------------------------------------------------------------

#Transform it to data frame
trees_8_st <-as.data.frame(TBK_trees_streets_8)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_8_st$class<-factor(trees_8_st$species)
trees_8_st$genus_f<-factor(trees_8_st$genus)

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

rfmodel_8_st <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                               mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                               mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_8_st, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_8_st

#varImpPlot(rfmodel_8_st, scale = FALSE)#, type=1, scale=TRUE)

#Prediction
pred_rf_8_st<-predict(covariates2, rfmodel_8_st, 
                      filename = "D:/Classifications/Escalonada/RF/rf_8_st_v1.tif",
                      format ="GTiff",
                      overwrite=TRUE)



species_8_st<-test_8_st[c("x","y","genus")]
coordinates(species_8_st)<-~x+y
projection(species_8_st)<- projection(TBK_trees)


#extract predictions
pred_8_st <- as.factor(extract(pred_rf_8_st, species_8_st))

obs_8_st <- as.factor(species_8_st$genus)


cm_8_st<-confusionMatrix(pred_8_st, reference = obs_8_st)

cm_8_st


# Woodlands ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_8_prk <-as.data.frame(TBK_trees_parks_8)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_8_prk$class<-factor(trees_8_prk$species)
trees_8_prk$genus_f<-factor(trees_8_prk$genus)

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

rfmodel_8_prk <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                                mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                                mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_8_prk, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_8_prk


#Prediction
pred_rf_8_prk<-predict(covariates2, rfmodel_8_prk, 
                       filename = "D:/Classifications/Escalonada/RF/rf_8_prk_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_8_prk<-test_8_prk[c("x","y","genus")]
coordinates(species_8_prk)<-~x+y
projection(species_8_prk)<- projection(TBK_trees)


#extract predictions
pred_8_prk <- as.factor(extract(pred_rf_8_prk, species_8_prk))

obs_8_prk <- as.factor(species_8_prk$genus)


cm_8_prk<-confusionMatrix(pred_8_prk, reference = obs_8_prk)

cm_8_prk

# Residential ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_8_resi <-as.data.frame(TBK_trees_resi_8)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_8_resi$class<-factor(trees_8_resi$species)
trees_8_resi$genus_f<-factor(trees_8_resi$genus)

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

rfmodel_8_resi <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                                 mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                                 mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_8_resi, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_8_resi


#Prediction
pred_rf_8_resi<-predict(covariates2, rfmodel_8_resi, 
                        filename = "D:/Classifications/Escalonada/RF/rf_8_resi_v1.tif",
                        format ="GTiff",
                        overwrite=TRUE)



species_8_resi<-test_8_resi[c("x","y","genus")]
coordinates(species_8_resi)<-~x+y
projection(species_8_resi)<- projection(TBK_trees)


#extract predictions
pred_8_resi <- as.factor(extract(pred_rf_8_resi, species_8_resi))

obs_8_resi <- as.factor(species_8_resi$genus)


cm_8_resi<-confusionMatrix(pred_8_resi, reference = obs_8_resi)

cm_8_resi


#Other ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_8_oth <-as.data.frame(TBK_trees_otro_8)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_8_oth$class<-factor(trees_8_oth$species)
trees_8_oth$genus_f<-factor(trees_8_oth$genus)

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

rfmodel_8_oth <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                                mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                                mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_8_oth, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_8_oth


#Prediction
pred_rf_8_oth<-predict(covariates2, rfmodel_8_oth, 
                       filename = "D:/Classifications/Escalonada/RF/rf_8_oth_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_8_oth<-test_8_oth[c("x","y","genus")]
coordinates(species_8_oth)<-~x+y
projection(species_8_oth)<- projection(TBK_trees)


#extract predictions
pred_8_oth <- as.factor(extract(pred_rf_8_oth, species_8_oth))

obs_8_oth <- as.factor(species_8_oth$genus)


cm_8_oth<-confusionMatrix(pred_8_oth, reference = obs_8_oth)

cm_8_oth


######################################################################################

#Prepare the first two classes
TBK_trees_9<-TBK_trees
TBK_trees_9$species<-ifelse(TBK_trees_9$GATTUNG == 2, "Acer",
                            ifelse(TBK_trees_9$GATTUNG == 52, "Tilia",
                                   ifelse(TBK_trees_9$GATTUNG == 8,"Carpinus", 
                                          ifelse(TBK_trees_9$GATTUNG == 20, "Fraxinus",
                                                 ifelse(TBK_trees_9$GATTUNG == 46, "Robinia",
                                                        ifelse(TBK_trees_9$GATTUNG == 42, "Prunus", 
                                                               ifelse(TBK_trees_9$GATTUNG == 3, "Aesculus",
                                                                      ifelse(TBK_trees_9$GATTUNG == 19, "Fagus",
                                                                             ifelse(TBK_trees_9$GATTUNG == 41, "Populus","Other")))))))))


TBK_trees_9$genus<-ifelse(TBK_trees_9$species == "Acer", 1,
                          ifelse(TBK_trees_9$species == "Tilia", 2, 
                                 ifelse(TBK_trees_9$species == "Carpinus", 3,
                                        ifelse(TBK_trees_9$species == "Fraxinus", 4,
                                               ifelse(TBK_trees_9$species == "Robinia", 5,
                                                      ifelse(TBK_trees_9$species == "Prunus", 6, 
                                                             ifelse(TBK_trees_9$species == "Aesculus",7 ,
                                                                    ifelse(TBK_trees_9$species == "Fagus", 8,
                                                                           ifelse(TBK_trees_9$species == "Populus", 9, 10)))))))))


# Divide the trees according to the land use
TBK_trees_streets_9<-subset(TBK_trees_9, lu_raster == 2)
TBK_trees_parks_9<-subset(TBK_trees_9, lu_raster == 1)
TBK_trees_resi_9<-subset(TBK_trees_9, lu_raster == 3)
TBK_trees_otro_9<-subset(TBK_trees_9, lu_raster == 4)


# Streets ---------------------------------------------------------------------------------------------------------------------------

#Transform it to data frame
trees_9_st <-as.data.frame(TBK_trees_streets_9)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_9_st$class<-factor(trees_9_st$species)
trees_9_st$genus_f<-factor(trees_9_st$genus)

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

rfmodel_9_st <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                               mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                               mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_9_st, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_9_st

#varImpPlot(rfmodel_9_st, scale = FALSE)#, type=1, scale=TRUE)

#Prediction
pred_rf_9_st<-predict(covariates2, rfmodel_9_st, 
                      filename = "D:/Classifications/Escalonada/RF/rf_9_st_v1.tif",
                      format ="GTiff",
                      overwrite=TRUE)



species_9_st<-test_9_st[c("x","y","genus")]
coordinates(species_9_st)<-~x+y
projection(species_9_st)<- projection(TBK_trees)


#extract predictions
pred_9_st <- as.factor(extract(pred_rf_9_st, species_9_st))

obs_9_st <- as.factor(species_9_st$genus)


cm_9_st<-confusionMatrix(pred_9_st, reference = obs_9_st)

cm_9_st


# Woodlands ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_9_prk <-as.data.frame(TBK_trees_parks_9)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_9_prk$class<-factor(trees_9_prk$species)
trees_9_prk$genus_f<-factor(trees_9_prk$genus)

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

rfmodel_9_prk <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                                mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                                mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_9_prk, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_9_prk


#Prediction
pred_rf_9_prk<-predict(covariates2, rfmodel_9_prk, 
                       filename = "D:/Classifications/Escalonada/RF/rf_9_prk_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_9_prk<-test_9_prk[c("x","y","genus")]
coordinates(species_9_prk)<-~x+y
projection(species_9_prk)<- projection(TBK_trees)


#extract predictions
pred_9_prk <- as.factor(extract(pred_rf_9_prk, species_9_prk))

obs_9_prk <- as.factor(species_9_prk$genus)


cm_9_prk<-confusionMatrix(pred_9_prk, reference = obs_9_prk)

cm_9_prk

# Residential ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_9_resi <-as.data.frame(TBK_trees_resi_9)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_9_resi$class<-factor(trees_9_resi$species)
trees_9_resi$genus_f<-factor(trees_9_resi$genus)

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

rfmodel_9_resi <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                                 mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                                 mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_9_resi, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_9_resi


#Prediction
pred_rf_9_resi<-predict(covariates2, rfmodel_9_resi, 
                        filename = "D:/Classifications/Escalonada/RF/rf_9_resi_v1.tif",
                        format ="GTiff",
                        overwrite=TRUE)



species_9_resi<-test_9_resi[c("x","y","genus")]
coordinates(species_9_resi)<-~x+y
projection(species_9_resi)<- projection(TBK_trees)


#extract predictions
pred_9_resi <- as.factor(extract(pred_rf_9_resi, species_9_resi))

obs_9_resi <- as.factor(species_9_resi$genus)


cm_9_resi<-confusionMatrix(pred_9_resi, reference = obs_9_resi)

cm_9_resi


#Other ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_9_oth <-as.data.frame(TBK_trees_otro_9)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_9_oth$class<-factor(trees_9_oth$species)
trees_9_oth$genus_f<-factor(trees_9_oth$genus)

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

rfmodel_9_oth <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                                mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                                mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_9_oth, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_9_oth


#Prediction
pred_rf_9_oth<-predict(covariates2, rfmodel_9_oth, 
                       filename = "D:/Classifications/Escalonada/RF/rf_9_oth_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_9_oth<-test_9_oth[c("x","y","genus")]
coordinates(species_9_oth)<-~x+y
projection(species_9_oth)<- projection(TBK_trees)


#extract predictions
pred_9_oth <- as.factor(extract(pred_rf_9_oth, species_9_oth))

obs_9_oth <- as.factor(species_9_oth$genus)


cm_9_oth<-confusionMatrix(pred_9_oth, reference = obs_9_oth)

cm_9_oth


######################################################################################

#Prepare the first two classes
TBK_trees_10<-TBK_trees
TBK_trees_10$species<-ifelse(TBK_trees_10$GATTUNG == 2, "Acer",
                            ifelse(TBK_trees_10$GATTUNG == 52, "Tilia",
                                   ifelse(TBK_trees_10$GATTUNG == 8,"Carpinus", 
                                          ifelse(TBK_trees_10$GATTUNG == 20, "Fraxinus",
                                                 ifelse(TBK_trees_10$GATTUNG == 46, "Robinia",
                                                        ifelse(TBK_trees_10$GATTUNG == 42, "Prunus", 
                                                               ifelse(TBK_trees_10$GATTUNG == 3, "Aesculus",
                                                                      ifelse(TBK_trees_10$GATTUNG == 19, "Fagus",
                                                                             ifelse(TBK_trees_10$GATTUNG == 41, "Populus",
                                                                                    ifelse(TBK_trees_10$GATTUNG == 45, "Quercus","Other"))))))))))


TBK_trees_10$genus<-ifelse(TBK_trees_10$species == "Acer", 1,
                          ifelse(TBK_trees_10$species == "Tilia", 2, 
                                 ifelse(TBK_trees_10$species == "Carpinus", 3,
                                        ifelse(TBK_trees_10$species == "Fraxinus", 4,
                                               ifelse(TBK_trees_10$species == "Robinia", 5,
                                                      ifelse(TBK_trees_10$species == "Prunus", 6, 
                                                             ifelse(TBK_trees_10$species == "Aesculus",7 ,
                                                                    ifelse(TBK_trees_10$species == "Fagus", 8,
                                                                           ifelse(TBK_trees_10$species == "Populus", 9,
                                                                                  ifelse(TBK_trees_10$species == "Quercus", 10, 11))))))))))


# Divide the trees according to the land use
TBK_trees_streets_10<-subset(TBK_trees_10, lu_raster == 2)
TBK_trees_parks_10<-subset(TBK_trees_10, lu_raster == 1)
TBK_trees_resi_10<-subset(TBK_trees_10, lu_raster == 3)
TBK_trees_otro_10<-subset(TBK_trees_10, lu_raster == 4)


# Streets ---------------------------------------------------------------------------------------------------------------------------

#Transform it to data frame
trees_10_st <-as.data.frame(TBK_trees_streets_10)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_10_st$class<-factor(trees_10_st$species)
trees_10_st$genus_f<-factor(trees_10_st$genus)

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

rfmodel_10_st <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                               mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                               mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_10_st, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_10_st

#varImpPlot(rfmodel_10_st, scale = FALSE)#, type=1, scale=TRUE)

#Prediction
pred_rf_10_st<-predict(covariates2, rfmodel_10_st, 
                      filename = "D:/Classifications/Escalonada/RF/rf_10_st_v1.tif",
                      format ="GTiff",
                      overwrite=TRUE)



species_10_st<-test_10_st[c("x","y","genus")]
coordinates(species_10_st)<-~x+y
projection(species_10_st)<- projection(TBK_trees)


#extract predictions
pred_10_st <- as.factor(extract(pred_rf_10_st, species_10_st))

obs_10_st <- as.factor(species_10_st$genus)


cm_10_st<-confusionMatrix(pred_10_st, reference = obs_10_st)

cm_10_st


# Woodlands ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_10_prk <-as.data.frame(TBK_trees_parks_10)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_10_prk$class<-factor(trees_10_prk$species)
trees_10_prk$genus_f<-factor(trees_10_prk$genus)

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

rfmodel_10_prk <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                                mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                                mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_10_prk, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_10_prk


#Prediction
pred_rf_10_prk<-predict(covariates2, rfmodel_10_prk, 
                       filename = "D:/Classifications/Escalonada/RF/rf_10_prk_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_10_prk<-test_10_prk[c("x","y","genus")]
coordinates(species_10_prk)<-~x+y
projection(species_10_prk)<- projection(TBK_trees)


#extract predictions
pred_10_prk <- as.factor(extract(pred_rf_10_prk, species_10_prk))

obs_10_prk <- as.factor(species_10_prk$genus)


cm_10_prk<-confusionMatrix(pred_10_prk, reference = obs_10_prk)

cm_10_prk

# Residential ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_10_resi <-as.data.frame(TBK_trees_resi_10)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_10_resi$class<-factor(trees_10_resi$species)
trees_10_resi$genus_f<-factor(trees_10_resi$genus)

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

rfmodel_10_resi <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                                 mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                                 mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_10_resi, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_10_resi


#Prediction
pred_rf_10_resi<-predict(covariates2, rfmodel_10_resi, 
                        filename = "D:/Classifications/Escalonada/RF/rf_10_resi_v1.tif",
                        format ="GTiff",
                        overwrite=TRUE)



species_10_resi<-test_10_resi[c("x","y","genus")]
coordinates(species_10_resi)<-~x+y
projection(species_10_resi)<- projection(TBK_trees)


#extract predictions
pred_10_resi <- as.factor(extract(pred_rf_10_resi, species_10_resi))

obs_10_resi <- as.factor(species_10_resi$genus)


cm_10_resi<-confusionMatrix(pred_10_resi, reference = obs_10_resi)

cm_10_resi


#Other ---------------------------------------------------------------------------------------------------------------------------
#Transform it to data frame
trees_10_oth <-as.data.frame(TBK_trees_otro_10)[c( "x","y", "species", "genus", "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8", "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8", "ndvi_mrj19", "ndvi_mrs21", "chm" )]
trees_10_oth$class<-factor(trees_10_oth$species)
trees_10_oth$genus_f<-factor(trees_10_oth$genus)

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

rfmodel_10_oth <- randomForest(genus_f ~ mr_j191_b1 + mr_j191_b2 + mr_j191_b3 + mr_j191_b4 + mr_j191_b5 + mr_j191_b6 + mr_j191_b7 + 
                                mr_j191_b8 + mr_s211_b1 + mr_s211_b2 + mr_s211_b3 + mr_s211_b4 + mr_s211_b5 + mr_s211_b6 + mr_s211_b7 + 
                                mr_s211_b8 + ndvi_mrj19 + ndvi_mrs21 + chm, data=train_10_oth, na.action = na.omit, ntree=2000, importance = TRUE, confusion=TRUE)

rfmodel_10_oth


#Prediction
pred_rf_10_oth<-predict(covariates2, rfmodel_10_oth, 
                       filename = "D:/Classifications/Escalonada/RF/rf_10_oth_v1.tif",
                       format ="GTiff",
                       overwrite=TRUE)



species_10_oth<-test_10_oth[c("x","y","genus")]
coordinates(species_10_oth)<-~x+y
projection(species_10_oth)<- projection(TBK_trees)


#extract predictions
pred_10_oth <- as.factor(extract(pred_rf_10_oth, species_10_oth))

obs_10_oth <- as.factor(species_10_oth$genus)


cm_10_oth<-confusionMatrix(pred_10_oth, reference = obs_10_oth)

cm_10_oth