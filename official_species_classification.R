
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

genus<- unique(TBK_trees$GATTUNG)
genus


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

#########################################################################################

#Load raster
covariates<-stack("D:/Preprocessed/New_preprocessing/covariates_masked.tif")
trees_chm<-raster("D:/Classifications/2.2_trees_MR_nobuildings.tif")
trees_chm<-resample(trees_chm, covariates)
covariates2<-covariates *trees_chm

names(covariates2)<-  c("RVI_mrj19", "RVI_mrs21", "ndvi_mrj19", "ndvi_mrs21", "ndre_mrj19", "ndre_mrs21", "chm", "lu_raster", 
                        "mr_j191_b1", "mr_j191_b2", "mr_j191_b3", "mr_j191_b4", "mr_j191_b5", "mr_j191_b6", "mr_j191_b7", "mr_j191_b8",
                        "mr_s211_b1", "mr_s211_b2", "mr_s211_b3", "mr_s211_b4", "mr_s211_b5", "mr_s211_b6", "mr_s211_b7", "mr_s211_b8")




######################################################################################

#Prepare the first two classes
TBK_trees_10<-TBK_trees
TBK_trees_10$species<-ifelse(TBK_trees_10$GATTUNG == 2, "Acer",
                             ifelse(TBK_trees_10$GATTUNG == 52, "Tilia",
                                 #   ifelse(TBK_trees_10$GATTUNG == 8,"Carpinus", 
                                  #        ifelse(TBK_trees_10$GATTUNG == 20, "Fraxinus",
                                                  ifelse(TBK_trees_10$GATTUNG == 46, "Robinia",
                                   #                      ifelse(TBK_trees_10$GATTUNG == 42, "Platanus", 
                                                                ifelse(TBK_trees_10$GATTUNG == 3, "Aesculus",
                                    #                                   ifelse(TBK_trees_10$GATTUNG == 19, "Fagus",
                                                                              ifelse(TBK_trees_10$GATTUNG == 41, "Populus",
                                     #                                                ifelse(TBK_trees_10$GATTUNG == 6, "Betula",
                                                                                            #ifelse(TBK_trees_10$GATTUNG == 45, "Quercus",
                                                                                                   "Other")))))
                           
                             
TBK_trees_10$genus<-ifelse(TBK_trees_10$species == "Tilia", 1,
                            ifelse(TBK_trees_10$species == "Robinia", 2,
                                   # ifelse(TBK_trees_10$species == "Platanus", 3,
                                          ifelse(TBK_trees_10$species == "Aesculus",4 ,
                                                ifelse(TBK_trees_10$species == "Acer", 6,
                                                       #ifelse(TBK_trees_10$species == "Fraxinus", 7,
                                                            #  ifelse(TBK_trees_10$species == "Betula", 8,
                                                                #     ifelse(TBK_trees_10$species == "Fagus", 9,
                                                                            ifelse(TBK_trees_10$species == "Populus", 10, 7)))))
                                                                             #     ifelse(TBK_trees_10$species == "Quercus", 11,,
                                                                                     #    ifelse(TBK_trees_10$species == "Carpinus", 12, 13)))))))))))


                                                    
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
                       filename = "D:/Classifications/Escalonada/RF/rf_12_st_oficial_v71.tif",
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
                        filename = "D:/Classifications/Escalonada/RF/rf_12_prk_oficial_v71.tif",
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
                         filename = "D:/Classifications/Escalonada/RF/rf_12_resi_oficial_v71.tif",
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
                        filename = "D:/Classifications/Escalonada/RF/rf_12_oth_oficial_v71.tif",
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

