
library(sf)
library(tidyverse)
library(exactextractr)

#Load data
str_trees<-st_read("D:/Tree_data_test/Test1/Class_streets_t1.gpkg")
prk_trees<-vect("D:/Tree_data_test/Test1/Class_park_t1.gpkg")
res_trees<-vect("D:/Tree_data_test/Test1/Class_resi_t1.gpkg")
oth_trees<-vect("D:/Tree_data_test/Test1/Class_other_t1.gpkg")

str_class<-rast("D:/Classifications/Escalonada/RF/rf_11_st_oficial.tif")
prk_class<-rast("D:/Classifications/Escalonada/RF/rf_11_prk_oficial.tif")
res_class<-rast("D:/Classifications/Escalonada/RF/rf_11_resi_oficial.tif")
oth_class<-rast("D:/Classifications/Escalonada/RF/rf_11_oth_oficial_2.tif")

str_trees$genus <- (extract(str_class, str_trees, fun="modal", na.rm=TRUE))[2]
prk_trees$genus <- (extract(prk_class, prk_trees, fun="modal", na.rm=TRUE))[2]
res_trees$genus <- (extract(res_class, res_trees, fun="modal", na.rm=TRUE))[2]
oth_trees$genus <- (extract(oth_class, oth_trees, fun="modal", na.rm=TRUE))[2]


merged_trees <- rbind(str_trees, prk_trees, res_trees, oth_trees)
merged_trees <- st_as_sf(merged_trees)

head(data_trees)


str_fractions<-exactextractr::exact_extract(str_class, str_trees, append_cols=TRUE, fun='frac')

prk_fractions<-exactextractr::exact_extract(prk_class, prk_trees, append_cols=TRUE, fun='frac')

res_fractions<-exactextractr::exact_extract(res_class, res_trees, append_cols=TRUE, fun='frac')

oth_fractions<-exactextractr::exact_extract(oth_class, oth_trees, append_cols=TRUE, fun='frac')