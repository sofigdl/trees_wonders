
library(sf)
library(tidyverse)
library(terra)
library(exactextractr)

#Load data
str_trees<-st_read("D:/Tree_data_test/Test1/Class_streets_t1.gpkg")
prk_trees<-st_read("D:/Tree_data_test/Test1/Class_park_t1.gpkg")
res_trees<-st_read("D:/Tree_data_test/Test1/Class_resi_t1.gpkg")
oth_trees<-st_read("D:/Tree_data_test/Test1/Class_other_t1.gpkg")


str_class<-rast("D:/Classifications/Escalonada/RF/rf_11_st_oficial.tif")
prk_class<-rast("D:/Classifications/Escalonada/RF/rf_11_prk_oficial.tif")
res_class<-rast("D:/Classifications/Escalonada/RF/rf_11_resi_oficial.tif")
oth_class<-rast("D:/Classifications/Escalonada/RF/rf_11_oth_oficial_2.tif")

#Extract the majoritarian class with the function modal
str_trees$genus <- (extract(str_class, str_trees, fun="modal", na.rm=TRUE))[2]
prk_trees$genus <- (extract(prk_class, prk_trees, fun="modal", na.rm=TRUE))[2]
res_trees$genus <- (extract(res_class, res_trees, fun="modal", na.rm=TRUE))[2]
oth_trees$genus <- (extract(oth_class, oth_trees, fun="modal", na.rm=TRUE))[2]

#Extract the fractions with the function "frac"
str_fractions<-exactextractr::exact_extract(str_class, str_trees, append_cols=TRUE, fun='frac')
prk_fractions<-exactextractr::exact_extract(prk_class, prk_trees, append_cols=TRUE, fun='frac')
res_fractions<-exactextractr::exact_extract(res_class, res_trees, append_cols=TRUE, fun='frac')
oth_fractions<-exactextractr::exact_extract(oth_class, oth_trees, append_cols=TRUE, fun='frac')

#Join the four data frames 
trees_fractions <- rbind(str_fractions, prk_fractions, res_fractions, oth_fractions)

