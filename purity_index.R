
library(sf)
library(tidyverse)
library(terra)
library(dplyr)
library(raster)
library(exactextractr)

#Load data
str_trees<-st_read("D:/Paper_1/Class_streets_t1.gpkg")
prk_trees<-st_read("D:/Paper_1/Class_parks_t1.gpkg")
res_trees<-st_read("D:/Paper_1/Class_resi_t1.gpkg")
oth_trees<-st_read("D:/Paper_1/Class_other_t1.gpkg")


str_class<-rast("D:/Classifications/Escalonada/RF/rf_12_st_oficial_v71.tif")
prk_class<-rast("D:/Classifications/Escalonada/RF/rf_12_prk_oficial_v71.tif")
res_class<-rast("D:/Classifications/Escalonada/RF/rf_12_resi_oficial_v72.tif")
oth_class<-rast("D:/Classifications/Escalonada/RF/rf_12_oth_oficial_v71.tif")

#Extract the majoritarian class with the function modal
str_trees$genus <- (terra::extract(str_class, str_trees, fun="modal", na.rm=TRUE))[[2]]
prk_trees$genus <- (terra::extract(prk_class, prk_trees, fun="modal", na.rm=TRUE))[[2]]
res_trees$genus <- (terra::extract(res_class, res_trees, fun="modal", na.rm=TRUE))[[2]]
oth_trees$genus <- (terra::extract(oth_class, oth_trees, fun="modal", na.rm=TRUE))[[2]]

#Extract the fractions with the function "frac"
str_fractions<-exactextractr::exact_extract(str_class, str_trees, append_cols=TRUE, fun='frac')
prk_fractions<-exactextractr::exact_extract(prk_class, prk_trees, append_cols=TRUE, fun='frac')
res_fractions<-exactextractr::exact_extract(res_class, res_trees, append_cols=TRUE, fun='frac')
oth_fractions<-exactextractr::exact_extract(oth_class, oth_trees, append_cols=TRUE, fun='frac')

str_fractions$geom<-str_trees$geom
prk_fractions$geom<-prk_trees$geom
res_fractions$geom<-res_trees$geom
oth_fractions$geom<-oth_trees$geom

#Join the four data frames 
trees_fractions <- rbind(str_fractions, prk_fractions, res_fractions, oth_fractions)

# Specify the columns you want to consider for finding the maximum
#cols_to_max <- c("frac_1", "frac_2", "frac_3",  "frac_4", "frac_5", "frac_7", "frac_8", "frac_9", "frac_10",  "frac_11",  "frac_12",  "frac_13")  
cols_to_max <- c("frac_1", "frac_2",  "frac_4", "frac_6", "frac_7", "frac_10")  



# Create a new column "max_value" containing the maximum value among selected columns
trees_fractions <- trees_fractions %>%
  mutate(max_value = pmax(!!!select(., all_of(cols_to_max))))

#-------------------------------------------------------------------------------------------------


#Calculate 
trees_fractions$entropy <- -rowSums(trees_fractions[, c("frac_1", "frac_2",  "frac_4", "frac_6", "frac_7", "frac_10")] * log(trees_fractions[, c("frac_1", "frac_2", "frac_4", "frac_6", "frac_7", "frac_10")]), na.rm = TRUE)
head(trees_fractions)

# Find the highest value in the "entropy" column
summary(trees_fractions)
max_entropy <- 1.7377

# Create the "index" column by dividing "entropy" by the highest value
trees_fractions$index <- 1-trees_fractions$entropy / max_entropy

class(trees_fractions$genus)

# Export the joined data to a geopackage
st_write(trees_fractions, "D:/Paper_1/purity_index_official.gpkg")
