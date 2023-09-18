
install.packages("writexl")
library(dplyr)
library(sf)
library(terra)
library(raster)
library(pacman)
library(tidyverse)
library(writexl)
# Load Libraries
pacman::p_load(dplyr,sf,ggplot2, mapview, st, units, REdaS)

################################################################################
#                              Extract genera
################################################################################

#Load data
str_trees<-vect("D:/Tree_data_test/Test1/Class_streets_t1.gpkg")
prk_trees<-vect("D:/Tree_data_test/Test1/Class_park_t1.gpkg")
res_trees<-vect("D:/Tree_data_test/Test1/Class_resi_t1.gpkg")
oth_trees<-vect("D:/Tree_data_test/Test1/Class_other_t1.gpkg")

str_class<-rast("D:/Classifications/Escalonada/RF/rf_11_st_oficial.tif")
prk_class<-rast("D:/Classifications/Escalonada/RF/rf_11_prk_oficial.tif")
res_class<-rast("D:/Classifications/Escalonada/RF/rf_11_resi_oficial.tif")
oth_class<-rast("D:/Classifications/Escalonada/RF/rf_11_oth_oficial_2.tif")

str_trees$genus <- (terra::extract(str_class, str_trees, fun="modal", na.rm=TRUE))[2]
prk_trees$genus <- (terra::extract(prk_class, prk_trees, fun="modal", na.rm=TRUE))[2]
res_trees$genus <- (terra::extract(res_class, res_trees, fun="modal", na.rm=TRUE))[2]
oth_trees$genus <- (terra::extract(oth_class, oth_trees, fun="modal", na.rm=TRUE))[2]


merged_trees <- rbind(str_trees, prk_trees, res_trees, oth_trees)

################################################################################
#                              SVF
################################################################################

data<-vect("D:/Tree_data_test/Official_test1/Points_all.gpkg")
svf<-rast("D:/Tree_data_test/Test1/SkyViewFactor.tif")

data$SVF_total <- terra::extract(svf, data)[2]


data$SVF_S<- data$SVF_1 / 4
data$SVF_E<- data$SVF_1 / 4
data$SVF_N<- data$SVF_1 / 4
data$SVF_W<- data$SVF_1 / 4

################################################################################
#                             Location
################################################################################

data$city <- "Munich"
data$treeID<- data$ID

data<-st_as_sf(data)

nutzung<-st_read("C:/Users/ang58gl/Documents/Data/Nutzung.shp")
nutzung <- st_transform(nutzung, CRS("+init=epsg:32632"))

data<-st_join(data, nutzung, join=st_intersects, left=FALSE)

data<- data[, !(names(data) == "id_1")]
data<- data[, !(names(data) == "GN_majorit")]
data<- data[, !(names(data) == "gml_id")]
data<- data[, !(names(data) == "oid")]
data<- data[, !(names(data) == "aktualit")]
data<- data[, !(names(data) == "nutzart")]
data<- data[, !(names(data) == "bez")]

################################################################################
#                             Height
################################################################################

trees_chm<-rast("D:/nDSM_Muc.tif")
pcth <- function(x, p=0.90, na.rm = TRUE) { quantile(x, p, na.rm = na.rm) }

merged_trees$height_90 <- (terra::extract(trees_chm, merged_trees, fun=pcth))[2]
                      

names(merged_trees$height_90)

merged_trees <- st_as_sf(merged_trees)

################################################################################
#                              Diameter
################################################################################

# Create centroids
centroids <- st_centroid(merged_trees)

# Count total of shapes
TotalTrees <- length(centroids[[1]])

# Precompute centroid coordinates
centroid_coords <- st_coordinates(centroids)

# Initialize vectors to store results
min_lengths <- numeric(TotalTrees)
med_lengths <- numeric(TotalTrees)
mean_lengths <- numeric(TotalTrees)
max_lengths <- numeric(TotalTrees)

# Create progress bar
pb <- txtProgressBar(min = 1, max = TotalTrees, style = 3)

# Loop over all available tree shapes
for (i in 1:TotalTrees) {
  
  # Convert polygon into points
  PolPoints <- st_cast(merged_trees[i,], "POINT")
  
  # Initialize variables to store lengths
  Lengthvector <- numeric(length(PolPoints[[1]]))
  
  # Loop over all polygon points to create the lines between centroid and polygon edges
  for (j in 1:length(PolPoints[[1]])) {
    # Define a set of coordinates to create line from the two points
    coordinates <- matrix(c(
      st_coordinates(PolPoints[j,][1])[1], centroid_coords[i,][1],
      st_coordinates(PolPoints[j,][1])[2], centroid_coords[i,][2]
    ), ncol = 2)
    
    # Create an sf LineString object
    line <- st_linestring(coordinates) %>% st_sfc() %>% st_set_crs(st_crs(centroids))
    
    # Calculate length of line
    Length <- st_length(line)
    
    # Store the length in the vector
    Lengthvector[j] <- Length
  }
  
  # Calculate fields from the lines and store them in vectors
  min_lengths[i] <- round(min(Lengthvector), 2)
  med_lengths[i] <- round(median(Lengthvector), 2)
  mean_lengths[i] <- round(mean(Lengthvector), 2)
  max_lengths[i] <- round(max(Lengthvector), 2)
  
  # Increment progress bar
  setTxtProgressBar(pb, i)
  
  # Close progress bar when done
  if(i == TotalTrees){
    close(pb)
  }
}

# Assign the vectors to the centroids data frame
centroids$min <- min_lengths
centroids$med <- med_lengths
centroids$mean <- mean_lengths
centroids$max <- max_lengths

merged_trees$diam<-(centroids$mean*2)


################################################################################
#                              DBH
################################################################################
merged_trees$cpa<-pi*(merged_trees$diam/2)^2

merged_trees$dbh <- ifelse(merged_trees$genus == 1, (exp(0.54 *log(merged_trees$cpa)+ 0.83 * log(merged_trees$height_90)- 0.09 * log(merged_trees$cpa) * log(merged_trees$height_90)+0.12)),
    ifelse(merged_trees$genus == 2, (exp(-0.06 * log(merged_trees$cpa) + 0.16 * log(merged_trees$height_90)+ 0.16 * log(merged_trees$cpa) * log(merged_trees$height_90)-1.63)),
          ifelse(merged_trees$genus == 3, (exp(0.36 * log(merged_trees$cpa) + 0.75 * log(merged_trees$height_90)-0.11)),
                  ifelse(merged_trees$genus == 4, (exp(0.31 * log(merged_trees$cpa) + 0.67 * log(merged_trees$height_90)+0.76 )),
                         ifelse(merged_trees$genus == 5, (exp(0.36 * log(merged_trees$cpa)+ 0.53 * log(merged_trees$height_90) + 0.67)),
                                ifelse(merged_trees$genus == 7, (exp(0.43 * log(merged_trees$cpa)+ 0.42 * log(merged_trees$height_90) + 0.68)),
                                       ifelse(merged_trees$genus == 8, (exp(0.20 * log(merged_trees$cpa)+ 0.72 * log(merged_trees$height_90) + 0.70)),
                                              ifelse(merged_trees$genus == 11, (exp(0.56 * log(merged_trees$cpa)+ 1.23 * log(merged_trees$height_90) - 0.11 * log(merged_trees$cpa) * log(merged_trees$height_90) - 0.74)), 
                                                     ifelse(merged_trees$genus == 13, (exp(0.44 * log(merged_trees$cpa)+ 0.73 * log(merged_trees$height_90) - 0.04 * log(merged_trees$cpa) * log(merged_trees$height_90) + 0.26)), NA)))))))))


merged_trees <- merged_trees %>%
  mutate(dbh_class = case_when(
    dbh<= 10 ~ 10,
    dbh> 10 & dbh <= 20 ~ 20,
    dbh> 20 & dbh <= 30 ~ 30,
    dbh> 30 & dbh <= 40 ~ 40,
    dbh> 40 & dbh <= 50 ~ 50,
    dbh> 50 & dbh <= 60 ~ 60,
    dbh> 60 & dbh <= 70 ~ 70,
    dbh> 70 & dbh <= 80 ~ 80,
    dbh> 80 & dbh <= 90 ~ 90,
    dbh> 90 ~ 100))



################################################################################
#                                extras
################################################################################

merged_trees$LAI<-0
merged_trees$competing<-0
merged_trees$soil_sealing <-0
merged_trees$crown_lenght<-0
merged_trees$soil_type<-0
merged_trees$field_capacity<-0
merged_trees$wilting_point<-0
merged_trees$rooting_depth<-0
merged_trees$period<-0
merged_trees$CO2_concentration<-0
merged_trees$radiation_Jan<-0
merged_trees$radiation_Feb<-0
merged_trees$radiation_Mar<-0
merged_trees$radiation_Apr<-0
merged_trees$radiation_May<-0
merged_trees$radiation_Jun<-0
merged_trees$radiation_Jul<-0
merged_trees$radiation_Aug<-0
merged_trees$radiation_Sep<-0
merged_trees$radiation_Oct<-0
merged_trees$radiation_Nov<-0
merged_trees$radiation_Dez<-0
merged_trees$temperature_Jan<-0
merged_trees$temperature_Feb<-0
merged_trees$temperature_Mar<-0
merged_trees$temperature_Apr<-0
merged_trees$temperature_May<-0
merged_trees$temperature_Jun<-0
merged_trees$temperature_Jul<-0
merged_trees$temperature_Aug<-0
merged_trees$temperature_Sep<-0
merged_trees$temperature_Oct<-0
merged_trees$temperature_Nov<-0
merged_trees$temperature_Dez<-0
merged_trees$humidity_Jan<-0
merged_trees$humidity_Feb<-0
merged_trees$humidity_Mar<-0
merged_trees$humidity_Apr<-0
merged_trees$humidity_May<-0
merged_trees$humidity_Jun<-0
merged_trees$humidity_Jul<-0
merged_trees$humidity_Aug<-0
merged_trees$humidity_Sep<-0
merged_trees$humidity_Oct<-0
merged_trees$humidity_Nov<-0
merged_trees$humidity_Dez<-0
merged_trees$wind_speed_Jan<-0
merged_trees$wind_speed_Feb<-0
merged_trees$wind_speed_Mar<-0
merged_trees$wind_speed_Apr<-0
merged_trees$wind_speed_May<-0
merged_trees$wind_speed_Jun<-0
merged_trees$wind_speed_Jul<-0
merged_trees$wind_speed_Aug<-0
merged_trees$wind_speed_Sep<-0
merged_trees$wind_speed_Oct<-0
merged_trees$wind_speed_Nov<-0
merged_trees$wind_speed_Dez<-0
merged_trees$precipitation_Jan<-0
merged_trees$precipitation_Feb<-0
merged_trees$precipitation_Mar<-0
merged_trees$precipitation_Apr<-0
merged_trees$precipitation_May<-0
merged_trees$precipitation_Jun<-0
merged_trees$precipitation_Jul<-0
merged_trees$precipitation_Aug<-0
merged_trees$precipitation_Sep<-0
merged_trees$precipitation_Oct<-0
merged_trees$precipitation_Nov<-0
merged_trees$precipitation_Dez<-0



################################################################################
#                              coordinates
################################################################################
merged_trees<-st_join(data, merged_trees, join=st_intersects, left=FALSE)

merged_trees <- st_transform(merged_trees, CRS("+init=epsg:4326"))

merged_trees <- merged_trees %>% extract(geometry, c('lon', 'lat'), '\\((.*), (.*)\\)', convert = TRUE)

input<- merged_trees %>%
  dplyr::select(City, site_name, lat, lon, ID,  SVF_E, SVF_S, SVF_W, SVF_N, competing, LAI, soil_sealing, soil_type, field_capacity, wilting_point, rooting_depth, period, CO2_concentration, genus, dbh_class, dbh, height_90, crown_lenght, diam.y, radiation_Jan, 
                radiation_Feb, radiation_Mar, radiation_Apr, radiation_May, radiation_Jun, radiation_Jul, radiation_Aug, radiation_Sep, radiation_Oct, radiation_Nov, radiation_Dez, temperature_Jan, temperature_Feb, temperature_Mar, temperature_Apr, temperature_May,
                temperature_Jun, temperature_Jul, temperature_Aug, temperature_Sep, temperature_Oct, temperature_Nov, temperature_Dez, humidity_Jan, humidity_Feb, humidity_Mar, humidity_Apr, humidity_May, humidity_Jun, humidity_Jul, humidity_Aug, humidity_Sep,
                humidity_Oct, humidity_Nov, humidity_Dez, wind_speed_Jan, wind_speed_Feb, wind_speed_Mar, wind_speed_Apr, wind_speed_May, wind_speed_Jun, wind_speed_Jul, wind_speed_Aug, wind_speed_Sep, wind_speed_Oct, wind_speed_Nov, wind_speed_Dez, precipitation_Jan,
                precipitation_Feb, precipitation_Mar, precipitation_Apr, precipitation_May, precipitation_Jun, precipitation_Jul, precipitation_Aug, precipitation_Sep, precipitation_Oct, precipitation_Nov, precipitation_Dez)

input<-data.frame(input)

names(input)<- c("city", "site", "latitude", "longitude", "TreeID", "SVF_E", "SVF_S", "SVF_W", "SVF_N", "competing", "LAI", "soil_sealing", "soil_type", "field_capacity", "wilting_point", "rooting_depth", "period", "CO2_concentration", "tree_genus", "dbh_class", "dbh", "height", "crown_lenght", "crown_diameter", "radiation_Jan", 
                 "radiation_Feb", "radiation_Mar", "radiation_Apr", "radiation_May", "radiation_Jun", "radiation_Jul", "radiation_Aug", "radiation_Sep", "radiation_Oct", "radiation_Nov", "radiation_Dez", "temperature_Jan", "temperature_Feb", "temperature_Mar", "temperature_Apr", "temperature_May",
                 "temperature_Jun", "temperature_Jul", "temperature_Aug", "temperature_Sep", "temperature_Oct", "temperature_Nov", "temperature_Dez", "humidity_Jan", "humidity_Feb", "humidity_Mar", "humidity_Apr", "humidity_May", "humidity_Jun", "humidity_Jul", "humidity_Aug", "humidity_Sep",
                 "humidity_Oct", "humidity_Nov", "humidity_Dez", "wind_speed_Jan", "wind_speed_Feb, wind_speed_Mar, wind_speed_Apr", "wind_speed_May", "wind_speed_Jun", "wind_speed_Jul", "wind_speed_Aug", "wind_speed_Sep", "wind_speed_Oct", "wind_speed_Nov", "wind_speed_Dez", "precipitation_Jan",
                 "precipitation_Feb", "precipitation_Mar", "precipitation_Apr", "precipitation_May", "precipitation_Jun", "precipitation_Jul", "precipitation_Aug", "precipitation_Sep", "precipitation_Oct", "precipitation_Nov", "precipitation_Dez")

write.table(data.frame(input), file="D:/Tree_data_test/Official_test1/input_data_t1.txt", sep = "\t", row.names = FALSE)
write_xlsx(data.frame(input), path = "D:/Tree_data_test/Official_test1/input_data_t1.xlsx")

table(merged_trees$genus)
