
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

str_class<-rast("D:/Classifications/Escalonada/RF/rf_12_st_oficial_v71.tif")
prk_class<-rast("D:/Classifications/Escalonada/RF/rf_12_prk_oficial_v71.tif")
res_class<-rast("D:/Classifications/Escalonada/RF/rf_12_resi_oficial_v71.tif")
oth_class<-rast("D:/Classifications/Escalonada/RF/rf_12_oth_oficial_v71.tif")

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
#data<- data[, !(names(data) == "nutzart")]
data<- data[, !(names(data) == "bez")]

################################################################################
#                             Height
################################################################################

trees_chm<-rast("D:/nDSM_Muc.tif")
pcth <- function(x, p=0.90, na.rm = TRUE) { quantile(x, p, na.rm = na.rm) }

merged_trees$height_90 <- (terra::extract(trees_chm, merged_trees, fun=pcth))[2]
                      

names(merged_trees)

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

merged_trees$dbh <-  ifelse(merged_trees$genus == 1,(exp(0.850240949 + 0.535103021*log(merged_trees$height_90) + 0.314799056*log(merged_trees$cpa))),
                            ifelse(merged_trees$genus == 2, (exp(0.130836553 + 0.798783592*log(merged_trees$height_90) + 0.319621277*log(merged_trees$cpa))),
                                   ifelse(merged_trees$genus == 4, (exp(0.865043405 + 0.524457333*log(merged_trees$height_90) + 0.376979042*log(merged_trees$cpa))),
                                          ifelse(merged_trees$genus == 6, (exp(0.638550037 + 0.536615182*log(merged_trees$height_90) + 0.362004299*log(merged_trees$cpa))),
                                                 ifelse(merged_trees$genus == 7, (exp(0.595540029 + 0.506617938*log(merged_trees$height_90) + 0.398067451*log(merged_trees$cpa))),
                                                        ifelse(merged_trees$genus == 10, (exp(-0.194552782 + 0.963358572 *log(merged_trees$height_90) + 0.328558707*log(merged_trees$cpa))),9999))))))


  #ifelse(merged_trees$genus == 1, (exp(0.54 *log(merged_trees$cpa)+ 0.83 * log(merged_trees$height_90)- 0.09 * log(merged_trees$cpa) * log(merged_trees$height_90)+0.12)),
    #ifelse(merged_trees$genus == 2, (exp(-0.06 * log(merged_trees$cpa) + 0.16 * log(merged_trees$height_90)+ 0.16 * log(merged_trees$cpa) * log(merged_trees$height_90)-1.63)),
          #ifelse(merged_trees$genus == 3, (exp(0.36 * log(merged_trees$cpa) + 0.75 * log(merged_trees$height_90)-0.11)),
                 # ifelse(merged_trees$genus == 4, (exp(0.31 * log(merged_trees$cpa) + 0.67 * log(merged_trees$height_90)+0.76 )),
                        # ifelse(merged_trees$genus == 5, (exp(0.36 * log(merged_trees$cpa)+ 0.53 * log(merged_trees$height_90) + 0.67)),
                              #  ifelse(merged_trees$genus == 7, (exp(0.43 * log(merged_trees$cpa)+ 0.42 * log(merged_trees$height_90) + 0.68)),
                                      # ifelse(merged_trees$genus == 8, (exp(0.20 * log(merged_trees$cpa)+ 0.72 * log(merged_trees$height_90) + 0.70)),
                                             # ifelse(merged_trees$genus == 11, (exp(0.56 * log(merged_trees$cpa)+ 1.23 * log(merged_trees$height_90) - 0.11 * log(merged_trees$cpa) * log(merged_trees$height_90) - 0.74)), 
                                                    # ifelse(merged_trees$genus == 13, (exp(0.44 * log(merged_trees$cpa)+ 0.73 * log(merged_trees$height_90) - 0.04 * log(merged_trees$cpa) * log(merged_trees$height_90) + 0.26)), NA)))))))))


merged_trees<- merged_trees %>%
  filter(dbh >= 2.5)

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
#                               Crown length
################################################################################

merged_trees$crown_lenght<-  ifelse(merged_trees$genus == 1,(exp(-0.950159509 + 1.174469321*log(merged_trees$height_90) + 0.038591176*log(merged_trees$cpa))),
                            ifelse(merged_trees$genus == 2, (exp(-1.04235872 + 1.157660627*log(merged_trees$height_90) + 0.0572923*log(merged_trees$cpa))),
                                   ifelse(merged_trees$genus == 4, (exp(-0.87803219 + 1.148279131*log(merged_trees$height_90) + 0.050911022*log(merged_trees$cpa))),
                                          ifelse(merged_trees$genus == 6, (exp(-0.843688213 + 1.123293846*log(merged_trees$height_90) + 0.055790862*log(merged_trees$cpa))),
                                                 ifelse(merged_trees$genus == 7, (exp(-0.798466663 + 1.128155774*log(merged_trees$height_90) + 0.039361241*log(merged_trees$cpa))),
                                                        ifelse(merged_trees$genus == 10, (exp( 0.910051812*log(merged_trees$height_90) + 0.036639386*log(merged_trees$cpa))), 9999))))))



################################################################################
#                              soil sealing
################################################################################

data <- data %>%
  mutate(soil_sealing = case_when(
    nutzart == "Bahnverkehr" ~ 10,
    nutzart == "Fläche besonderer funktionaler Prägung" ~ 20,
    nutzart == "Fläche gemischter Nutzung" ~ 25,
    nutzart == "Fließgewässer" ~ 0,
    nutzart == "Flugverkehr" ~ 0,
    nutzart == "Friedhof" ~ 10,
    nutzart == "Gehölz" ~ 0,
    nutzart == "Heide" ~ 0,
    nutzart == "Industrie- und Gewerbefläche" ~ 35,
    nutzart == "Landwirtschaft" ~ 0,
    nutzart == "Moor" ~ 0,
    nutzart == "Platz" ~ 50,
    nutzart == "Sport-, Freizeit- und Erholungsfläche" ~ 0,
    nutzart == "Stehendes Gewässer" ~ 0,
    nutzart == "Straßenverkehr" ~ 35,
    nutzart == "Tagebau, Grube, Steinbruch" ~ 0,
    nutzart == "Unland/Vegetationslose Fläche" ~ 15,
    nutzart == "Wald" ~ 0,
    nutzart == "Weg" ~ 0,
    nutzart == "Wohnbaufläche" ~ 10))


################################################################################
#                                extras
################################################################################

merged_trees$LAI<-0
merged_trees$competing<-0
merged_trees$soil_type<-"sandy loam"
merged_trees$field_capacity<-25
merged_trees$wilting_point<-8
merged_trees$rooting_depth<-80
merged_trees$period<-"1991-2020"
merged_trees$CO2_concentration<-400
merged_trees$radiation_Jan<-371
merged_trees$radiation_Feb<-614
merged_trees$radiation_Mar<-962
merged_trees$radiation_Apr<-1396
merged_trees$radiation_May<-1674
merged_trees$radiation_Jun<-1845
merged_trees$radiation_Jul<-1855
merged_trees$radiation_Aug<-1653
merged_trees$radiation_Sep<-1177
merged_trees$radiation_Oct<-744
merged_trees$radiation_Nov<-417
merged_trees$radiation_Dez<-311
merged_trees$temperature_Jan<-0.9
merged_trees$temperature_Feb<-1.9
merged_trees$temperature_Mar<-5.7
merged_trees$temperature_Apr<-10.2
merged_trees$temperature_May<-14.3
merged_trees$temperature_Jun<-17.8
merged_trees$temperature_Jul<-19.6
merged_trees$temperature_Aug<-19.4
merged_trees$temperature_Sep<-14.7
merged_trees$temperature_Oct<-10.1
merged_trees$temperature_Nov<-4.9
merged_trees$temperature_Dez<-1.8
merged_trees$humidity_Jan<-80
merged_trees$humidity_Feb<-76
merged_trees$humidity_Mar<-71
merged_trees$humidity_Apr<-65
merged_trees$humidity_May<-67
merged_trees$humidity_Jun<-67
merged_trees$humidity_Jul<-66
merged_trees$humidity_Aug<-68
merged_trees$humidity_Sep<-75
merged_trees$humidity_Oct<-80
merged_trees$humidity_Nov<-83
merged_trees$humidity_Dez<-82
merged_trees$wind_speed_Jan<-2.7
merged_trees$wind_speed_Feb<-2.7
merged_trees$wind_speed_Mar<-2.9
merged_trees$wind_speed_Apr<-2.6
merged_trees$wind_speed_May<-2.5
merged_trees$wind_speed_Jun<-2.4
merged_trees$wind_speed_Jul<-2.5
merged_trees$wind_speed_Aug<-2.2
merged_trees$wind_speed_Sep<-2.2
merged_trees$wind_speed_Oct<-2.3
merged_trees$wind_speed_Nov<-2.4
merged_trees$wind_speed_Dez<-2.7
merged_trees$precipitation_Jan<-51.9
merged_trees$precipitation_Feb<-45.5
merged_trees$precipitation_Mar<-61.2
merged_trees$precipitation_Apr<-56.0
merged_trees$precipitation_May<-107.0
merged_trees$precipitation_Jun<-120.9
merged_trees$precipitation_Jul<-118.9
merged_trees$precipitation_Aug<-116.5
merged_trees$precipitation_Sep<-78.1
merged_trees$precipitation_Oct<-66.9
merged_trees$precipitation_Nov<-58.4
merged_trees$precipitation_Dez<-58.5
merged_trees$irrigation_start<-1
merged_trees$irrigation_end<-12
merged_trees$irrigation_amount<-0





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
                precipitation_Feb, precipitation_Mar, precipitation_Apr, precipitation_May, precipitation_Jun, precipitation_Jul, precipitation_Aug, precipitation_Sep, precipitation_Oct, precipitation_Nov, precipitation_Dez, irrigation_start, irrigation_end, irrigation_amount)

input<-data.frame(input)

names(input)<- c("city", "site", "latitude", "longitude", "TreeID", "SVF_E", "SVF_S", "SVF_W", "SVF_N", "competing", "LAI", "soil_sealing", "soil_type", "field_capacity", "wilting_point", "rooting_depth", "period", "CO2_concentration", "tree_genus", "dbh_class", "dbh", "height", "crown_lenght", "crown_diameter", "radiation_Jan", 
                 "radiation_Feb", "radiation_Mar", "radiation_Apr", "radiation_May", "radiation_Jun", "radiation_Jul", "radiation_Aug", "radiation_Sep", "radiation_Oct", "radiation_Nov", "radiation_Dez", "temperature_Jan", "temperature_Feb", "temperature_Mar", "temperature_Apr", "temperature_May",
                 "temperature_Jun", "temperature_Jul", "temperature_Aug", "temperature_Sep", "temperature_Oct", "temperature_Nov", "temperature_Dez", "humidity_Jan", "humidity_Feb", "humidity_Mar", "humidity_Apr", "humidity_May", "humidity_Jun", "humidity_Jul", "humidity_Aug", "humidity_Sep",
                 "humidity_Oct", "humidity_Nov", "humidity_Dez", "wind_speed_Jan", "wind_speed_Feb", "wind_speed_Mar", "wind_speed_Apr", "wind_speed_May", "wind_speed_Jun", "wind_speed_Jul", "wind_speed_Aug", "wind_speed_Sep", "wind_speed_Oct", "wind_speed_Nov", "wind_speed_Dez", "precipitation_Jan",
                 "precipitation_Feb", "precipitation_Mar", "precipitation_Apr", "precipitation_May", "precipitation_Jun", "precipitation_Jul", "precipitation_Aug", "precipitation_Sep", "precipitation_Oct", "precipitation_Nov", "precipitation_Dez", "irrigation_start", "irrigation_end", "irrigation_amount")

write.table(data.frame(input), file="D:/Tree_data_test/Official_test1/input_data_t4.txt", sep = "\t", row.names = FALSE)
write_xlsx(data.frame(input), path = "D:/Tree_data_test/Official_test1/input_data_t4.xlsx")

table(merged_trees$genus)
