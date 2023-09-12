
install.packages("exactextractr")
library(dplyr)
library(sf)
library(terra)
library(raster)
library(pacman)
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

str_trees$genus <- (extract(str_class, str_trees, fun="modal", na.rm=TRUE))[2]
prk_trees$genus <- (extract(prk_class, prk_trees, fun="modal", na.rm=TRUE))[2]
res_trees$genus <- (extract(res_class, res_trees, fun="modal", na.rm=TRUE))[2]
oth_trees$genus <- (extract(oth_class, oth_trees, fun="modal", na.rm=TRUE))[2]


merged_trees <- rbind(str_trees, prk_trees, res_trees, oth_trees)

################################################################################
#                              SVF
################################################################################

data<-vect("D:/Tree_data_test/Official_test1/Points_all.gpkg")
svf<-rast("D:/Tree_data_test/Test1/SkyViewFactor.tif")

data$SVF_total <- extract(svf, data)[2]


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

merged_trees$height_90 <- (extract(trees_chm, merged_trees, fun=pcth))[2]
                      

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
merged_trees$cpa<-pi*(merged_trees$diam.y/2)^2

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
  mutate(dbh = case_when(
    genus == 1 ~ exp(0.54 * log(cpa) + 0.83 * log(height_90) - 0.09 * log(cpa) * log(height_90) + 0.12),
    genus == 2 ~ exp(-0.06 * log(cpa) + 0.16 * log(height_90) + 0.16 * log(cpa) * log(height_90) - 1.63),
    genus == 3 ~ exp(0.36 * log(cpa) + 0.75 * log(height_90) - 0.11),
    genus == 4 ~ exp(0.31 * log(cpa) + 0.67 * log(height_90) + 0.76),
    genus == 5 ~ exp(0.36 * log(cpa) + 0.53 * log(height_90) + 0.67),
    genus == 7 ~ exp(0.43 * log(cpa) + 0.42 * log(height_90) + 0.68),
    genus == 8 ~ exp(0.20 * log(cpa) + 0.72 * log(height_90) + 0.70),
    genus == 11 ~ exp(0.56 * log(cpa) + 1.23 * log(height_90) - 0.11 * log(cpa) * log(height_90) - 0.74),
    genus == 13 ~ exp(0.44 * log(cpa) + 0.73 * log(height_90) - 0.04 * log(cpa) * log(height_90) + 0.26),
    TRUE ~ NA_real_
  ))                    


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
    dbh> 80 & dbh <= 90 ~ 90),
    dbh> 90 ~ 100)
    
merged_trees$dbh_class<-ifelse(merged_trees$dbh<= 10, 10,
                               ifelse(merged_trees$dbh > 10 & merged_trees$dbh <= 20, 20,
                                      ifelse(merged_trees$dbh > 20 & merged_trees$dbh <= 30, 30, 999)))
                                 #            ifelse(merged_trees$dbh > 30 & merged_trees$dbh <= 40, 40,
                                  #                  ifelse(merged_trees$dbh > 40 & merged_trees$dbh <= 50, 50,
                                   #                        ifelse(merged_trees$dbh > 50 & merged_trees$dbh <= 60, 60,
                                    #                              ifelse(merged_trees$dbh > 60 & merged_trees$dbh <= 70, 70,
                                     #                                    ifelse(merged_trees$dbh > 70 & merged_trees$dbh <= 80, 80,
                                      #                                          ifelse(merged_trees$dbh > 80 & merged_trees$dbh <= 90, 90,
                                       #                                                ifelse(merged_trees>90, 100, 999))#))))))))


################################################################################
#                                extras
################################################################################

merged_trees$LAI<-0
merged_trees$competing<-0

################################################################################
#                              coordinates
################################################################################
merged_trees<-st_join(data, merged_trees, join=st_intersects, left=FALSE)


data <- st_transform(data, CRS("+init=epsg:4326"))

data <- data %>% extract(geom, c('lon', 'lat', 'Z'), '\\((.*), (.*), (.*)\\)', convert = TRUE)

input<- data %>%
  select(City, site_name, lat, lon, ID, SVF_S, SVF_E, SVF_N, SVF_W, competing, LAI, genus, dbh_class, dbh, height_90, diam)
