install.packages("pacman")
library(sp)
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
merged_trees <- st_as_sf(merged_trees)

################################################################################
#                              SVF
################################################################################

data<-vect("D:/Tree_data_test/Official_test1/Points_all.gpkg")
svf<-rast("D:/Tree_data_test/Test1/SkyViewFactor.tif")

data$SVF_1 <- extract(svf, data)[2]


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



################################################################################
#                             Height
################################################################################

trees_chm<-rast("D:/nDSM_Muc.tif")
pct <- function(x, p=0.90, na.rm = TRUE) { quantile(x, p, na.rm = na.rm) }

merged_trees$height_90 <-extract(trees_chm, merged_trees, fun=pct)[2]


################################################################################
#                              Diameter
################################################################################

# Create centroids
centroids <- st_centroid(merged_trees)

# Select tree Id (to do loop afterwards
TReeID <- 3

# Duplicate point and move upwards
newpointUp <- centroids[TReeID,]
newpointDown <- centroids[TReeID,]
newpointUp[1,]$geometry[[1]][2] <- newpointUp[1,]$geometry[[1]][2] + 50
newpointDown[1,]$geometry[[1]][2] <- newpointDown[1,]$geometry[[1]][2] - 50

# Define a set of coordinates to create line from the two points
coordinates <- matrix(c(
  c(st_coordinates(newpointUp)[1], st_coordinates(newpointDown[1,])[1]),
  c(st_coordinates(newpointUp)[2], st_coordinates(newpointDown[1,])[2])
), ncol = 2)

# Create an sf LineString object
line <- st_linestring(coordinates) %>% st_sfc() %>% st_set_crs(st_crs(centroids))

# Define rotation function
rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2) 

# Create rotated line object
combined_lines <- line

# Rotate line by 5
for (i in seq(5, 355, by = 5)) {
  
  linerot <- ((line - st_centroid(line)) * rot(deg2rad(i)) + st_centroid(line)) %>%
    st_sfc() %>% st_set_crs(st_crs(centroids))
  
  # Combine the rotated LINESTRING objects into a single sfc object
  combined_lines <- st_combine(c(combined_lines, linerot))
  
}

# Intersect objects
intersect <- st_intersection(combined_lines, merged_trees[TReeID,])

#Visual inspection
mapview(merged_trees) + mapview(centroids) + mapview(intersect) 

# Create a new vector
Lengthvector <- vector()

for(i in 1:length(intersect[[1]])){
  TemporalLIne <- st_linestring(intersect[[1]][[i]]) %>% st_sfc() %>% st_set_crs(st_crs(centroids))
  Length <- st_length(TemporalLIne)
  Lengthvector <- c(Lengthvector,c(Length))
}

# Convert vector to dataframe 
LengthDF <- data.frame(Length = Lengthvector)

# Plot geom density
ggplot(LengthDF, aes(x=Length)) +
  geom_density(aes(fill = "red"), color = "black", alpha=0.3, lwd = .5, show.legend = F) +
  geom_vline(aes(color=paste0("Mean:",   round(mean(Lengthvector),2)) , xintercept=mean(Lengthvector)), linetype="solid",  size=1.0, show.legend = NA)+
  geom_vline(aes(color=paste0("Median:",   round(median(Lengthvector),2)) , xintercept=median(Lengthvector)), linetype="solid",  size=1.0, show.legend = NA)+
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  labs(color = "Stats")
