data<-vect("D:/Test_MR/Thomas/Rs_32632.gpkg")
svf<-rast("D:/Test_MR/SVF.tif")

data$SVF_1 <- terra::extract(svf, data)[2]


data$SVF_S<- data$SVF_1 / 4
data$SVF_E<- data$SVF_1 / 4
data$SVF_N<- data$SVF_1 / 4
data$SVF_W<- data$SVF_1 / 4

data$City <- "Munich"
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

head(data)

st_write(data, "D:/Test_MR/Thomas/RS_trees.xlsx")
