pacman::p_load(sf, FNN, dplyr)

kataster<-st_read("D:/Test_MR/Kataster_overlapped_counted.gpkg")
rstrees <- st_read("D:/Test_MR/RS_Points_MR_32632.gpkg")

# Asegurar mismo CRS
rstrees <- st_transform(rstrees, st_crs(kataster))

# Eliminar Z si existe
kataster <- st_zm(kataster, drop = TRUE)
rstrees  <- st_zm(rstrees, drop = TRUE)

kat_coords <- st_coordinates(kataster)[, 1:2]
rs_coords  <- st_coordinates(rstrees)[, 1:2]

nn <- get.knnx(
  data  = rs_coords,     # puntos RS (base)
  query = kat_coords,    # puntos kataster (consulta)
  k = 1                  # nearest neighbor
)

kataster$nn_dist_m <- nn$nn.dist[, 1]      # distancia en metros
kataster$rs_id_nn  <- nn$nn.index[, 1]  

summary(kataster$nn_dist_m)

sd(kataster$nn_dist_m, na.rm = TRUE)
mean(kataster$nn_dist_m, na.rm = TRUE)
median(kataster$nn_dist_m, na.rm = TRUE)
quantile(kataster$nn_dist_m, probs = c(0.5, 0.75, 0.9), na.rm = TRUE)

hist(kataster$nn_dist_m,
     breaks = 30,
     main = "Spatial error RS vs cadaster",
     xlab = "Distance to NN (m)")

kataster_far <- kataster[kataster$nn_dist_m > 15, ]
kataster_far
