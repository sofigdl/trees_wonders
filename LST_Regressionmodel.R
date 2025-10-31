# Load Libraries
pacman::p_load(vip, dplyr,sf,ggplot2, st, terra, exactextractr, ranger,
               landscapemetrics, spdep, FNN, car, MASS, glmnet, spdep, 
               caret, GWmodel, iml, osmdata, randomForest)

#Cargar capas
zensus<-vect("D:/3-Paper/Zensus_MR.gpkg")
ext <- ext(zensus)
r<-rast(ext, resolution = 100, crs = crs(zensus))
grid <- as.polygons(r)
grid$grid_id <- 1:nrow(grid)
grid$cell_area <- expanse(grid, unit = "m")

###############################################################################
#                                   LST
###############################################################################
LST20<-rast("D:/3-Paper/LST_D_2020-06-01_2020-09-01.tif")
LST20 <- project(LST20, crs(grid))
LST21<-rast("D:/3-Paper/LST_D_2021-06-01_2021-09-01.tif")
LST21 <- project(LST21, crs(grid))
LST22<-rast("D:/3-Paper/LST_D_2022-06-01_2022-09-01.tif")
LST22 <- project(LST22, crs(grid))
LST23<-rast("D:/3-Paper/LST_D_2023-06-01_2023-09-01.tif")
LST23 <- project(LST23, crs(grid))

# Stack the rasters
LST_stack <- c(LST20, LST21, LST22, LST23)

# Compute the pixel-wise mean (ignore NAs)
LST_mean <- app(LST_stack, fun = mean, na.rm = TRUE)

#Transform LST to °C
#LST<-LST-273.15

# Extract mean values considering fractional coverage
grid$mean_LST <- extract(LST_mean, grid, fun = mean, na.rm = TRUE)[,2]

# Extract mean values considering fractional coverage
grid$median_LST <- extract(LST_mean, grid, fun = median, na.rm = TRUE)[,2]


###############################################################################
#                                   SVF
###############################################################################

svf<-rast("D:/Test_MR/SVF.tif")

grid$SVF <- terra::extract(svf, grid, fun = mean, na.rm = TRUE)[2]

###############################################################################
#                                Spectral
###############################################################################

#Load co-registered raster
mr_s21<- rast("D:/Preprocessed/New_preprocessing/cor_21SEP_MR.tif")
mr_j19<- rast("D:/Preprocessed/Clipped/clip-19JUL-WV2-middle.tif")
mr_j19<-resample(mr_j19, mr_s21)


#Create the functions for the indices
#NDVI
ndvi<-function(s){
  nir=subset(s, 7) #pay attention to the band order in WV2
  red=subset(s, 5) 
  
  (nir-red)/(nir+red)
}

#apply the ndvi
ndvi_mrj19<- ndvi(mr_j19)
ndvi_mrs21<- ndvi(mr_s21)

grid$ndvi19 <- terra::extract(ndvi_mrj19, grid, fun = mean, na.rm = TRUE)[2]
grid$ndvi21 <- terra::extract(ndvi_mrs21, grid, fun = mean, na.rm = TRUE)[2]


###############################################################################
#                                   Trees
###############################################################################
tree_points<-vect("D:/Simulations/trees_32632.gpkg")

##Number of trees
# Add a dummy column to count
tree_points$count <- 1

# Aggregate by summing the dummy column = point count
tree_metrics <- zonal(tree_points["count"], grid, fun = sum, na.rm = FALSE)

# Join the results back to the full grid
grid$num_trees <- tree_metrics[match(grid$grid_id, tree_metrics$zone), 2]

# Replace NA (cells with no trees) with 0
grid$num_trees[is.na(grid$num_trees)] <- 0

###Repeat with all the variables

#Mean dbh
#tree_metrics <- zonal(tree_points["dbh"], grid, fun = mean, na.rm = FALSE)
#grid$dbh_mean <- tree_metrics[match(grid$grid_id, tree_metrics$zone), 2]
#grid$dbh_mean[is.na(grid$dbh_mean)] <- 0

#Mean CPA
tree_metrics <- zonal(tree_points["CPA"], grid, fun = mean, na.rm = FALSE)
grid$CPA_mean <- tree_metrics[match(grid$grid_id, tree_metrics$zone), 2]
grid$CPA_mean[is.na(grid$CPA_mean)] <- 0

#Mean height
tree_metrics <- zonal(tree_points["height"], grid, fun = mean, na.rm = FALSE)
grid$t_height_mean <- tree_metrics[match(grid$grid_id, tree_metrics$zone), 2]
grid$t_height_mean[is.na(grid$t_height_mean)] <- 0

#Mean cd
tree_metrics <- zonal(tree_points["cd"], grid, fun = mean, na.rm = FALSE)
grid$cd_mean <- tree_metrics[match(grid$grid_id, tree_metrics$zone), 2]
grid$cd_mean[is.na(grid$cd_mean)] <- 0

#Mean bti
#tree_metrics <- zonal(tree_points["Bti"], grid, fun = sum, na.rm = FALSE)
#grid$bti_sum <- tree_metrics[match(grid$grid_id, tree_metrics$zone), 2]
#grid$bti_sum[is.na(grid$bti_mean)] <- 0

#Mean cool(sh) su
#tree_metrics <- zonal(tree_points["cool(sh) su"], grid, fun = mean, na.rm = FALSE)
#grid$cool_sh_mean <- tree_metrics[match(grid$grid_id, tree_metrics$zone), 2]
#grid$cool_sh_mean[is.na(grid$cool_sh_mean)] <- 0

#Mean cool(tr) su
#tree_metrics <- zonal(tree_points["cool(tr) su"], grid, fun = mean, na.rm = FALSE)
#grid$cool_tr_mean <- tree_metrics[match(grid$grid_id, tree_metrics$zone), 2]
#grid$cool_tr_mean[is.na(grid$cool_tr_mean)] <- 0

#Tree species

# Spatial join: assign each tree point to a grid cell
trees_in_grid <- terra::intersect(tree_points, grid)

# Convert to data.frame for summarizing
trees_df <- as.data.frame(trees_in_grid)

# Count number of trees per species per grid cell
species_count <- trees_df %>%
  group_by(grid_id = grid_id, species) %>%     # replace 'ID' with your grid's ID column name
  summarize(count = n(), .groups = "drop")

species_percentage <- species_count %>%
  group_by(grid_id) %>%
  mutate(percentage = 100 * count / sum(count)) %>%
  ungroup()

# Filter for Acer 
acer_cells <- as.data.frame(species_percentage %>%
  filter(species == "Acer pseudoplatanus"))

# Join the results back to the full grid
grid$num_acer <- acer_cells[match(grid$grid_id, acer_cells$grid_id), 3]
grid$num_acer[is.na(grid$num_acer)] <- 0

grid$per_acer <- acer_cells[match(grid$grid_id, acer_cells$grid_id), 4]
grid$per_acer[is.na(grid$per_acer)] <- 0

tilia_cells <- as.data.frame(species_percentage %>%
                              filter(species == "Tilia cordata"))

# Join the results back to the full grid
grid$num_tilia <- tilia_cells[match(grid$grid_id, tilia_cells$grid_id), 3]
grid$num_tilia[is.na(grid$num_tilia)] <- 0
grid$per_tilia <- tilia_cells[match(grid$grid_id, tilia_cells$grid_id), 4]
grid$per_tilia[is.na(grid$per_tilia)] <- 0

aesculus_cells <- as.data.frame(species_percentage %>%
                              filter(species == "Aesculus hippocastanum"))

# Join the results back to the full grid
grid$num_aesculus <- aesculus_cells[match(grid$grid_id, aesculus_cells$grid_id), 3]
grid$num_aesculus[is.na(grid$num_aesculus)] <- 0
grid$per_aesculus <- aesculus_cells[match(grid$grid_id, aesculus_cells$grid_id), 4]
grid$per_aesculus[is.na(grid$per_aesculus)] <- 0

robin_cells <- as.data.frame(species_percentage %>%
                              filter(species == "Robinia pseudoacacia"))

# Join the results back to the full grid
grid$num_robin <- robin_cells[match(grid$grid_id, robin_cells$grid_id), 3]
grid$num_robin[is.na(grid$num_robin)] <- 0
grid$per_robin <- robin_cells[match(grid$grid_id, robin_cells$grid_id), 4]
grid$per_robin[is.na(grid$per_robin)] <- 0

popul_cells <- as.data.frame(species_percentage %>%
                              filter(species == "Populus nigra 'Italica'"))

# Join the results back to the full grid
grid$num_popul <- popul_cells[match(grid$grid_id, popul_cells$grid_id), 3]
grid$num_popul[is.na(grid$num_popul)] <- 0
grid$per_popul <- popul_cells[match(grid$grid_id, popul_cells$grid_id), 4]
grid$per_popul[is.na(grid$per_popul)] <- 0

other_cells <- as.data.frame(species_percentage %>%
                              filter(species == "Fraxinus excelsior"))

# Join the results back to the full grid
grid$num_other <- other_cells[match(grid$grid_id, other_cells$grid_id), 3]
grid$num_other[is.na(grid$num_other)] <- 0
grid$per_other <- other_cells[match(grid$grid_id, other_cells$grid_id), 4]
grid$per_other[is.na(grid$per_other)] <- 0

###############################################################################
#                           Tree configuration
###############################################################################
#canopy<-vect("D:/Trees_data/Tree_segments_MR_LU.gpkg")
canopy<-rast("D:/Classifications/2.2_trees_MR_nobuildings.tif")
#Calculate percentage of canopy cover
canopy_area <- zonal(canopy, grid, fun = sum, na.rm = TRUE)
cell_area <- res(canopy)[1] * res(canopy)[2]
canopy_area$canopy_area <- canopy_area$`2.2_trees_MR_nobuildings` * cell_area
grid$canopy_area <- canopy_area$canopy_area
grid$canopy_area[is.na(grid$canopy_area)] <- 0
grid$canopy_perc <- (grid$canopy_area / grid$cell_area) * 100


# Mean nearest-neighbor distance for trees inside each cell 
#coords <- crds(tree_points)
#nn <- get.knn(coords, k = 1)
#tree_points$nn_dist <- nn$nn.dist
#tree_metrics <- zonal(tree_points["nn_dist"], grid, fun = mean, na.rm = FALSE)
#grid$mean_nn <- tree_metrics[match(grid$grid_id, tree_metrics$zone), 2]
#grid$mean_nn[is.na(grid$mean_nn)] <- 0


#Clustering
# create neighbors for grid (queen or rook)
#nb <- poly2nb(st_as_sf(grid), queen = TRUE)
#lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# local Moran (LISA) on canopy_prop
#local_moran <- localmoran(grid$canopy_perc, lw)
#grid$local_moran_I <- local_moran[,1]
#grid$local_moran_p <- local_moran[,5]




###############################################################################
#                           Landscape metrics
###############################################################################

#Percentage of green areas

green<-rast("D:/Classifications/1_vegetation_MR.tif")
green <- project(green, "EPSG:32632")
#green<-as.polygons(green)
#Calculate percentage of canopy cover
#green_in_grid <- intersect(grid, green)
#green_in_grid$green_area <- expanse(green_in_grid, unit = "m") 
green_frac <- exact_extract(green, grid, 'mean')
green_fraction <- zonal(green, grid, fun = mean, na.rm = TRUE)
cell_area <- res(green)[1] * res(green)[2]
green_area$green_area <- green_area$layer * cell_area
grid$green_area <- green_area$green_area
grid$green_area[is.na(grid$green_area)] <- 0
grid$green_perc <- (grid$green_area / grid$cell_area) * 100


grid$veg<-grid$green_area - grid$canopy_area
grid$veg_perc<-(grid$veg / grid$cell_area) * 100
###############################################################################
#                               Built-up
###############################################################################

buildings<-vect("C:/Users/ang58gl/Documents/Data/buildings_MR.gpkg")
ndsm <-rast("D:/nDSM_Muc.tif")
# Extract mean values considering fractional coverage
buildings <- buildings[, c("_id", "building", "building_levels", "roof_shape")]
buildings$median_height <- extract(ndsm, buildings, fun = median, na.rm = TRUE)[,2]
buildings$area<-expanse(buildings)

#Building heights
building_metrics <- zonal(buildings["median_height"], grid, fun = mean, na.rm = FALSE)
grid$b_height_mean <- building_metrics[match(grid$grid_id, building_metrics$zone), 2]
grid$b_height_mean[is.na(grid$b_height_mean)] <- 0

#Building heights
building_metrics <- zonal(buildings["area"], grid, fun = sum, na.rm = FALSE)
grid$b_area_sum <- building_metrics[match(grid$grid_id, building_metrics$zone), 2]
grid$b_area_sum[is.na(grid$b_area_sum)] <- 0


#Spacing between buildings
#Estimate minimum distance between buildings.
n <- nrow(buildings)
edge_distances <- numeric(n)

#for (i in seq_len(n)) {
#  dists <- distance(buildings[i, ], buildings[-i, ])
#  dists_vec <- as.numeric(dists)
#  edge_distances[i] <- min(dists_vec, na.rm = TRUE)
#}

dist_poly <- sapply(1:nrow(buildings), function(i) {
     d <- min(distance(buildings[i,], buildings[-i,]))
     return(d)
  })

buildings$edge_distance <- edge_distances
building_metrics <- zonal(buildings["edge_distance"], grid, fun = mean, na.rm = FALSE)
grid$mean_b_dist2 <- building_metrics[match(grid$grid_id, building_metrics$zone), 2]
grid$mean_b_dist2[is.na(grid$mean_b_dist2)] <- 250

#Method 2
# Rasterize buildings (1 = built, NA = not built)
r <- rast(buildings, res = 1)  # adjust resolution
build_r <- rasterize(buildings, r, field = 1)

# Compute Euclidean distance from each pixel to the nearest building
dist_r <- distance(build_r)

# Mask distance outside of urban area if needed
dist_r <- mask(dist_r, build_r, inverse = TRUE)

# Compute mean inter-building distance
grid$mean_b_dist1 <- extract(dist_r, grid, fun = mean, na.rm = TRUE)[,2]



#Road density
#define the aoi as st
aoi<-st_bbox(st_transform(st_as_sf(grid), 4326))

#Get the data from OSM
query<-opq(bbox =aoi) |>
  add_osm_feature(key = 'highway')
roads_osm<- osmdata_sf(query)$osm_lines

#Transform back to Spatvect
roads<-vect(st_transform(roads_osm, st_crs(st_as_sf(grid))))

roads<- roads[,c("osm_id", "name", "highway", "lanes", "maxspeed")]

roads_in_grid<- intersect(roads, grid)

# Compute lengths (in meters)
roads_in_grid$length_m <-perim(roads_in_grid)

#writeVector(roads_in_grid, "D:/3-Paper/roads_grid.gpkg")

# Aggregate by grid ID (replace "ID" with your actual grid ID field)
road_density <- zonal(roads_in_grid["length_m"], grid, fun = sum, na.rm = TRUE)
grid$r_length <- road_density[match(grid$grid_id, road_density$zone), 2]
grid$r_length[is.na(grid$r_length)] <-0
grid$r_dens <- grid$r_length/grid$cell_area
grid$r_dens[is.na(grid$r_dens)] <-0
###############################################################################
#                            Distance to water
###############################################################################

water<-vect("D:/3-Paper/Water_1.gpkg")
grid_centroids <- centroids(grid)

# Calculate distance from each centroid to the water bodies
dist_to_water <- distance(grid_centroids, water)
grid$dist_water <- apply(dist_to_water, 1, min)

#Calculate percentage of water in each cell
#water_in_grid <- intersect(grid, water)
#water_in_grid$water_area <- expanse(water_in_grid, unit = "m") 
#water_area_per_cell <- zonal(water_in_grid["water_area"], grid, fun = sum, na.rm = TRUE)
#grid$water_area <- water_area_per_cell$water_area
#grid$water_area[is.na(grid$water_area)] <- 0
#grid$water_perc <- (grid$water_area / grid$cell_area) * 100

###############################################################################
#                            Regression
###############################################################################
df <- as.data.frame(grid)
# Remove all rows with NA in predictors or response
df <- na.omit(df)

#model_lm <- lm(mean_LST ~ num_trees + dbh_mean + CPA_mean + t_height_mean + cd_mean +
#bti_mean + cool_sh_mean + cool_tr_mean +  canopy_area + canopy_perc + mean_nn+local_moran_I + 
#  local_moran_p + b_height_mean + b_area_sum + dist_water + water_area + water_perc + 
#  veg, data = df)

#summary(model_lm)



#X <- model.matrix(mean_LST ~ num_trees + dbh_mean + CPA_mean + t_height_mean + cd_mean +
#                    bti_mean + cool_sh_mean + cool_tr_mean + canopy_area +
#                    canopy_perc + local_moran_I + local_moran_p + mean_nn +
#                    b_height_mean + b_area_sum + dist_water + water_area + water_perc + veg,
#                  data = df)[, -1]  # remove intercept
#y <- df$mean_LST

# Cross-validated LASSO
#lasso_cv <- cv.glmnet(X, y, alpha = 1, standardize = TRUE)
#plot(lasso_cv)

# Coefficients at best lambda
#coef_lasso<-coef(lasso_cv, s = "lambda.min")
#coef_lasso

# Convert to data.frame
#coef_df <- data.frame(
#  term = rownames(coef_lasso),
#  estimate = as.numeric(coef_lasso))

# Drop intercept and zeros
#coef_df <- coef_df %>%
#  dplyr::filter(term != "(Intercept)", estimate != 0) %>%
#  dplyr::arrange(desc(abs(estimate)))

# Print ranked coefficients
#print(coef_df)

# Plot importance
#ggplot(coef_df, aes(x = reorder(term, abs(estimate)), y = estimate, fill = estimate > 0)) +
#  geom_col() +
#  scale_fill_manual(values = c("TRUE" = "orange", "FALSE" = "steelblue")) +
#  coord_flip() +
#  labs(x = "Predictor", y = "Coefficient (standardized scale)",
#       title = "Variable importance from LASSO")

# Refit glmnet as a caret model (so vip works)
#lasso_fit <- glmnet(X, y, alpha = 1, lambda = lasso_cv$lambda.min)


#df<-df[, c("mean_LST", "num_trees", "dbh_mean", "CPA_mean", "t_height_mean", "cd_mean",                 
#"bti_mean", "cool_sh_mean", "cool_tr_mean", "canopy_area",                   
#"canopy_perc", "local_moran_I", "local_moran_p",                
#"b_height_mean", "b_area_sum", "dist_water", "water_area", "water_perc", "veg")]

df<-df[, c("mean_LST", "SVF", "ndvi19", "ndvi21", "num_trees", "CPA_mean", "t_height_mean",
           "cd_mean", "num_acer", "per_acer", "num_tilia", "per_tilia", "num_robin", "per_robin",
           "num_aesculus", "per_aesculus", "num_popul", "per_popul", "num_other", "per_other", 
           "canopy_area", "canopy_perc", "green_area", "veg", "green_perc", "b_height_mean",
           "b_area_sum", "mean_b_dist2",  "mean_b_dist1", "r_dens", "dist_water")]


set.seed(123)

ctrl <- trainControl(method = "cv", number = 5)  # 5-fold CV

# Fit Random Forest with CV
rf_fit <- train(
  mean_LST ~ .,
  data = df,
  method = "rf",
  type="regression",
  trControl = ctrl,
  tuneLength = 5,
  importance=TRUE)

# View results
rf_fit
plot(rf_fit)
var_imp <- varImp(rf_cv, scale = TRUE)
ggplot(var_imp, top = 15) +
  labs(title = "Variable Importance (Random Forest with Cross-Validation)",
       x = "Importance", y = "")

#rf_fit <- ranger(
#  formula = mean_LST ~ ., 
#  data = df, 
#  importance = "permutation",  # or "impurity"
#  num.trees = 500,
#  mtry = floor(sqrt(ncol(df) - 1)),  # typical RF default
#  min.node.size = 5
#)


vip(rf_cv, num_features = 15, aesthetics = list(fill = "darkgreen")) +
  labs(title = "Random Forest Variable Importance (Permutation)")




# Wrap the model
predictor <- Predictor$new(
  model = rf_fit, 
  data = df[, -which(names(df) == "mean_LST")], 
  y = df$mean_LST
)

# Partial dependence for "num_trees"
pdp_numtrees <- FeatureEffect$new(predictor, feature = "b_area_sum", method = "pdp")
plot(pdp_numtrees)



set.seed(123)
rf_fit <- ranger(
  formula = mean_LST ~ num_trees + dbh_mean + CPA_mean + t_height_mean + cd_mean +
    bti_mean + cool_sh_mean + cool_tr_mean +
    b_height_mean + dist_water + green_area + veg, 
  data = df, 
  importance = "permutation",  # or "impurity"
  num.trees = 500,
  mtry = floor(sqrt(ncol(df) - 1)),  # typical RF default
  min.node.size = 5
)


vip(rf_fit, num_features = 10, aesthetics = list(fill = "darkgreen")) +
  labs(title = "Random Forest Variable Importance (Permutation)")

###############################################################################
#                                    GWR
###############################################################################
# Remove rows with NA in LST or predictors
vars <- c("mean_LST", "num_trees", "dbh_mean", "CPA_mean", "t_height_mean", "cd_mean",                 
          "bti_mean", "cool_sh_mean", "cool_tr_mean", "canopy_area",                   
          "canopy_perc", "local_moran_I", "local_moran_p",                
          "b_height_mean", "b_area_sum", "dist_water", "water_area", "water_perc", "veg")

grid_clean <- grid[complete.cases(as.data.frame(grid)[, vars]), ]

# Check consistency
nrow(grid_clean) == nrow(as.data.frame(grid_clean))  


grid_sp <- as(grid_clean, "Spatial")

bw <- bw.gwr(mean_LST ~ num_trees + dbh_mean + CPA_mean + t_height_mean + cd_mean +
               bti_mean + cool_sh_mean + cool_tr_mean + canopy_area  + local_moran_I + 
               b_height_mean + b_area_sum + dist_water + veg,
             data = grid_sp,
             approach = "AICc",
             kernel = "bisquare",
             adaptive =TRUE)

gwr_fit <- gwr.basic(mean_LST ~ num_trees + dbh_mean + CPA_mean + t_height_mean + cd_mean +
                     bti_mean + cool_sh_mean + cool_tr_mean + canopy_area +  local_moran_I +
                     b_height_mean + b_area_sum + dist_water + veg,
                     data = grid_sp,
                     bw = bw,
                     kernel = "bisquare",
                     adaptive = TRUE)

coef_df<- as.data.frame(gwr_fit$SDF)

# Add local coefficients to your SpatVector grid
grid_clean$coef_num_trees   <- coef_df$num_trees
grid_clean$coef_dbh_mean    <- coef_df$dbh_mean
grid_clean$coef_CPA_mean    <- coef_df$CPA_mean
grid_clean$coef_t_height    <- coef_df$t_height_mean
grid_clean$coef_cd_mean     <- coef_df$cd_mean
grid_clean$coef_bti_mean    <- coef_df$bti_mean
grid_clean$coef_cool_sh     <- coef_df$cool_sh_mean
grid_clean$coef_cool_tr     <- coef_df$cool_tr_mean
grid_clean$coef_b_height    <- coef_df$b_height_mean
grid_clean$coef_b_area_sum    <- coef_df$b_area_sum
grid_clean$coef_dist_water  <- coef_df$dist_water
grid_clean$coef_green_area  <- coef_df$green_area
grid_clean$coef_veg         <- coef_df$veg
grid_clean$local_R2 <- coef_df$Local_R2

plot(grid_clean["local_R2"])



ggplot(st_as_sf(grid_clean)) +
  geom_sf(aes(fill = local_R2)) +
  scale_fill_distiller(palette = "YlGnBu", direction = 0) +
  labs(title = "Local GWR R2")

ggplot(st_as_sf(grid_clean)) +
  geom_sf(aes(fill = coef_num_trees)) +
  scale_fill_gradient2(
    low = "#125CA6",mid = "#FFFFCC", high = "#BF4600"
  ) +
  labs(title = "Local coefficient: Tree density → LST",
       fill = "Coefficient")

ggplot(st_as_sf(grid_clean)) +
  geom_sf(aes(fill = coef_veg)) +
  scale_fill_gradient2(
    low = "#125CA6",mid = "#FFFFCC", high = "#BF4600"
  ) +
  labs(title = "Local coefficient: Other vegetation density → LST",
       fill = "Coefficient")

ggplot(st_as_sf(grid_clean)) +
  geom_sf(aes(fill = coef_b_height)) +
  scale_fill_gradient2(
    low = "#125CA6",mid = "#FFFFCC", high = "#BF4600"
  ) +
  labs(title = "Local coefficient: Mean building height → LST",
       fill = "Coefficient")


ggplot(st_as_sf(grid_clean)) +
  geom_sf(aes(fill = coef_b_area_sum)) +
  scale_fill_gradient2(
    low = "#125CA6",mid = "#FFFFCC", high = "#BF4600"
  ) +
  labs(title = "Local coefficient: Building footprint → LST",
       fill = "Coefficient")

ggplot(st_as_sf(grid_clean)) +
  geom_sf(aes(fill = coef_cool_tr)) +
  scale_fill_gradient2(
    low = "#125CA6",mid = "#FFFFCC", high = "#BF4600"
  ) +
  labs(title = "Local coefficient: Cooling by transpiration → LST",
       fill = "Coefficient")

ggplot(st_as_sf(grid_clean)) +
  geom_sf(aes(fill = coef_cool_sh)) +
  scale_fill_gradient2(
    low = "#125CA6",mid = "#FFFFCC", high = "#BF4600"
  ) +
  labs(title = "Local coefficient: Cooling by shading → LST",
       fill = "Coefficient")

