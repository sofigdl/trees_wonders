# Load Libraries
pacman::p_load(vip, dplyr,sf,ggplot2, st, terra, exactextractr, ranger,
               landscapemetrics, spdep, FNN, car, MASS, glmnet, spdep, 
               caret, GWmodel, iml, osmdata, randomForest)

###############################################################################
#                                   LST
###############################################################################
LST20<-rast("D:/3-Paper/2020-06-01_2020-09-01_Downscaled_LST_NoResiduals.tif")
LST20 <- project(LST20, "EPSG:32632")
LST21<-rast("D:/3-Paper/2021-06-01_2021-09-01_Downscaled_LST_NoResiduals.tif")
LST21 <- project(LST21, "EPSG:32632")
LST22<-rast("D:/3-Paper/2022-06-01_2022-09-01_Downscaled_LST_NoResiduals.tif")
LST22 <- project(LST22, "EPSG:32632")
LST23<-rast("D:/3-Paper/2023-06-01_2023-09-01_Downscaled_LST_NoResiduals.tif")
LST23 <- project(LST23, "EPSG:32632")
LST24<-rast("D:/3-Paper/2024-06-01_2024-09-01_Downscaled_LST_NoResiduals.tif")
LST24 <- project(LST24, "EPSG:32632")
LST25<-rast("D:/3-Paper/2025-06-01_2025-09-01_Downscaled_LST_NoResiduals.tif")
LST25 <- project(LST25, "EPSG:32632")

# Stack the rasters
LST_stack <- c(LST20, LST21, LST22, LST23, LST24, LST25)

# Compute the pixel-wise mean (ignore NAs)
LST_mean <- app(LST_stack, fun = mean, na.rm = TRUE)

##### At the end we find yearly anomalies/sensitivity of LST and modeling predicted LST with mean_LST

###############################################################################
#                                   Grid
###############################################################################



grid <- as.polygons(LST_mean, dissolve = FALSE)
grid$grid_id <- 1:nrow(grid)
grid$cell_area <- expanse(grid, unit = "m")
grid$mean <- NULL

# Extract mean values considering fractional coverage
grid$mean_LST <- extract(LST_mean, grid, fun = mean, na.rm = TRUE)[,2]

# Extract mean values considering fractional coverage
#rid$median_LST <- extract(LST_mean, grid, fun = median, na.rm = TRUE)[,2]

###############################################################################
#                                   SVF
###############################################################################

svf<-rast("D:/Test_MR/SVF.tif")

grid$SVF <- terra::extract(svf, grid, fun = mean)[2]

###############################################################################
#                                Spectral
###############################################################################

#Load co-registered raster
#mr_s21<- rast("D:/Preprocessed/New_preprocessing/cor_21SEP_MR.tif")
#mr_j19<- rast("D:/Preprocessed/Clipped/clip-19JUL-WV2-middle.tif")
#mr_j19<-resample(mr_j19, mr_s21)


#Create the functions for the indices
#NDVI
#ndvi<-function(s){
#  nir=subset(s, 7) #pay attention to the band order in WV2
#  red=subset(s, 5) 
  
#  (nir-red)/(nir+red)
#}

#apply the ndvi
#ndvi_mrj19<- ndvi(mr_j19)
#ndvi_mrs21<- ndvi(mr_s21)

#grid$ndvi19 <- terra::extract(ndvi_mrj19, grid, fun = mean, na.rm = TRUE)[2]
#grid$ndvi21 <- terra::extract(ndvi_mrs21, grid, fun = mean, na.rm = TRUE)[2]


###############################################################################
#                                   Trees
###############################################################################
tree_points<-vect("D:/Simulations/trees_32632.gpkg")

##Number of trees
# Add a dummy column to count
tree_int <- intersect(tree_points, grid)

# Aggregate by summing the dummy column = point count
tree_metrics <- aggregate(tree_int, fun = mean, by= "grid_id", count = TRUE)
tree_metrics <- as.data.frame(tree_metrics)
tree_metrics <- tree_metrics[, c("grid_id","agg_dbh", "agg_height", "agg_cd", "agg_CPA", "agg_LAI", "agg_n", "species")]

# Join the results back to the full grid
grid$num_trees <- tree_metrics[match(grid$grid_id, tree_metrics$grid_id), 7]

# Replace NA (cells with no trees) with 0
grid$num_trees[is.na(grid$num_trees)] <- 0

grid$t_ratio<-grid$num_trees/grid$cell_area

###Repeat with all the variables

#Mean dbh
#tree_metrics <- zonal(tree_points["dbh"], grid, fun = mean, na.rm = FALSE)
#grid$dbh_mean <- tree_metrics[match(grid$grid_id, tree_metrics$zone), 2]
#grid$dbh_mean[is.na(grid$dbh_mean)] <- 0

#Mean CPA
grid$CPA_mean <- tree_metrics[match(grid$grid_id, tree_metrics$grid_id), 5]
grid$CPA_mean[is.na(grid$CPA_mean)] <- 0

#Mean height
grid$t_height_mean <- tree_metrics[match(grid$grid_id, tree_metrics$grid_id), 3]
grid$t_height_mean[is.na(grid$t_height_mean)] <- 0

#Mean cd
grid$cd_mean <- tree_metrics[match(grid$grid_id, tree_metrics$grid_id), 4]
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

#Distance to trees

grid_centroids <- centroids(grid)

# Calculate distance from each centroid to the water bodies
dist_to_trees <- nearest(grid_centroids, tree_points)
grid$dist_tr <- dist_to_trees$distance


#Tree species

# Convert to data.frame for summarizing
trees_df <- as.data.frame(tree_int)

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
canopy[is.na(canopy)] <- 0
#Calculate percentage of canopy cover
grid_rast <- rasterize(grid, canopy, field = "grid_id")
# 4. Calculate proportion of green pixels in each grid cell
canopy_frac <- zonal(canopy, grid_rast, fun = "mean", na.rm = TRUE)
grid$canopy_perc <- canopy_frac$`2.2_trees_MR_nobuildings`[match(grid$grid_id, canopy_frac$grid_id)] * 100
grid$canopy_area <- (grid$canopy_perc / 100) * grid$cell_area


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


# 3. Rasterize grid so zonal() has a matching structure
#grid_rast <- rasterize(grid, green, field = "grid_id")
# 4. Calculate proportion of green pixels in each grid cell
green_frac <- zonal(green, grid_rast, fun = "mean", na.rm = TRUE)

grid$green_perc <- green_frac$layer[match(grid$grid_id, green_frac$grid_id)] * 100
grid$green_perc[is.na(grid$green_perc)] <- 0
grid$green_area <- (grid$green_perc / 100) * grid$cell_area
grid$green_area[is.na(grid$green_area)] <- 0

grid$veg<-grid$green_area - grid$canopy_area
grid$veg_perc<-(grid$veg / grid$cell_area) * 100

###############################################################################
#                               Built-up
###############################################################################

buildings<-vect("C:/Users/ang58gl/Documents/Data/buildings_MR.gpkg")
project(buildings, crs(grid))
buildings <- makeValid(buildings)
buildings_single <- disagg(buildings)
g <- geom(buildings_single)
geom_hash <- tapply(
  paste(round(g[, "x"], 5), round(g[, "y"], 5)),
  g[, "geom"], # group by geometry ID
  paste, collapse = "_"
)

dup_idx <- duplicated(geom_hash)
buildings_clean <- subset(buildings_single, !dup_idx)

buildings_clean <- buildings_clean[, c("_id", "building", "building_levels", "roof_shape")]


#Building area
# Compute intersection polygons
b_diss <- aggregate(buildings_clean, dissolve=TRUE)
b_int <- intersect(grid, b_diss)
b_int$int_area <- expanse(b_int, unit="m")
building_metrics <- as.data.frame(aggregate(b_int, by = "grid_id", fun = sum))
grid$b_area_sum <- building_metrics[match(grid$grid_id, building_metrics$grid_id), 28]
grid$b_area_sum[is.na(grid$b_area_sum)] <- 0
grid$b_ratio<-grid$b_area_sum/grid$cell_area



ndsm <-rast("D:/nDSM_Muc.tif")
# Extract mean values considering fractional coverage
buildings_clean$median_height <- extract(ndsm, buildings_clean, fun = median, na.rm = TRUE)[,2]

#Building heights
building_metrics <- zonal(buildings_clean["median_height"], grid, fun = mean, na.rm = FALSE)
grid$b_height_mean <- building_metrics[match(grid$grid_id, building_metrics$zone), 2]
grid$b_height_mean[is.na(grid$b_height_mean)] <- 0


#Spacing between buildings
#Estimate minimum distance between buildings.
n <- nrow(buildings_clean)
edge_distances <- numeric(n)

#for (i in seq_len(n)) {
#  dists <- distance(buildings[i, ], buildings[-i, ])
#  dists_vec <- as.numeric(dists)
#  edge_distances[i] <- min(dists_vec, na.rm = TRUE)
#}

dist_poly <- sapply(1:nrow(buildings_clean), function(i) {
     d <- min(distance(buildings_clean[i,], buildings_clean[-i,]))
     return(d)
  })

buildings_clean$edge_distance <- dist_poly
building_metrics <- zonal(buildings_clean["edge_distance"], grid, fun = mean, na.rm = FALSE)
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
aoi<-(st_transform(st_as_sf(grid), 4326))
bbox<-st_bbox(aoi)


set_overpass_url("https://overpass-api.de/api/interpreter")

#Get the data from OSM
query<-opq(bbox = bbox, timeout = 120) |>
  add_osm_feature(key = "highway", value = c(
    "motorway", "trunk", "primary", "secondary", "tertiary",
    "unclassified", "residential", "service"
  ))

roads_osm<- osmdata_sf(query)$osm_lines

#Transform back to Spatvect
roads<-vect(st_transform(roads_osm, st_crs(st_as_sf(grid))))

roads<- roads[,c("osm_id", "name", "highway", "lanes", "maxspeed")]

roads_in_grid<- intersect(roads, grid)

# Compute lengths (in meters)
roads_in_grid$length_m <-perim(roads_in_grid)


# Aggregate by grid ID (replace "ID" with your actual grid ID field)
road_density <- as.data.frame(aggregate(roads_in_grid, by = "grid_id", fun = sum))
names(road_density)
#road_density <- zonal(roads_in_grid["length_m"], grid, fun = sum, na.rm = TRUE)
grid$r_length <- road_density[match(grid$grid_id, road_density$grid_id), 32]
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

#writeVector(grid, "D:/3-Paper/grid.gpkg")
###############################################################################
#                            Regression
###############################################################################
#grid <- vect("D:/3-Paper/grid.gpkg")

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


#Select variables and generate df

#df<-df[, c("mean_LST", "SVF", #"ndvi19", "ndvi21",
#           "num_trees", "CPA_mean", "t_height_mean",
#           "cd_mean", "num_acer", "per_acer", "num_tilia", "per_tilia", "num_robin", "per_robin",
#           "num_aesculus", "per_aesculus", "num_popul", "per_popul", "num_other", "per_other", 
#           "canopy_area", "canopy_perc", "green_area", "veg", "green_perc", "b_height_mean",
#           "b_area_sum", "mean_b_dist2",  "mean_b_dist1", "r_dens")]#, "dist_water")]

df<-df[, c("mean_LST", "SVF", "t_ratio", "CPA_mean", "t_height_mean",
           "cd_mean", "per_acer", "per_tilia", "per_other", "per_robin", "per_aesculus",
           "canopy_perc", "veg_perc", "green_perc", "b_height_mean",
           "b_ratio", "mean_b_dist2",  "mean_b_dist1", "r_dens", "dist_water", "dist_tr")]


set.seed(123)

#Crossvalidation and regression model

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
var_imp <- varImp(rf_fit, scale = FALSE)
ggplot(var_imp, top = 15) +
  labs(title = "Variable Importance (Random Forest with Cross-Validation)",
       x = "Variable", y = "Importance")

#rf_fit <- ranger(
#  formula = mean_LST ~ ., 
#  data = df, 
#  importance = "permutation",  # or "impurity"
#  num.trees = 500,
#  mtry = floor(sqrt(ncol(df) - 1)),  # typical RF default
#  min.node.size = 5
#)

#Variable importance in %IncMSE
importance(rf_fit$finalModel)

vip(rf_fit, num_features = 15, aesthetics = list(fill = "darkgreen")) +
  labs(title = "Random Forest Variable Importance (Permutation)")

###############################################################################
#                          Partial Dependence Plots
###############################################################################

## Partial dependence plots

# Wrap the model
predictor <- Predictor$new(
  model = rf_fit, 
  data = df[, -which(names(df) == "mean_LST")], 
  y = df$mean_LST
)

# Partial dependence for "num_trees"

pdp_green <- FeatureEffect$new(predictor, feature = "green_perc", method = "pdp")
plot(pdp_green)

pdp_water <- FeatureEffect$new(predictor, feature = "dist_water", method = "pdp")
plot(pdp_water)

pdp_tilia <- FeatureEffect$new(predictor, feature = "per_tilia", method = "pdp")
plot(pdp_tilia)

pdp_treeheight <- FeatureEffect$new(predictor, feature = "t_height_mean", method = "pdp")
plot(pdp_treeheight)

pdp_build <- FeatureEffect$new(predictor, feature = "b_ratio", method = "pdp")
plot(pdp_build)

pdp_rdens <- FeatureEffect$new(predictor, feature = "r_dens", method = "pdp")
plot(pdp_rdens)

pdp_bdist <- FeatureEffect$new(predictor, feature = "mean_b_dist1", method = "pdp")
plot(pdp_bdist)

pdp_canopy <- FeatureEffect$new(predictor, feature = "canopy_perc", method = "pdp")
plot(pdp_canopy)


###############################################################################
#                                    GWR
###############################################################################
# Remove rows with NA in LST or predictors
vars <- c("mean_LST", "SVF", "t_ratio", "CPA_mean", "t_height_mean",
          "cd_mean", "per_acer", "per_tilia", "per_robin",
          "per_aesculus", "per_other", 
          "canopy_perc", "veg_perc", "green_perc", "b_height_mean",
          "b_ratio", "mean_b_dist2",  "mean_b_dist1", "r_dens", "dist_water")

grid_clean <- grid[complete.cases(as.data.frame(grid)[, vars]), ]


#grid_clean <- grid_clean[ , c("mean_LST", "SVF", "t_ratio", "t_height_mean",
#                            cd_mean", "per_acer", "per_tilia", "per_other", 
#                              "green_perc", "canopy_perc",
#                              "b_ratio",  "mean_b_dist1", "r_dens")]

grid_sp <- as(grid_clean, "Spatial")
crs(grid_sp) <- CRS("+init=EPSG:32632") 
scaled <- grid_sp@data

#Scale numeric columns
df_scaled <- scaled %>%
  mutate(across(where(is.numeric) & !any_of(c("mean_LST", "grid_id")),
                ~ scale(.) %>% as.vector))

nzv <- nearZeroVar(df_scaled)
df_scaled <- df_scaled[, -nzv]

# Reassign scaled attributes
grid_sp@data <- df_scaled

vif_model <- lm(mean_LST ~ green_perc +  b_ratio + mean_b_dist1 +
                  t_height_mean + per_tilia + dist_water + canopy_perc +
                  r_dens, data = as.data.frame(grid_sp@data))
car::vif(vif_model)

bw.AICcA <- bw.gwr(mean_LST ~ green_perc +  b_ratio + mean_b_dist1 +
                     t_height_mean + per_tilia + dist_water + canopy_perc +
                     r_dens,  
             data = grid_sp,
             approach = "AICc",
             kernel = "bisquare",
             adaptive = TRUE)

bw.CVA <- bw.gwr(mean_LST ~ green_perc +  b_ratio + mean_b_dist1 +
                   t_height_mean + per_tilia + dist_water + canopy_perc +
                   r_dens,  
                   data = grid_sp,
                   approach = "CV",
                   kernel = "bisquare",
                   adaptive = TRUE)


gwr_fit <- gwr.basic(mean_LST ~ green_perc +  b_ratio + mean_b_dist1 +
                       t_height_mean + per_tilia + dist_water + canopy_perc +
                       r_dens,
                     data = grid_sp,
                     bw = bw.AICcA,
                     kernel = "bisquare",
                     adaptive = TRUE)

coef_df<- as.data.frame(gwr_fit$SDF)

# Add local coefficients to your SpatVector grid
grid_clean$coef_r_dens   <- coef_df$r_dens
grid_clean$coef_t_height  <- coef_df$t_height_mean
#grid_clean$coef_CPA_mean   <- coef_df$CPA_mean
grid_clean$coef_b_ratio   <- coef_df$b_ratio
#grid_clean$coef_perc_acer <- coef_df$per_acer
grid_clean$coef_perc_tilia<- coef_df$per_tilia
#grid_clean$coef_perc_other<- coef_df$per_other
grid_clean$coef_green_perc<- coef_df$green_perc
grid_clean$coef_canopy_perc <- coef_df$canopy_perc
grid_clean$coef_dist_w  <- coef_df$dist_water
#grid_clean$coef_SVF     <- coef_df$SVF
grid_clean$coef_b_dist  <- coef_df$mean_b_dist1
grid_clean$local_R2 <- coef_df$Local_R2


ggplot(st_as_sf(grid_clean)) +
  geom_sf(aes(fill = local_R2)) +
  scale_fill_distiller(palette = "YlGnBu", direction = 0) +
  labs(title = "Local GWR R2")

ggplot(st_as_sf(grid_clean)) +
  geom_sf(aes(fill = coef_green_perc)) +
  scale_fill_gradient2(
    low = "#125CA6",mid = "#FFFFCC", high = "#BF4600"
  ) +
  labs(title = "Local coefficient: Percentage of green cover → LST",
       fill = "Coefficient")


ggplot(st_as_sf(grid_clean)) +
  geom_sf(aes(fill = coef_dist_w)) +
  scale_fill_gradient2(
    low = "#125CA6",mid = "#FFFFCC", high = "#BF4600"
  ) +
  labs(title = "Local coefficient: Mean distance to water  → LST",
       fill = "Coefficient")


ggplot(st_as_sf(grid_clean)) +
  geom_sf(aes(fill = coef_perc_tilia)) +
  scale_fill_gradient2(
    low = "#125CA6",mid = "#FFFFCC", high = "#BF4600"
  ) +
  labs(title = "Local coefficient: Percentage of Tilia → LST",
       fill = "Coefficient")


ggplot(st_as_sf(grid_clean)) +
  geom_sf(aes(fill = coef_t_height)) +
  scale_fill_gradient2(
    low = "#125CA6",mid = "#FFFFCC", high = "#BF4600"
  ) +
  labs(title = "Local coefficient: Mean tree height → LST",
       fill = "Coefficient")

ggplot(st_as_sf(grid_clean)) +
  geom_sf(aes(fill = coef_b_ratio)) +
  scale_fill_gradient2(
    low = "#125CA6",mid = "#FFFFCC", high = "#BF4600"
  ) +
  labs(title = "Local coefficient: Building density → LST",
       fill = "Coefficient")


ggplot(st_as_sf(grid_clean)) +
  geom_sf(aes(fill = coef_r_dens)) +
  scale_fill_gradient2(
    low = "#125CA6",mid = "#FFFFCC", high = "#BF4600"
  ) +
  labs(title = "Local coefficient: Road density → LST",
       fill = "Coefficient")


ggplot(st_as_sf(grid_clean)) +
  geom_sf(aes(fill = coef_b_dist)) +
  scale_fill_gradient2(
    low = "#125CA6",mid = "#FFFFCC", high = "#BF4600"
  ) +
  labs(title = "Local coefficient: Mean distance to buildings → LST",
       fill = "Coefficient")

ggplot(st_as_sf(grid_clean)) +
  geom_sf(aes(fill = coef_canopy_perc)) +
  scale_fill_gradient2(
    low = "#125CA6",mid = "#FFFFCC", high = "#BF4600"
  ) +
  labs(title = "Local coefficient: Percentage of canopy cover  → LST",
       fill = "Coefficient")



###############################################################################

install.packages("SpatialML")
library(SpatialML)

grid_clean_noNA <- grid[complete.cases(as.data.frame(grid)[, vars]), ]
grid_clean_noNA <- grid_clean_noNA <- grid_clean_noNA[ , c("mean_LST", "SVF", "t_ratio", "t_height_mean",
                                                 "cd_mean", "per_acer", "per_tilia", "per_other", 
                                                 "green_perc", "canopy_perc",
                                                 "b_ratio",  "mean_b_dist1", "r_dens")]
grid_df<-as.data.frame(grid_clean)
# Compute centroids of only the remaining cells
grid_centroids <- centroids(grid_clean)

# Extract coords
coords <- crds(grid_centroids, df = TRUE)

bwrf<-grf.bw(formula = mean_LST ~ mean_LST ~ green_perc +  b_ratio + mean_b_dist1 +
               t_height_mean + per_tilia + dist_water + canopy_perc +
               r_dens,
             dataset = grid_df,
             coords = coords)
  
  
gwrf_model <- grf(
  formula = mean_LST ~ green_perc +  b_ratio + mean_b_dist1 +
    t_height_mean + per_tilia + dist_water + canopy_perc +
    r_dens,
  dframe = grid_df,
  coords= coords,
  bw = bw.AICcA, 
  kernel = "adaptive"
)


# Predicted LST at each observation (grid cell)
pred_LST <- gwrf_model$LGofFit$LM_yfitPred

# Add to your grid dataframe for mapping
df$pred_LST <- pred_LST

grid_clean$pred_LST <- gwrf_model$LGofFit$LM_yfitPred

# Plot with terra
plot(grid_clean["pred_LST"], main="Predicted LST from GWRF")


grid_sf <- st_as_sf(grid_clean)

ggplot(grid_sf) +
  geom_sf(aes(fill = pred_LST)) +
  scale_fill_viridis_c() +
  labs(title="Predicted LST from GWRF") +
  theme_minimal()


var_imp_local <- gwrf_model$Local.Variable.Importance

# For example, map importance of "green_area"
df$green_imp <- var_imp_local[, "green_perc"]

# Map it
grid_sf$green_imp <- df$green_imp

plot(grid_sf["green_imp"], main="Local importance of green_area")

ggplot(grid_sf) +
  geom_sf(aes(fill = green_imp)) +
  scale_fill_viridis_c() +
  labs(title="Importance of vegetation area") +
  theme_minimal()


df$brat_imp <- var_imp_local[, "b_ratio"]

# Map it
grid_sf$brat_imp<- df$brat_imp

ggplot(grid_sf) +
  geom_sf(aes(fill = brat_imp)) +
  scale_fill_viridis_c() +
  labs(title="Importance of building density") +
  theme_minimal()



df$bdist_imp <- var_imp_local[, "mean_b_dist1"]

grid_sf$bdist_imp<- df$bdist_imp

ggplot(grid_sf) +
  geom_sf(aes(fill = bdist_imp)) +
  scale_fill_viridis_c() +
  labs(title="Importance of mean distance to buildings") +
  theme_minimal()




df$th_imp <- var_imp_local[, "t_height_mean"]

grid_sf$th_imp<- df$th_imp

ggplot(grid_sf) +
  geom_sf(aes(fill = th_imp)) +
  scale_fill_viridis_c() +
  labs(title="Importance of mean tree height") +
  theme_minimal()


###############################################################################
#                            LST sensitivity
###############################################################################

#sensitivity of each year

sd_summer   <- app(LST_stack, fun = sd, na.rm = TRUE)

anomalies <- LST_stack-LST_mean
percent_change <- (LST_stack - LST_mean) / LST_mean * 100
z_scores <- (LST_stack - LST_mean) / sd_summer

cv_summer <- sd_summer / LST_mean 

names(anomalies) <- paste0("anom_", 2020:2025)
names(percent_change) <- paste0("pchg_", 2020:2025)
names(z_scores) <- paste0("z_", 2020:2025)

###############################################################################
#                          Comparison of LST 
###############################################################################


df$LST_pred <- predict(rf_fit, newdata = df)

ggplot(df, aes(x = mean_LST, y = LST_pred)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dashed") +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dotted") +
  labs(x = "Observed mean LST (°C)",
       y = "Modelled LST (°C)",
       title = "Observed vs. Modelled Land Surface Temperature (Random Forest)") +
  theme_minimal(base_size = 14)

rmse_val <- rmse(df$mean_LST, df$LST_pred)
r2_val <- cor(df$mean_LST, df$LST_pred)^2
