# Load Libraries
pacman::p_load(vip, dplyr,sf,ggplot2, st, terra, exactextractr, ranger,
               landscapemetrics, spdep, FNN, car, MASS, glmnet, spdep, 
               caret, GWmodel, iml, osmdata, randomForest, rsample, purrr, pdp)

#load grids 

grid_10 <- st_read("D:/3-Paper/grid_10m.gpkg")
grid_10 <- na.omit(grid_10)

grid_100 <- vect("D:/3-Paper/grid_100m.gpkg")
grid_100 <- na.omit(grid_100)


# Random sample n rows
set.seed(42)

###############################################################################
#                         Tree variables regression
###############################################################################
## 10 meters
#select only the tree variables
tree_10 <- grid_10[,c("mean_LST", "t_ratio", "CPA_mean", "t_height_mean",
                       "cd_mean", "per_acer", "per_tilia", "per_other", "per_robin", "per_aesculus",
                      "canopy_perc", "dist_tr") ]

tree_10 <- st_drop_geometry(tree_10)


K <- 5
folds <- sample(rep(1:K, length.out = nrow(tree_10)))

cv_rmse_t10 <- numeric(K)
imp_tree10 <- vector("list", K)

for (k in 1:K) {
  
  train <- tree_10[folds != k, ] 
  valid <- tree_10[folds == k, ] 
  
  rf_tree10 <- ranger(
    mean_LST ~ .,
    data = train,
    num.trees = 300,
    min.node.size = 5,
    importance = "permutation",   
    num.threads = 1               
  )
  
  pred_t10 <- predict(rf_tree10, valid)$predictions
  cv_rmse_t10[k] <- sqrt(mean((valid$mean_LST - pred_t10)^2))
  
  imp_tree10[[k]] <- rf_tree10$variable.importance
}

imp_mat <- do.call(cbind, imp_tree10)

imp_mean <- rowMeans(imp_mat)
imp_sd   <- apply(imp_mat, 1, sd)

imp_cv_t10 <- data.frame(
  variable = rownames(imp_mat),
  mean_imp = imp_mean,
  sd_imp   = imp_sd
)

imp_cv_t10 <- imp_cv_t10[order(-imp_cv_t10$mean_imp), ]
head(imp_cv_t10)

#Variable importance plot

imp_top10 <- imp_cv_t10 %>%
  slice_max(mean_imp, n = 10)

ggplot(imp_top10,
       aes(x = reorder(variable, mean_imp),
           y = mean_imp)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Variable Importance",
    x = "Variable",
    y = "Mean Permutation Importance"
  ) +
  theme_minimal(base_size = 18)



#-------------------------------------------------------------------------------
## 100 meters

#select only the tree variables
#tree_100 <- grid_100[,c("mean_LST", "t_ratio", "CPA_mean", "t_height_mean",
#                      "per_acer", "per_tilia", "per_other", "per_robin", "per_aesculus",
#                      "canopy_perc", "dist_tr") ]


tree_100 <- grid_100[,c("mean_LST", "num_trees", "t_height_mean", "agg_LAI", "ETA_tot",
"dist_tr", "canopy_perc")]

#tree_100 <- st_drop_geometry(tree_100)
tree_100 <- as.data.frame(tree_100)
tree_100 <-na.omit(tree_100)

K <- 5
folds <- sample(rep(1:K, length.out = nrow(tree_100)))

cv_rmse_t100 <- numeric(K)
imp_tree100 <- vector("list", K)

for (k in 1:K) {
  
  train <- tree_100[folds != k, ] 
  valid <- tree_100[folds == k, ] 
  
  rf_tree100 <- ranger(
    mean_LST ~ .,
    data = train,
    num.trees = 300,
    min.node.size = 5,
    importance = "permutation",   
    num.threads = 1               
  )
  
  pred_t100 <- predict(rf_tree100, valid)$predictions
  cv_rmse_t100[k] <- sqrt(mean((valid$mean_LST - pred_t100)^2))
  
  imp_tree100[[k]] <- rf_tree100$variable.importance
}

imp_mat <- do.call(cbind, imp_tree100)

imp_mean <- rowMeans(imp_mat)
imp_sd   <- apply(imp_mat, 1, sd)

imp_cv_t100 <- data.frame(
  variable = rownames(imp_mat),
  mean_imp = imp_mean,
  sd_imp   = imp_sd
)

imp_cv_t100 <- imp_cv_t100[order(-imp_cv_t100$mean_imp), ]
head(imp_cv_t100)

#Variable importance plot
imp_top100 <- imp_cv_t100 %>%
  slice_max(mean_imp, n = 10)

ggplot(imp_top100,
       aes(x = reorder(variable, mean_imp),
           y = mean_imp)) +
  geom_col(fill = "darkolivegreen") +
  coord_flip() +
  labs(
    title = "LST Variable Importance only trees",
    x = "Variable",
    y = "Mean Permutation Importance"
  ) +
  theme_minimal(base_size = 18)



###############################################################################
#                         All variables regression
###############################################################################

## 10 meters 
#select only the tree variables
all_10 <- grid_10[,c("mean_LST", "SVF", "t_ratio", "CPA_mean", "t_height_mean",
                       "cd_mean", "per_acer", "per_tilia", "per_other", "per_robin", "per_aesculus",
                       "canopy_perc", "veg_perc", "green_perc", "b_height_mean",
                       "b_ratio", "mean_b_dist2",  "mean_b_dist1", "r_dens", "dist_water", "dist_tr") ]

all_10 <- st_drop_geometry(all_10)

K <- 5
folds <- sample(rep(1:K, length.out = nrow(all_10)))

cv_rmse_a10 <- numeric(K)
imp_all10 <- vector("list", K)

for (k in 1:K) {
  
  train <- all_10[folds != k, ] 
  valid <- all_10[folds == k, ] 
  
  rf_all10 <- ranger(
    mean_LST ~ .,
    data = train,
    num.trees = 300,
    min.node.size = 5,
    importance = "permutation",   
    num.threads = 1               
  )
  
  pred_a10 <- predict(rf_all10, valid)$predictions
  cv_rmse_a10[k] <- sqrt(mean((valid$mean_LST - pred_a10)^2))
  
  imp_all10[[k]] <- rf_all10$variable.importance
}

imp_mat <- do.call(cbind, imp_all10)

imp_mean <- rowMeans(imp_mat)
imp_sd   <- apply(imp_mat, 1, sd)

imp_cv_a10 <- data.frame(
  variable = rownames(imp_mat),
  mean_imp = imp_mean,
  sd_imp   = imp_sd
)

imp_cv_a10 <- imp_cv_a10[order(-imp_cv_a10$mean_imp), ]
head(imp_cv_a10)

#Variable importance plot

imp_top10 <- imp_cv_a10 %>%
  slice_max(mean_imp, n = 10)

ggplot(imp_top10,
       aes(x = reorder(variable, mean_imp),
           y = mean_imp)) +
  geom_col(fill = "cadetblue3") +
  coord_flip() +
  labs(
    title = "Variable Importance",
    x = "Variable",
    y = "Mean Permutation Importance"
  ) +
  theme_minimal(base_size = 18)

#-------------------------------------------------------------------------------

#select only the tree variables
#all_100 <- grid_100[,c("mean_LST", "SVF", "t_ratio", "CPA_mean", "t_height_mean",
#                     "cd_mean", "per_acer", "per_tilia", "per_other", "per_robin", "per_aesculus",
#                     "canopy_perc", "green_perc", "b_height_mean",
#                     "b_ratio", "mean_b_dist2",  "mean_b_dist1", "r_dens", "dist_water", "dist_tr") ]

all_100 <- grid_100[,c("mean_LST", "num_trees", "t_height_mean", "SVF", "agg_LAI", "ETA_tot",
                        "dist_tr", "dist_water", "canopy_perc", "green_perc", "b_height_mean",
                       "b_area_sum", "r_dens" ) ]


#all_100 <- st_drop_geometry(all_100)
all_100 <- as.data.frame(all_100)
all_100 <- na.omit(all_100)


K <- 5
folds <- sample(rep(1:K, length.out = nrow(all_100)))

cv_rmse_a100 <- numeric(K)
imp_all100 <- vector("list", K)

for (k in 1:K) {
  
  train <- all_100[folds != k, ] 
  valid <- all_100[folds == k, ] 
  
  rf_all100 <- ranger(
    mean_LST ~ .,
    data = train,
    num.trees = 300,
    min.node.size = 5,
    importance = "permutation",   
    num.threads = 1               
  )
  
  pred_a100 <- predict(rf_all100, valid)$predictions
  cv_rmse_a100[k] <- sqrt(mean((valid$mean_LST - pred_a100)^2))
  
  imp_all100[[k]] <- rf_all100$variable.importance
}

imp_mat <- do.call(cbind, imp_all100)

imp_mean <- rowMeans(imp_mat)
imp_sd   <- apply(imp_mat, 1, sd)

imp_cv_a100 <- data.frame(
  variable = rownames(imp_mat),
  mean_imp = imp_mean,
  sd_imp   = imp_sd
)

imp_cv_a100 <- imp_cv_a100[order(-imp_cv_a100$mean_imp), ]
head(imp_cv_a100)

#Variable importance plot

imp_top10 <- imp_cv_a100 %>%
  slice_max(mean_imp, n = 10)

ggplot(imp_top10,
       aes(x = reorder(variable, mean_imp),
           y = mean_imp)) +
  geom_col(fill = "coral") +
  coord_flip() +
  labs(
    title = "LST Variable Importance for grid",
    x = "Variable",
    y = "Mean Permutation Importance"
  ) +
  theme_minimal(base_size = 18)


###############################################################################
#                       Partial Dependence Plots
###############################################################################

## Partial dependence plots

# Wrap the model
predictort10 <- Predictor$new(
  model = rf_tree10, 
  data = tree_10[, -which(names(tree_10) == "mean_LST")], 
  y = tree_10$mean_LST
)


# Create a Partial Dependence Plot for 'lstat' (percent lower status population)
pdp_tree100 <- partial(
  object = rf_tree100, 
  pred.var = "canopy_perc", 
  grid.resolution = 20,  # Number of points in the grid
  train = tree_100
)
plot(pdp_lstat, main = "Partial Dependence of medv on lstat")

# Partial dependence for "num_trees"

pdp_can <- FeatureEffect$new(predictort10, feature = "canopy_perc", method = "pdp")
plot(pdp_can)

pdp_distr <- FeatureEffect$new(predictort10, feature = "dist_tr", method = "pdp")
plot(pdp_distr)

pdp_CPA <- FeatureEffect$new(predictort10, feature = "CPA_mean", method = "pdp")
plot(pdp_CPA)

pdp_cd <- FeatureEffect$new(predictort10, feature = "cd_mean", method = "pdp")
plot(pdp_cd)

#-------------------------------------------------------------------------------


## Partial dependence plots

# Wrap the model
predictort100 <- Predictor$new(
  model = rf_tree100, 
  data = tree_100[, -which(names(tree_100) == "mean_LST")], 
  y = tree_100$mean_LST
)

# Partial dependence for "num_trees"

pdp_can_100 <- FeatureEffect$new(predictort100, feature = "canopy_perc", method = "pdp")
plot(pdp_can_100)

pdp_CPA_100 <- FeatureEffect$new(predictort100, feature = "CPA_mean", method = "pdp")
plot(pdp_CPA_100)

pdp_cd_100 <- FeatureEffect$new(predictort100, feature = "cd_mean", method = "pdp")
plot(pdp_cd_100)


pdp_trat_100 <- FeatureEffect$new(predictort10, feature = "t_ratio", method = "pdp")
plot(pdp_trat_100)

#-------------------------------------------------------------------------------


## Partial dependence plots

# Wrap the model
predictora100 <- Predictor$new(
  model = rf_all100, 
  data = all_100[, -which(names(all_100) == "mean_LST")], 
  y = all_100$mean_LST
)

# Partial dependence for "num_trees"


pdp_green_a100 <- FeatureEffect$new(predictora100, feature = "green_perc", method = "pdp")
plot(pdp_green_a100)

pdp_can_a100 <- FeatureEffect$new(predictora100, feature = "canopy_perc", method = "pdp")
plot(pdp_can_a100)

pdp_build_100 <- FeatureEffect$new(predictora100, feature = "b_ratio", method = "pdp")
plot(pdp_build_100)
 
pdp_veg_100 <- FeatureEffect$new(predictora100, feature = "veg_perc", method = "pdp")
plot(pdp_veg_100)


#-------------------------------------------------------------------------------

# Wrap the model
predictora10 <- Predictor$new(
  model = rf_all10, 
  data = all_10[, -which(names(all_10) == "mean_LST")], 
  y = all_10$mean_LST
)


# Partial dependence for "num_trees"

pdp_green_a10 <- FeatureEffect$new(predictora10, feature = "green_perc", method = "pdp")
plot(pdp_green_a10)

pdp_can_a10 <- FeatureEffect$new(predictora10, feature = "canopy_perc", method = "pdp")
plot(pdp_can_a10)

pdp_distb_10 <- FeatureEffect$new(predictora10, feature = "mean_b_dist1", method = "pdp")
plot(pdp_distb_10)

pdp_veg_10 <- FeatureEffect$new(predictora10, feature = "veg_perc", method = "pdp")
plot(pdp_veg_10)


###############################################################################
#                       Species consideration
###############################################################################

# species proportion column names
species_cols <- c("per_tilia", "per_acer", "per_aesculus",
                  "per_robin", "per_popul", "per_other")

df <- st_drop_geometry(all_10)  # or your cleaned data frame

# assign dominant species (string)
df$dominant_species <- species_cols[apply(df[, species_cols], 1, which.max)]

n_per_species <- 4000   # choose depending on computing power

grid_sub_species <- df %>%
  group_by(dominant_species) %>%
  slice_sample(prop=0.25) %>%
  ungroup()

grid_sub_species$dominant_species <-
  as.factor(grid_sub_species$dominant_species)

grid_sub_species<-as.data.frame(grid_sub_species)

df_rep <- grid_sub_species[,c("mean_LST", "SVF", "t_ratio", "CPA_mean", "t_height_mean",
                              "cd_mean", "dominant_species", "canopy_perc", "veg_perc", "green_perc", "b_height_mean",
                              "b_ratio", "mean_b_dist2",  "mean_b_dist1", "r_dens", "dist_water", "dist_tr") ]

K <- 5
folds <- sample(rep(1:K, length.out = nrow(df_rep)))

cv_rmse <- numeric(K)
imp_list <- vector("list", K)

for (k in 1:K) {
  
  train <- df_rep[folds != k, ] 
  valid <- df_rep[folds == k, ] 
  
  rf <- ranger(
    mean_LST ~ .,
    data = train,
    num.trees = 300,
    min.node.size = 5,
    importance = "permutation",   
    num.threads = 1               
  )
  
  pred <- predict(rf, valid)$predictions
  cv_rmse[k] <- sqrt(mean((valid$mean_LST - pred)^2))
  
  imp_list[[k]] <- rf$variable.importance
}

imp_mat <- do.call(cbind, imp_list)

imp_mean <- rowMeans(imp_mat)
imp_sd   <- apply(imp_mat, 1, sd)

imp_cv <- data.frame(
  variable = rownames(imp_mat),
  mean_imp = imp_mean,
  sd_imp   = imp_sd
)

imp_cv_sp <- imp_cv[order(-imp_cv$mean_imp), ]
head(imp_cv_sp)

#Variable importance plot

imp_10sp<- imp_cv_sp %>%
  slice_max(mean_imp, n = 15)

ggplot(imp_10sp,
       aes(x = reorder(variable, mean_imp),
           y = mean_imp)) +
  geom_col(fill = "chocolate") +
  coord_flip() +
  labs(
    title = "Variable Importance",
    x = "Variable",
    y = "Mean Permutation Importance"
  ) +
  theme_minimal(base_size = 18)    

#-----------------------------------------------------------------------------


df_tilia <- df %>%
  filter(dominant_species == "per_tilia")

folds <- sample(rep(1:K, length.out = nrow(df_tilia)))

cv_rmse <- numeric(K)
imp_list <- vector("list", K)

for (k in 1:K) {
  
  train <- df_tilia[folds != k, ] 
  valid <- df_tilia[folds == k, ] 
  
  rf <- ranger(
    mean_LST ~ .,
    data = train,
    num.trees = 300,
    min.node.size = 5,
    importance = "permutation",   
    num.threads = 1               
  )
  
  pred <- predict(rf, valid)$predictions
  cv_rmse[k] <- sqrt(mean((valid$mean_LST - pred)^2))
  
  imp_list[[k]] <- rf$variable.importance
}

imp_mat <- do.call(cbind, imp_list)

imp_mean <- rowMeans(imp_mat)
imp_sd   <- apply(imp_mat, 1, sd)

imp_cv <- data.frame(
  variable = rownames(imp_mat),
  mean_imp = imp_mean,
  sd_imp   = imp_sd
)

imp_cv_til <- imp_cv[order(-imp_cv$mean_imp), ]
head(imp_cv_til)

#Variable importance plot

imp_10til<- imp_cv_til %>%
  slice_max(mean_imp, n = 15)

ggplot(imp_10til,
       aes(x = reorder(variable, mean_imp),
           y = mean_imp)) +
  geom_col(fill = "orchid") +
  coord_flip() +
  labs(
    title = "Variable Importance",
    x = "Variable",
    y = "Mean Permutation Importance"
  ) +
  theme_minimal(base_size = 18)   

#---------------------------------------------------------------------------

df_acer <- df %>%
  filter(dominant_species == "per_acer")

folds <- sample(rep(1:K, length.out = nrow(df_acer)))

cv_rmse <- numeric(K)
imp_list <- vector("list", K)

for (k in 1:K) {
  
  train <- df_acer[folds != k, ] 
  valid <- df_acer[folds == k, ] 
  
  rf <- ranger(
    mean_LST ~ .,
    data = train,
    num.trees = 300,
    min.node.size = 5,
    importance = "permutation",   
    num.threads = 1               
  )
  
  pred <- predict(rf, valid)$predictions
  cv_rmse[k] <- sqrt(mean((valid$mean_LST - pred)^2))
  
  imp_list[[k]] <- rf$variable.importance
}

imp_mat <- do.call(cbind, imp_list)

imp_mean <- rowMeans(imp_mat)
imp_sd   <- apply(imp_mat, 1, sd)

imp_cv <- data.frame(
  variable = rownames(imp_mat),
  mean_imp = imp_mean,
  sd_imp   = imp_sd
)

imp_cv_acer <- imp_cv[order(-imp_cv$mean_imp), ]
head(imp_cv_acer)

#Variable importance plot

imp_10acer<- imp_cv_acer %>%
  slice_max(mean_imp, n = 15)

ggplot(imp_10acer,
       aes(x = reorder(variable, mean_imp),
           y = mean_imp)) +
  geom_col(fill = "cornflowerblue") +
  coord_flip() +
  labs(
    title = "Variable Importance",
    x = "Variable",
    y = "Mean Permutation Importance"
  ) +
  theme_minimal(base_size = 18) 
         