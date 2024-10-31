pacman::p_load(sf, terra, dplyr, ggplot2, viridis, ggspatial, prettymapr)


######################### Diversity for trees in public areas total #####################

rs_trees_all<-st_read("D:/Paper_1/Diversity/RS_trees.shp")
cadastre_all<-st_read("D:/Paper_1/Diversity/Cadastre_trees.shp")


# Filter the data frames to include only rows where 'public' is 1
rs_trees <- rs_trees_all[rs_trees_all$Public == 0, ]
cadastre <- cadastre_all[cadastre_all$Public == 1, ]

# Reclassify the 'GATTUNG' column in the cadastre dataset
cadastre_aggregated <- cadastre %>%
  mutate(GATTUNG = ifelse(GATTUNG %in% c("52", "2", "46", "3", "41"), GATTUNG, "80"))

#  Prepare Frequency and proportions
species_counts_cd<-as.data.frame(table(cadastre$GATTUNG))
species_counts_cd$Prop <- species_counts_cd$Freq / sum(species_counts_cd$Freq)

#  Prepare Frequency and proportions
species_counts_cag<-as.data.frame(table(cadastre_aggregated$GATTUNG))
species_counts_cag$Prop <- species_counts_cag$Freq / sum(species_counts_cag$Freq)

species_counts_rs<-as.data.frame(table(rs_trees$t1_majorit))
species_counts_rs$Prop <- species_counts_rs$Freq / sum(species_counts_rs$Freq)



# Function to calculate various diversity indices
calculate_diversity_indices <- function(data) {
  # Check if the proportions sum up to 1
  if (sum(data$Prop) != 1) {
    stop("Proportions do not sum up to 1. Please check the data.")
  }
  
  # Shannon Index
  shannon_index <- -sum(data$Prop * log(data$Prop))
  
  # Simpson Index
  simpson_index <- 1-sum(data$Prop^2)
  
  # Inverse Simpson Index
  inverse_simpson_index <- 1 / simpson_index
  
  # Pielou's Evenness Index
  pielou_evenness <- shannon_index / log(length(data$Prop))
  
  # Margalef's Richness Index
  margalef_richness <- (length(data$Prop) - 1) / log(sum(data$Freq))
  
  # Menhinick's Index
  menhinick_index <- length(data$Prop) / sqrt(sum(data$Freq))
  
  # Combine the indices into a list
  diversity_indices <- data.frame(
    Shannon = shannon_index,
    Simpson = simpson_index,
    Inverse_Simpson = inverse_simpson_index,
    Pielou_Evenness = pielou_evenness,
    Margalef_Richness = margalef_richness,
    Menhinick = menhinick_index
  )
  
  return(diversity_indices)
}



# Calculate the diversity indices for the cadastre
diversity_indices_cd <- calculate_diversity_indices(species_counts_cd)
print(diversity_indices_cd)

# Calculate the diversity indices for the aggregated cadastre
diversity_indices_cag <- calculate_diversity_indices(species_counts_cag)
print(diversity_indices_cag)


# Calculate the diversity indices for remote sensing
diversity_indices_rs <- calculate_diversity_indices(species_counts_rs)
print(diversity_indices_rs)



######################### Diversity for trees in private areas total #####################

rs_trees_all<-st_read("D:/Paper_1/Diversity/RS_trees.shp")


# Filter the data frames to include only rows where 'public' is 1
rs_trees_pr <- rs_trees_all[rs_trees_all$Public == 0, ]


species_counts_rs_pr<-as.data.frame(table(rs_trees_pr$t1_majorit))
species_counts_rs_pr$Prop <- species_counts_rs_pr$Freq / sum(species_counts_rs_pr$Freq)


# Calculate the diversity indices for the given data frame
diversity_indices_rs_pr <- calculate_diversity_indices(species_counts_rs_pr)
print(diversity_indices_rs_pr)



######################### Diversity for trees in public areas grid (REMOTE) #####################


# Load your MR layer
MR <- st_read("C:/Users/ang58gl/Documents/Data/MittlererRing_new.shp")

# Define the grid size
grid_size <- 250  # 100 meters

# Create a grid over the extent of the MR layer
grid <- st_make_grid(MR, cellsize = grid_size, square = TRUE)

# Convert grid to an sf object
grid_rs<- st_sf(geometry = grid, crs = st_crs(rs_trees_all))

######
grid_rs$Richness <- NA
grid_rs$Shannon <- NA
grid_rs$Simpson <- NA
grid_rs$InvSimp <- NA
grid_rs$Pielou <- NA
#grid_rs$Margalef <- NA
#grid_rs$Menhi <- NA


for(i in 1: nrow(grid_rs)){
  
  Subgrid <- st_intersection(grid_rs[i,],rs_trees) %>% as.data.frame() %>% select("t1_majorit") %>% na.omit() %>% table() %>% as.data.frame()
  
  # Apply threshold: only proceed if there are 3 or more trees
  if (nrow(Subgrid) >= 3) {
    
  grid_rs$Richness[i] <- nrow(Subgrid)
  
  Subgrid$prop <- Subgrid$Freq / sum(Subgrid$Freq)
  
  grid_rs$Shannon[i] <- -sum(Subgrid$prop * log(Subgrid$prop))
  
  # Simpson Index
  grid_rs$Simpson[i]<- 1-sum(Subgrid$prop^2)
  
  # Inverse Simpson Index
  grid_rs$InvSimp[i] <- 1 / sum(Subgrid$prop^2)
  
  # Pielou's Evenness Index
  grid_rs$Pielou[i] <- -sum(Subgrid$prop * log(Subgrid$prop)) / log(length(Subgrid$prop))
  
  # Margalef's Richness Index
  #grid_rs$Margalef[i] <- (length(Subgrid$prop) - 1) / log(sum(Subgrid$Freq))
  
  # Menhinick's Index
 # grid_rs$Menhi[i] <- length(Subgrid$prop) / sqrt(sum(Subgrid$Freq))
  } else {
    # If less than 3 trees, set the indices to NA for the cell
    grid_rs$Richness[i] <- NA
    grid_rs$Shannon[i] <- NA
    grid_rs$Simpson[i] <- NA
    grid_rs$InvSimp[i] <- NA
    grid_rs$Pielou[i] <- NA
  }
}

plot(grid_rs)


# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_rs, aes(fill = Richness), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Richness") +
  # Additional map elements
  labs(title = "Genera Richness",
       subtitle = "Base Map: OpenStreetMap"
       ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_rs))


# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_rs, aes(fill = Shannon), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Shannon Index") +
  # Additional map elements
  labs(title = "Shannon Index in Public Areas (Remote Sensing)",
       subtitle = "Base Map: OpenStreetMap"
  ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_rs))

# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_rs, aes(fill = Simpson), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Simpson Index") +
  # Additional map elements
  labs(title = "Simpson's Diversity Index in Public Areas (Remote Sensing)",
       subtitle = "Base Map: OpenStreetMap"
  ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_rs))


# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_rs, aes(fill = InvSimp), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Inverse Simpson Index") +
  # Additional map elements
  labs(title = "Inverse Simpson's Diversity Index in Public Areas (Remote Sensing)",
       subtitle = "Base Map: OpenStreetMap"
  ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_rs))


# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_rs, aes(fill = Pielou), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Pielou Index") +
  # Additional map elements
  labs(title = "Pielou's Evenness Index in Public Areas (Remote Sensing)",
       subtitle = "Base Map: OpenStreetMap"
  ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_rs))

# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_rs, aes(fill = Margalef), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Margalef Index") +
  # Additional map elements
  labs(title = "Margalef's Richness Index in Public Areas (Remote Sensing)",
       subtitle = "Base Map: OpenStreetMap"
  ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_rs))


# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_rs, aes(fill = Menhi), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Menhinick's Index") +
  # Additional map elements
  labs(title = "Menhinick's Index in Public Areas (Remote Sensing)",
       subtitle = "Base Map: OpenStreetMap"
  ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_rs))

######################### Diversity for trees in public areas grid  (CADASTRE) #####################


# Load your MR layer
MR <- st_read("C:/Users/ang58gl/Documents/Data/MittlererRing_new.shp")

# Define the grid size
grid_size <- 250  # 100 meters

# Create a grid over the extent of the MR layer
grid <- st_make_grid(MR, cellsize = grid_size, square = TRUE)

# Convert grid to an sf object
grid_cd <- st_sf(geometry = grid, crs = st_crs(rs_trees_all))

######
grid_cd$Richness <- NA
grid_cd$Shannon <- NA
grid_cd$Simpson <- NA
grid_cd$InvSimp <- NA
grid_cd$Pielou <- NA
#grid_cd$Margalef <- NA
#grid_cd$Menhi <- NA


for(i in 1: nrow(grid_cd)){
  
  Subgrid <- st_intersection(grid_cd[i,],cadastre_aggregated) %>% as.data.frame() %>% select("GATTUNG") %>% na.omit() %>% table() %>% as.data.frame()
  
  # Apply threshold: only proceed if there are 3 or more trees
  if (nrow(Subgrid) >= 3) {
    
  grid_cd$Richness[i] <- nrow(Subgrid)
  
  Subgrid$prop <- Subgrid$Freq / sum(Subgrid$Freq)
  
  grid_cd$Shannon[i] <- -sum(Subgrid$prop * log(Subgrid$prop))
  
  # Simpson Index
  grid_cd$Simpson[i]<- 1-sum(Subgrid$prop^2)
  
  # Inverse Simpson Index
  grid_cd$InvSimp[i] <- 1 / sum(Subgrid$prop^2)
  
  # Pielou's Evenness Index
  grid_cd$Pielou[i] <- -sum(Subgrid$prop * log(Subgrid$prop)) / log(length(Subgrid$prop))
  
  # Margalef's Richness Index
  #grid_cd$Margalef[i] <- (length(Subgrid$prop) - 1) / log(sum(Subgrid$Freq))
  
  # Menhinick's Index
 # grid_cd$Menhi[i] <- length(Subgrid$prop) / sqrt(sum(Subgrid$Freq))
} else {
  # If less than 3 trees, set the indices to NA for the cell
  grid_cd$Richness[i] <- NA
  grid_cd$Shannon[i] <- NA
  grid_cd$Simpson[i] <- NA
  grid_cd$InvSimp[i] <- NA
  grid_cd$Pielou[i] <- NA
}
  }

plot(grid_cd)


# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_cd, aes(fill = Richness), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Richness") +
  # Additional map elements
  labs(title = "Genera Richness",
       subtitle = "Base Map: OpenStreetMap"
  ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_cd))


# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_cd, aes(fill = Shannon), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Shannon Index") +
  # Additional map elements
  labs(title = "Shannon Index in Public Areas (Cadastre)",
       subtitle = "Base Map: OpenStreetMap"
  ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_cd))

# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_cd, aes(fill = Simpson), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Simpson Index") +
  # Additional map elements
  labs(title = "Simpson's Diversity Index in Public Areas (Cadastre)",
       subtitle = "Base Map: OpenStreetMap"
  ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_cd))


# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_cd, aes(fill = InvSimp), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Inverse Simpson Index") +
  # Additional map elements
  labs(title = "Inverse Simpson's Diversity Index in Public Areas (Cadastre)",
       subtitle = "Base Map: OpenStreetMap"
  ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_cd))


# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_cd, aes(fill = Pielou), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Pielou Index") +
  # Additional map elements
  labs(title = "Pielou's Evenness Index in Public Areas (Cadastre)",
       subtitle = "Base Map: OpenStreetMap"
  ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_cd))

# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_cd, aes(fill = Margalef), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Margalef Index") +
  # Additional map elements
  labs(title = "Margalef's Richness Index in Public Areas (Cadastre)",
       subtitle = "Base Map: OpenStreetMap"
  ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_cd))


# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_cd, aes(fill = Menhi), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Menhinick's Index") +
  # Additional map elements
  labs(title = "Menhinick's Index in Public Areas (Cadastre)",
       subtitle = "Base Map: OpenStreetMap"
  ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_cd))


######################### Diversity for trees in private areas grid  (Remote Sensing) #####################

# Load your MR layer
MR <- st_read("C:/Users/ang58gl/Documents/Data/MittlererRing_new.shp")

# Define the grid size
grid_size <- 250  # 100 meters

# Create a grid over the extent of the MR layer
grid <- st_make_grid(MR, cellsize = grid_size, square = TRUE)

# Convert grid to an sf object
grid_pri <- st_sf(geometry = grid, crs = st_crs(rs_trees_all))

######
grid_pri$Richness <- NA
grid_pri$Shannon <- NA
grid_pri$Simpson <- NA
grid_pri$InvSimp <- NA
grid_pri$Pielou <- NA
#grid_pri$Margalef <- NA
#grid_pri$Menhi <- NA


for(i in 1: nrow(grid_pri)){
  
  Subgrid <- st_intersection(grid_pri[i,],rs_trees_pr) %>% as.data.frame() %>% select("t1_majorit") %>% na.omit() %>% table() %>% as.data.frame()
  
  # Apply threshold: only proceed if there are 3 or more trees
  if (nrow(Subgrid) >= 3) {
    
  grid_pri$Richness[i] <- nrow(Subgrid)
  
  Subgrid$prop <- Subgrid$Freq / sum(Subgrid$Freq)
  
  grid_pri$Shannon[i] <- -sum(Subgrid$prop * log(Subgrid$prop))
  
  # Simpson Index
  grid_pri$Simpson[i]<- 1-sum(Subgrid$prop^2)
  
  # Inverse Simpson Index
  grid_pri$InvSimp[i] <- 1 / sum(Subgrid$prop^2)
  
  # Pielou's Evenness Index
  grid_pri$Pielou[i] <- -sum(Subgrid$prop * log(Subgrid$prop)) / log(length(Subgrid$prop))
  
  # Margalef's Richness Index
  #grid_pri$Margalef[i] <- (length(Subgrid$prop) - 1) / log(sum(Subgrid$Freq))
  
  # Menhinick's Index
  #grid_pri$Menhi[i] <- length(Subgrid$prop) / sqrt(sum(Subgrid$Freq))
} else {
  # If less than 3 trees, set the indices to NA for the cell
  grid_pri$Richness[i] <- NA
  grid_pri$Shannon[i] <- NA
  grid_pri$Simpson[i] <- NA
  grid_pri$InvSimp[i] <- NA
  grid_pri$Pielou[i] <- NA
}
  }

plot(grid_pri)


# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_pri, aes(fill = Richness), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Richness") +
  # Additional map elements
  labs(title = "Genera Richness",
       subtitle = "Base Map: OpenStreetMap"
  ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_pri))


# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_pri, aes(fill = Shannon), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Shannon Index") +
  # Additional map elements
  labs(title = "Shannon Index in Private Areas (Remote Sensing)",
       subtitle = "Base Map: OpenStreetMap"
  ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_pri))

# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_pri, aes(fill = Simpson), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Simpson Index") +
  # Additional map elements
  labs(title = "Simpson's Diversity Index in Private Areas (Remote Sensing)",
       subtitle = "Base Map: OpenStreetMap"
  ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_pri))


# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_pri, aes(fill = InvSimp), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Inverse Simpson Index") +
  # Additional map elements
  labs(title = "Inverse Simpson's Diversity Index in Private Areas (Remote Sensing)",
       subtitle = "Base Map: OpenStreetMap"
  ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_pri))


# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_pri, aes(fill = Pielou), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Pielou Index") +
  # Additional map elements
  labs(title = "Pielou's Evenness Index in Private Areas (Remote Sensing)",
       subtitle = "Base Map: OpenStreetMap"
  ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_pri))

# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_pri, aes(fill = Margalef), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Margalef Index") +
  # Additional map elements
  labs(title = "Margalef's Richness Index in Private Areas (Remote Sensing)",
       subtitle = "Base Map: OpenStreetMap"
  ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_pri))


# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_pri, aes(fill = Menhi), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Menhinick's Index") +
  # Additional map elements
  labs(title = "Menhinick's Index in Private Areas (Remote Sensing)",
       subtitle = "Base Map: OpenStreetMap"
  ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_pri))



################################################################################
# Differences of Indices between different sources

grid_combined <- st_join(grid_rs, grid_cd, join = st_equals)


grid_combined$Simp_dif <- grid_combined$Simpson.x-grid_combined$Simpson.y
grid_combined$Shan_dif <- grid_combined$Shannon.x-grid_combined$Shannon.y
grid_combined$Piel_dif <- grid_combined$Pielou.x-grid_combined$Pielou.y


################################################################################
# Differences of Indices between different sources

grid_combined <- st_join(grid_combined, grid_pri, join = st_equals)

grid_combined$Simp_pupri <- grid_combined$Simpson.x-grid_combined$Simpson
grid_combined$Shan_pupri <- grid_combined$Shannon.x-grid_combined$Shannon
grid_combined$Piel_pupri <- grid_combined$Pielou.x-grid_combined$Pielou


################################################################################
#
# Omit NA values from the grid_combined dataset
grid_combined_clean <- grid_combined %>% 
  dplyr::filter(!is.na(Simp_dif))

# Create a vertical histogram of Simpson Index from the grid_combined dataset
ggplot(grid_combined_clean, aes(x = Simp_dif)) +
  geom_histogram(aes(fill = ..x..), binwidth = 0.05, color = "black", alpha = 0.7) +
  
  # Add custom color palette for the fill
  scale_fill_gradient2(low="#d01c8b", mid="#f7f7f7", high ="#4dac26", name="Difference in
Simpson's Index") +
  
  # Flip the axes to make the histogram vertical
  coord_flip() +
  
  # Labels and theme
  labs(title = " ",
       y = "Frequency",
       x = "Difference of Simpson Index") +
  theme_minimal()

# Create a vertical histogram of Simpson Index from the grid_combined dataset
ggplot(grid_combined, aes(x = Shan_dif)) +
  geom_histogram(aes(fill = ..x..), binwidth = 0.05, color = "black", alpha = 0.7) +
  
  # Add custom color palette for the fill
  scale_fill_gradient2(low="#d01c8b", mid="#f7f7f7", high ="#4dac26", name="Difference in
Shannon Index") +
  
  # Flip the axes to make the histogram vertical
  coord_flip() +
  
  # Labels and theme
  labs(title = " ",
       y = "Frequency",
       x = "Difference of Shannon Index") +
  theme_minimal()


# Create a vertical histogram of Simpson Index from the grid_combined dataset
ggplot(grid_combined, aes(x = Simp_pupri)) +
  geom_histogram(aes(fill = ..x..), binwidth = 0.05, color = "black", alpha = 0.7) +
  
  # Add custom color palette for the fill
  scale_fill_gradient2(low="#3790c7", mid="#f6eff7", high ="#016c59", name="Difference in
Simpson's Index") +
  
  # Flip the axes to make the histogram vertical
  coord_flip() +
  
  # Labels and theme
  labs(title = " ",
       y = "Frequency",
       x = "Difference of Simpson Index") +
  theme_minimal()





# Create a vertical histogram of Simpson Index from the grid_combined dataset
ggplot(grid_combined, aes(x = Shan_pupri)) +
  geom_histogram(aes(fill = ..x..), binwidth = 0.05, color = "black", alpha = 0.7) +
  
  # Add custom color palette for the fill
  scale_fill_gradient2(low="#3790c7", mid="#f6eff7", high ="#016c59", name="Difference in
Simpson's Index") +
  
  # Flip the axes to make the histogram vertical
  coord_flip() +
  
  # Labels and theme
  labs(title = " ",
       y = "Frequency",
       x = "Difference of Simpson Index") +
  theme_minimal()

st_write(grid_combined, "D:/Paper_1/Diversity/Diversity_grid_corrected.shp")


summary(grid_combined_clean)


dif<-data.frame(grid_combined_clean$Simp_dif)


dif_filtered <- dif %>%
  filter(grid_combined_clean.Simp_dif < 0) #1 & grid_combined_clean.Simp_dif > -0.1)


306/539*100

123/539*100

81/539*100

49/539*100



######################### Diversity for trees in public areas grid  (CADASTRE) #####################


# Load your MR layer
MR <- st_read("C:/Users/ang58gl/Documents/Data/MittlererRing_new.shp")

# Define the grid size
grid_size <- 250  # 100 meters

# Create a grid over the extent of the MR layer
grid <- st_make_grid(MR, cellsize = grid_size, square = TRUE)

# Convert grid to an sf object
grid_cd <- st_sf(geometry = grid, crs = st_crs(rs_trees))

######
grid_cd$Richness <- NA
grid_cd$Shannon <- NA
grid_cd$Simpson <- NA
grid_cd$InvSimp <- NA
grid_cd$Pielou <- NA
#grid_cd$Margalef <- NA
#grid_cd$Menhi <- NA


for(i in 1: nrow(grid_cd)){
  
  Subgrid <- st_intersection(grid_cd[i,],rs_trees_all) %>% as.data.frame() %>% select("t1_majorit") %>% na.omit() %>% table() %>% as.data.frame()
  
  # Apply threshold: only proceed if there are 3 or more trees
  if (nrow(Subgrid) >= 3) {
    
    grid_cd$Richness[i] <- nrow(Subgrid)
    
    Subgrid$prop <- Subgrid$Freq / sum(Subgrid$Freq)
    
    grid_cd$Shannon[i] <- -sum(Subgrid$prop * log(Subgrid$prop))
    
    # Simpson Index
    grid_cd$Simpson[i]<- 1-sum(Subgrid$prop^2)
    
    # Inverse Simpson Index
    grid_cd$InvSimp[i] <- 1 / sum(Subgrid$prop^2)
    
    # Pielou's Evenness Index
    grid_cd$Pielou[i] <- -sum(Subgrid$prop * log(Subgrid$prop)) / log(length(Subgrid$prop))
    
    # Margalef's Richness Index
    #grid_cd$Margalef[i] <- (length(Subgrid$prop) - 1) / log(sum(Subgrid$Freq))
    
    # Menhinick's Index
    # grid_cd$Menhi[i] <- length(Subgrid$prop) / sqrt(sum(Subgrid$Freq))
  } else {
    # If less than 3 trees, set the indices to NA for the cell
    grid_cd$Richness[i] <- NA
    grid_cd$Shannon[i] <- NA
    grid_cd$Simpson[i] <- NA
    grid_cd$InvSimp[i] <- NA
    grid_cd$Pielou[i] <- NA
  }
}

plot(grid_cd)



######################### Diversity for trees in public areas grid  (CADASTRE) #####################
# Create a new 'majority' column in the 'cadastre' dataset based on the 'GATTUNG' values
cadastre_aggregated <- cadastre_aggregated %>%
  mutate(majority = case_when(
    GATTUNG == 52 ~ 1,
    GATTUNG == 41 ~ 10,
    GATTUNG == 2  ~ 6,
    GATTUNG == 46 ~ 2,
    GATTUNG == 3  ~ 4,
    GATTUNG == 80 ~ 7,
    TRUE ~ 7  # If none of the conditions are met, assign NA
  ))

cadastre_aggregated <- st_zm(cadastre_aggregated, drop = TRUE, what = "ZM")
# Select the Gattung and geometry columns from cadastre
cadastre_selected <- cadastre_aggregated %>%
  select(majority, geometry)

# Select the majority and geometry columns from rs_trees
rs_trees_selected <- rs_trees %>%
  select(majority = t1_majorit, geometry)


# Stack the two datasets together
stacked_sf <- rbind(cadastre_selected, rs_trees_selected)

# Load your MR layer
MR <- st_read("C:/Users/ang58gl/Documents/Data/MittlererRing_new.shp")

# Define the grid size
grid_size <- 250  # 100 meters

# Create a grid over the extent of the MR layer
grid <- st_make_grid(MR, cellsize = grid_size, square = TRUE)

# Convert grid to an sf object
grid_cdrs <- st_sf(geometry = grid, crs = st_crs(stacked_sf))

######
grid_cdrs$Richness <- NA
grid_cdrs$Shannon <- NA
grid_cdrs$Simpson <- NA
grid_cdrs$InvSimp <- NA
grid_cdrs$Pielou <- NA
#grid_cdrs$Margalef <- NA
#grid_cdrs$Menhi <- NA


for(i in 1: nrow(grid_cdrs)){
  
  Subgrid <- st_intersection(grid_cdrs[i,],stacked_sf) %>% as.data.frame() %>% select("majority") %>% na.omit() %>% table() %>% as.data.frame()
  
  # Apply threshold: only proceed if there are 3 or more trees
  if (nrow(Subgrid) >= 3) {
    
    grid_cdrs$Richness[i] <- nrow(Subgrid)
    
    Subgrid$prop <- Subgrid$Freq / sum(Subgrid$Freq)
    
    grid_cdrs$Shannon[i] <- -sum(Subgrid$prop * log(Subgrid$prop))
    
    # Simpson Index
    grid_cdrs$Simpson[i]<- 1-sum(Subgrid$prop^2)
    
    # Inverse Simpson Index
    grid_cdrs$InvSimp[i] <- 1 / sum(Subgrid$prop^2)
    
    # Pielou's Evenness Index
    grid_cdrs$Pielou[i] <- -sum(Subgrid$prop * log(Subgrid$prop)) / log(length(Subgrid$prop))
    
    # Margalef's Richness Index
    #grid_cdrs$Margalef[i] <- (length(Subgrid$prop) - 1) / log(sum(Subgrid$Freq))
    
    # Menhinick's Index
    # grid_cdrs$Menhi[i] <- length(Subgrid$prop) / sqrt(sum(Subgrid$Freq))
  } else {
    # If less than 3 trees, set the indices to NA for the cell
    grid_cdrs$Richness[i] <- NA
    grid_cdrs$Shannon[i] <- NA
    grid_cdrs$Simpson[i] <- NA
    grid_cdrs$InvSimp[i] <- NA
    grid_cdrs$Pielou[i] <- NA
  }
}

plot(grid_cdrs)


grid_new <- st_join(grid_cd, grid_cdrs, join = st_equals)

st_write(grid_new, "D:/Paper_1/Diversity/Diversity_grid_all.shp")
