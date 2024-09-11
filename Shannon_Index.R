pacman::p_load(sf, terra, dplyr, ggplot2, viridis, ggspatial, prettymapr)


######################### Diversity for trees in public areas total #####################

rs_trees_all<-st_read("D:/Paper_1/Diversity/RS_trees.shp")
cadastre_all<-st_read("D:/Paper_1/Diversity/Cadastre_trees.shp")


# Filter the data frames to include only rows where 'public' is 1
rs_trees <- rs_trees_all[rs_trees_all$Public == 1, ]
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
  simpson_index <- sum(data$Prop^2)
  
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
  grid_rs$Simpson[i]<- sum(Subgrid$prop^2)
  
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
  grid_cd$Simpson[i]<- sum(Subgrid$prop^2)
  
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
grid_size <- 200  # 100 meters

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
  grid_pri$Simpson[i]<- sum(Subgrid$prop^2)
  
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
