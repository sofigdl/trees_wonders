######################### Diversity for trees in public areas grid (REMOTE) #####################


# Load your MR layer
MR <- st_read("C:/Users/ang58gl/Documents/Data/MittlererRing_new.shp")

# Define the grid size
grid_size <- 100  # 100 meters

# Create a grid over the extent of the MR layer
grid <- st_make_grid(MR, cellsize = grid_size, square = TRUE)

# Convert grid to an sf object
grid_sf <- st_sf(geometry = grid, crs = st_crs(rs_trees_all))

######
grid_sf$Richness <- NA
grid_sf$Shannon <- NA
grid_sf$Simpson <- NA
grid_sf$InvSimp <- NA
grid_sf$Pielou <- NA
grid_sf$Margalef <- NA
grid_sf$Menhi <- NA


for(i in 1: nrow(grid_sf)){
  
  Subgrid <- st_intersection(grid_sf[i,],rs_trees) %>% as.data.frame() %>% select("t1_majorit") %>% na.omit() %>% table() %>% as.data.frame()
  
  grid_sf$Richness[i] <- nrow(Subgrid)
  
  Subgrid$prop <- Subgrid$Freq / sum(Subgrid$Freq)
  
  grid_sf$Shannon[i] <- -sum(Subgrid$prop * log(Subgrid$prop))
  
  # Simpson Index
  grid_sf$Simpson[i]<- sum(Subgrid$prop^2)
  
  # Inverse Simpson Index
  grid_sf$InvSimp[i] <- 1 / sum(Subgrid$prop^2)
  
  # Pielou's Evenness Index
  grid_sf$Pielou[i] <- -sum(Subgrid$prop * log(Subgrid$prop)) / log(length(Subgrid$prop))
  
  # Margalef's Richness Index
  grid_sf$Margalef[i] <- (length(Subgrid$prop) - 1) / log(sum(Subgrid$Freq))
  
  # Menhinick's Index
  grid_sf$Menhi[i] <- length(Subgrid$prop) / sqrt(sum(Subgrid$Freq))
}

plot(grid_sf)


# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_sf, aes(fill = Richness), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Richness") +
  # Additional map elements
  labs(title = "Genera Richness",
       subtitle = "Base Map: OpenStreetMap"
  ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_sf))


# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_sf, aes(fill = Shannon), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Shannon Index") +
  # Additional map elements
  labs(title = "Shannon Index in Public Areas (Remote Sensing)",
       subtitle = "Base Map: OpenStreetMap"
  ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_sf))

# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_sf, aes(fill = Simpson), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Simpson Index") +
  # Additional map elements
  labs(title = "Simpson's Diversity Index in Public Areas (Remote Sensing)",
       subtitle = "Base Map: OpenStreetMap"
  ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_sf))


# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_sf, aes(fill = InvSimp), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Inverse Simpson Index") +
  # Additional map elements
  labs(title = "Inverse Simpson's Diversity Index in Public Areas (Remote Sensing)",
       subtitle = "Base Map: OpenStreetMap"
  ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_sf))


# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_sf, aes(fill = Pielou), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Pielou Index") +
  # Additional map elements
  labs(title = "Pielou's Evenness Index in Public Areas (Remote Sensing)",
       subtitle = "Base Map: OpenStreetMap"
  ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_sf))

# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_sf, aes(fill = Margalef), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Margalef Index") +
  # Additional map elements
  labs(title = "Margalef's Richness Index in Public Areas (Remote Sensing)",
       subtitle = "Base Map: OpenStreetMap"
  ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_sf))


# Plot the map with OpenStreetMap as the base layer
ggplot() +
  # Add OpenStreetMap tiles
  annotation_map_tile(type = "osm", zoom = 14) +
  # Add the grid layer with 75% opacity
  geom_sf(data = grid_sf, aes(fill = Menhi), color = NA, alpha = 0.65) +
  # Use a blue-to-yellow color palette
  scale_fill_viridis(option = "D", direction = 1, name = "Menhinick's Index") +
  # Additional map elements
  labs(title = "Menhinick's Index in Public Areas (Remote Sensing)",
       subtitle = "Base Map: OpenStreetMap"
  ) +
  theme_minimal() +
  # Ensure the map uses the same CRS
  coord_sf(crs = st_crs(grid_sf))

######################### Diversity for trees in public areas grid  (CADASTRE) #####################


# Load your MR layer
MR <- st_read("C:/Users/ang58gl/Documents/Data/MittlererRing_new.shp")

# Define the grid size
grid_size <- 100  # 100 meters

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
grid_cd$Margalef <- NA
grid_cd$Menhi <- NA


for(i in 1: nrow(grid_cd)){
  
  Subgrid <- st_intersection(grid_cd[i,],cadastre) %>% as.data.frame() %>% select("GATTUNG") %>% na.omit() %>% table() %>% as.data.frame()
  
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
  grid_cd$Margalef[i] <- (length(Subgrid$prop) - 1) / log(sum(Subgrid$Freq))
  
  # Menhinick's Index
  grid_cd$Menhi[i] <- length(Subgrid$prop) / sqrt(sum(Subgrid$Freq))
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
grid_size <- 100  # 100 meters

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
grid_pri$Margalef <- NA
grid_pri$Menhi <- NA


for(i in 1: nrow(grid_pri)){
  
  Subgrid <- st_intersection(grid_pri[i,],rs_trees_pr) %>% as.data.frame() %>% select("t1_majorit") %>% na.omit() %>% table() %>% as.data.frame()
  
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
  grid_pri$Margalef[i] <- (length(Subgrid$prop) - 1) / log(sum(Subgrid$Freq))
  
  # Menhinick's Index
  grid_pri$Menhi[i] <- length(Subgrid$prop) / sqrt(sum(Subgrid$Freq))
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
