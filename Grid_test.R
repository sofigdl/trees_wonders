pacman::p_load(sf, terra, dplyr, ggplot2, spdep)




# Load your MR layer
MR <- st_read("C:/Users/ang58gl/Documents/Data/MittlererRing_new.shp")

# Define the grid size
grid_size <- 100  # 100 meters

# Create a grid over the extent of the MR layer
grid <- st_make_grid(MR, cellsize = grid_size, square = TRUE)

# Convert grid to an sf object
grid_sf <- st_sf(geometry = grid, crs = st_crs(rs_trees_all))


grid_sf$Simpson <- NA



for(i in 1: nrow(grid_sf)){
  
  Subgrid <- st_intersection(grid_sf[i,],rs_trees_pr) %>% as.data.frame() %>% select("t1_majorit") %>% na.omit() %>% table() %>% as.data.frame()
  
  # Simpson Index
  grid_sf$Simpson[i]<- sum(Subgrid$prop^2)
  
}

######################### MODE #####################

# Define the grid sizes to test
grid_sizes <- seq(50, 1000, by = 50)  # You can adjust the sequence as needed

# Initialize a data frame to store the results
simpson_results <- data.frame(GridSize = numeric(), ModeSimpson = numeric())

# Loop over the different grid sizes
for (grid_size in grid_sizes) {
  
  # Create the grid for the current grid size
  grid <- st_make_grid(MR, cellsize = grid_size, square = TRUE)
  grid_sf <- st_sf(geometry = grid, crs = st_crs(rs_trees_all))
  
  # Initialize Simpson column
  grid_sf$Simpson <- NA
  
  # Calculate the Simpson Index for each grid cell
  for (i in 1:nrow(grid_sf)) {
    
    # Extract points within the current grid cell
    Subgrid <- st_intersection(grid_sf[i,], rs_trees) %>% 
      as.data.frame() %>% 
      select("t1_majorit") %>% 
      na.omit() %>% 
      table() %>% 
      as.data.frame()
    
    # Calculate proportions
    Subgrid$prop <- Subgrid$Freq / sum(Subgrid$Freq)
    
    # Calculate Simpson Index and store it in the grid_sf object
    grid_sf$Simpson[i] <- sum(Subgrid$prop^2)
  }
  
  # Calculate the mode of the Simpson values
  simpson_mode <- median(grid_sf$Simpson, na.rm = TRUE)
  
  # Store the results in the data frame
  simpson_results <- rbind(simpson_results, data.frame(GridSize = grid_size, ModeSimpson = simpson_mode))
}

# Plot the results
ggplot(simpson_results, aes(x = GridSize, y = ModeSimpson)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Median of Simpson Index vs. Grid Size",
       x = "Grid Size (meters)",
       y = "Mode of Simpson Index") +
  theme_minimal()




######################### Standard deviation #####################

simpson_results_st <- data.frame(GridSize = numeric(), StdDevSimpson = numeric())

for (grid_size in grid_sizes) {
  grid <- st_make_grid(MR, cellsize = grid_size, square = TRUE)
  grid_sf <- st_sf(geometry = grid, crs = st_crs(rs_trees_all))
  grid_sf$Simpson <- NA
  
  for (i in 1:nrow(grid_sf)) {
    Subgrid <- st_intersection(grid_sf[i,], rs_trees_pr) %>% 
      as.data.frame() %>% 
      select("t1_majorit") %>% 
      na.omit() %>% 
      table() %>% 
      as.data.frame()
    
    Subgrid$prop <- Subgrid$Freq / sum(Subgrid$Freq)
    grid_sf$Simpson[i] <- sum(Subgrid$prop^2)
  }
  
  stddev_simpson <- sd(grid_sf$Simpson, na.rm = TRUE)
  simpson_results_st <- rbind(simpson_results_st, data.frame(GridSize = grid_size, StdDevSimpson = stddev_simpson))
}

ggplot(simpson_results_st, aes(x = GridSize, y = StdDevSimpson)) +
  geom_line(color = "green") +
  geom_point(color = "purple") +
  labs(title = "Standard Deviation of Simpson Index vs. Grid Size",
       x = "Grid Size (meters)",
       y = "Standard Deviation of Simpson Index") +
  theme_minimal()


########################### Spatial Autocorrelation

library(spdep)

# Initialize a data frame to store the results
morans_results <- data.frame(GridSize = numeric(), MoransI = numeric(), PValue = numeric())

# Loop over each grid size
for (grid_size in grid_sizes) {
  # Create a grid over the extent of the MR layer
  grid <- st_make_grid(MR, cellsize = grid_size, square = TRUE)
  grid_sf <- st_sf(geometry = grid, crs = st_crs(rs_trees_all))
  
  # Initialize the Simpson column
  grid_sf$Simpson <- NA
  
  # Calculate the Simpson Index for each grid cell
  for (i in 1:nrow(grid_sf)) {
    Subgrid <- st_intersection(grid_sf[i,], rs_trees_pr) %>% 
      as.data.frame() %>% 
      select("t1_majorit") %>% 
      na.omit() %>% 
      table() %>% 
      as.data.frame()
    
    if (nrow(Subgrid) > 0) {
      Subgrid$prop <- Subgrid$Freq / sum(Subgrid$Freq)
      grid_sf$Simpson[i] <- sum(Subgrid$prop^2)
    }
  }
  
  # Remove rows with NA values in Simpson Index
  grid_sf_clean <- grid_sf[!is.na(grid_sf$Simpson), ]
  
  # Create a spatial weights matrix (use queen or rook method)
  nb <- poly2nb(grid_sf_clean)  # Neighbors list
  
  # Filter out cells with no neighbors
  empty_neighbours <- sapply(nb, function(x) length(x) == 0)
  if (any(empty_neighbours)) {
    nb <- nb[!empty_neighbours]
    grid_sf_clean <- grid_sf_clean[!empty_neighbours, ]
  }
  
  # Convert neighbor list to spatial weights matrix
  lw <- nb2listw(nb, zero.policy = TRUE)  # Handle isolated cells with zero weights
  
  # Calculate Moran's I
  morans_test <- moran.test(grid_sf_clean$Simpson, lw, na.action = na.exclude, zero.policy = TRUE)
  
  # Store the results
  morans_results <- rbind(morans_results, data.frame(GridSize = grid_size, MoransI = morans_test$estimate[1], PValue = morans_test$p.value))
}

# Plot the results
ggplot(morans_results, aes(x = GridSize, y = MoransI)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Moran's I vs. Grid Size",
       x = "Grid Size (meters)",
       y = "Moran's I") +
  theme_minimal()
