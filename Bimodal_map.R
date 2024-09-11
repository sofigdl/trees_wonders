# Sample public areas layer (replace with your actual public area layer)
public_areas <- st_read("C:/Users/ang58gl/Documents/Data/public_areas_joint.shp")
public_areas <- st_simplify(st_transform(public_areas, st_crs(MR)))


# Calculate percentage of public area in each grid cell
grid_sf$PublicAreaPercent <- sapply(1:nrow(grid_sf), function(i) {
  grid_cell <- grid_sf[i, ]
  intersection <- st_intersection(public_areas, grid_cell)
  
  if (nrow(intersection) == 0) {
    return(0)  # No overlap
  }
  
  intersection_area <- st_area(intersection) %>% sum()
  grid_cell_area <- st_area(grid_cell)
  
  return(as.numeric(intersection_area / grid_cell_area) * 100)  # Return percentage
})

# Create a bivariate color scheme
# Discretize Simpson's Index and PublicAreaPercent into categories
grid_sf$Simpson_cat <- cut(grid_sf$Simpson, breaks = quantile(grid_sf$Simpson, probs = seq(0, 1, by = 0.33), na.rm = TRUE),
                           include.lowest = TRUE, labels = c("low", "medium", "high"))

grid_sf$PublicAreaPercent_cat <- cut(grid_sf$PublicAreaPercent, breaks = quantile(grid_sf$PublicAreaPercent, probs = seq(0, 1, by = 0.33), na.rm = TRUE),
                                     include.lowest = TRUE, labels = c("low", "medium", "high"))

# Combine the categories to form a bivariate class
grid_sf$bivariate_class <- paste0(grid_sf$Simpson_cat, "-", grid_sf$PublicAreaPercent_cat)

# Define a bivariate color palette
bivariate_colors <- c(
  "low-low" = "#e8e8e8",  "low-medium" = "#b3d1e1",  "low-high" = "#64acbe",
  "medium-low" = "#cbb8d7", "medium-medium" = "#a5add3", "medium-high" = "#4b84c4",
  "high-low" = "#9972a3",   "high-medium" = "#8063a2",   "high-high" = "#3b4994"
)

# Assign colors to grid cells based on their bivariate class
grid_sf$bivariate_color <- bivariate_colors[grid_sf$bivariate_class]

# Plot the map
ggplot() +
  geom_sf(data = grid_sf, aes(fill = bivariate_color), color = NA, alpha = 0.75) +
  scale_fill_identity() +
  coord_sf() +
  labs(
    title = "Bivariate Map of Simpson's Index and Public Area Percentage",
    fill = "Simpson's Index / Public Area %",
    caption = "Bivariate color scheme: \nlow = low, medium = medium, high = high"
  ) +
  theme_minimal()

# Create a custom legend
legend <- expand.grid(
  Simpson = c("low", "medium", "high"),
  PublicArea = c("low", "medium", "high")
) %>%
  mutate(bivariate_class = paste0(Simpson, "-", PublicArea),
         color = bivariate_colors[bivariate_class])

ggplot(legend, aes(x = Simpson, y = PublicArea, fill = color)) +
  geom_tile() +
  scale_fill_identity() +
  labs(x = "Simpson's Index", y = "Public Area %") +
  theme_minimal()

