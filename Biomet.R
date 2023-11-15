library(sf)
library(ggplot2)
library(dplyr)
library(viridis)
library(RColorBrewer)

land_use<-st_read("C:/Users/ang58gl/Documents/Data/nutzung_MittlererRing.gpkg")

LU_classes<-as.data.frame(table(nutzung$nutzart))
LU_classes

# Order the data frame by Frequency in descending order
df <- LU_classes[order(-LU_classes$Freq), ]

# Convert Var1 to a factor and reorder its levels by Frequency
df$Var1 <- factor(df$Var1, levels = df$Var1)

# Create a histogram using ggplot2
ggplot(df, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
 # coord_flip() +  # Flip the coordinates to create a horizontal bar chart
  labs(title = "Frequency Histogram", x = "Var1", y = "Frequency") +
  theme_minimal()


tree_counts<-st_read("D:/BIOMET/counts_trees.gpkg")

summary(tree_counts)

points_by_class <- tree_counts %>%
  group_by(nutzart) %>%
  summarize(TotalPoints = sum(NUMPOINTS))

area_by_class <- tree_counts %>%
  group_by(nutzart) %>%
  summarize(TotalArea = sum(area))


joined_data <- left_join(as.data.frame(points_by_class), as.data.frame(area_by_class), by = "nutzart")

joined_data$density <- joined_data$TotalPoints/ joined_data$TotalArea


# Order the data frame by Frequency in descending order
joined_data <- joined_data[order(-joined_data$density), ]

# Convert Var1 to a factor and reorder its levels by Frequency
joined_data$Var1 <- factor(joined_data$nutzart, levels = joined_data$nutzart)

joined_data<- joined_data %>%
  filter(Var1!="Stehendes Gewässer")

joined_data<- joined_data %>%
  filter(Var1!="Friedhof")

# Create a histogram using ggplot2
ggplot(joined_data, aes(x = Var1, y = density, fill = Var1)) +
  geom_col() +
  # coord_flip() +  # Flip the coordinates to create a horizontal bar chart
  labs(title = "Tree density by class", x = "Land use", y = "Density") +
  theme_minimal() +
  theme(
    text = element_text(size = 20),  # Adjust the font size of text elements
    plot.title = element_text(size = 24, face = "bold"),  # Adjust the font size and style of the plot title
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set3")


trees_seg <- st_read("D:/Trees_data/Tree_segments_MR.gpkg")

# Ensure both layers have the same coordinate reference system (CRS)
trees_seg <- st_transform(trees_seg, st_crs(land_use))

# Intersect the land use and tree polygons
intersected <- st_intersection(land_use, trees_seg)

# Calculate the area of each original land use polygon
land_use$land_use_area <- st_area(land_use)

# Calculate the area of each intersected polygon
intersected$intersected_area <- st_area(intersected)


join_trees <- st_join(intersected, land_use)

# Calculate the percentage of land use covered by trees
join_trees$percentage_covered <- (join_trees$intersected_area / join_trees$land_use_area) * 100

# Display the result
print(intersected[, c("land_use_id", "percentage_covered")])