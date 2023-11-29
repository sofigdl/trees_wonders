install.packages("MASS")
library(sf)
library(ggplot2)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(broom)
library(ggpmisc)
library(MASS)

#----------------------------------------------------------------------------------------------------------
#----------                               Data Preparation                                    -------------
#----------------------------------------------------------------------------------------------------------

# Extracting tree information for each polygon
#----------------------------------------------------------------------------------------------------

#Import land use polygons
land_use<-st_read("D:/BIOMET/LST/D_2020.gpkg")

#----------------------------------------------------------------------------------------------------
#Import trees
tree_points <-st_read("D:/Trees_data/Mittel_ring_baüme_Tobias.gpkg")
tree_segments <- st_read("D:/Trees_data/Tree_segments_MR.gpkg")

# Ensure both layers have the same coordinate reference system (CRS)
tree_points <- st_transform(tree_points, st_crs(land_use))
# Ensure both layers have the same coordinate reference system (CRS)
tree_segments <- st_transform(tree_segments, st_crs(land_use))


#Perform spatial join between points and land use
trees_joint <- st_join(tree_points, land_use, join = st_within)

#Count points in each polygon
tree_counts <- trees_joint %>%
  group_by(gml_id) %>%
  summarize(point_count = n())

#------------------------------------

## Intersect the land use and tree polygons
intersected <- st_intersection(land_use, tree_segments)

# Estimate the area
intersected <- intersected %>%
  mutate(tree_area = st_area(.))

# Assuming your spatial intersection resulted in a new layer named 'intersected_data'
tree_areas<-intersected%>%
  group_by(gml_id) %>%
  summarize(total_tree_area = sum(tree_area))

#join the counts and the area of trees per land use polygon
trees_info <- left_join(as.data.frame(tree_counts), as.data.frame(tree_areas), by = "gml_id")

#focus on the variables of interest
trees_info <- trees_info[c("gml_id", "point_count", "total_tree_area")]

#join them to the land use
trees_landuse <- left_join(land_use, trees_info, by = "gml_id")

#adding the area
trees_landuse$area <- st_area(trees_landuse)

#estimate tree density
trees_landuse$density <- trees_landuse$point_count/ trees_landuse$area
trees_landuse$density[is.na(trees_landuse$density)] <- 0


#estimate area percentage
trees_landuse$percentage <- trees_landuse$total_tree_area/ trees_landuse$area * 100
#trees_landuse$percentage[is.na(trees_landuse$percentage)] <- 0
#trees_landuse$point_count[is.na(trees_landuse$point_count)] <- 0
#trees_landuse$total_tree_area[is.na(trees_landuse$total_tree_area)] <- 0
#trees_landuse$X_mean[is.na(trees_landuse$X_mean)] <- 35

# Now we involve the LST. The zonal stats were extracted by Antonio
#lst_20<-st_read("D:/BIOMET/LST/Zonal_2020.gpkg")

# Ensure both layers have the same coordinate reference system (CRS)
#lst_20<- st_transform(lst_20, st_crs(trees_landuse))
#head(lst_20)

#join them to the land use
#trees_landuse <- leftt_join(trees_landuse, as.data.frame(lst_20), by = "gml_id")

head(trees_landuse)

#We filter certain land use classes: classes with too few polygons and water
trees_landuse <- trees_landuse %>%
  filter(nutzart!="Stehendes Gewässer")

trees_landuse<- trees_landuse%>%
  filter(nutzart!="Friedhof")

trees_landuse<- trees_landuse%>%
  filter(nutzart!="Fließgewässer")

#----------------------------------------------------------------------------------------------------------
#----------                                    Plots                                          -------------
#----------------------------------------------------------------------------------------------------------
trees_landuse <- trees_landuse %>%
  mutate(LU = case_when(
    nutzart == "Bahnverkehr" ~ "Traffic",
    nutzart == "Fläche besonderer funktionaler Prägung" ~ "Industrial",
    nutzart == "Fläche gemischter Nutzung" ~ "Mixed",
    nutzart == "Gehölz" ~ "Recreational",
    nutzart == "Industrie- und Gewerbefläche" ~ "Industrial",
    nutzart == "Platz" ~ "Recreational",
    nutzart == "Sport-, Freizeit- und Erholungsfläche" ~ "Recreational",
    nutzart == "Straßenverkehr" ~ "Traffic",
    nutzart == "Unland/Vegetationslose Fläche" ~ "Recreational",
    nutzart == "Weg" ~ "Traffic",
    nutzart == "Wohnbaufläche" ~ "Residential",
    TRUE ~ "Other"  # Default for unmatched cases
  ))


#Land use threshold
#----------------------------------------------------------------------------------------------------

LU_classes<-as.data.frame(table(trees_landuse$LU))
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


# Density of trees per land use class
#------------------------------------------------------------------------------------------------------
# Assuming your spatial intersection resulted in a new layer named 'intersected_data'
tree_density<-trees_landuse%>%
  group_by(LU) %>%
  summarize(total_density = mean(density))

summary(trees_landuse)

points_by_class <- trees_landuse %>%
  group_by(LU) %>%
  summarize(TotalPoints = sum(as.numeric(point_count)))

area_by_class <- trees_landuse %>%
  group_by(LU) %>%
  summarize(TotalArea = sum(area))


joined_data <- left_join(as.data.frame(points_by_class), as.data.frame(area_by_class), by = "LU")

joined_data$density <- as.numeric(joined_data$TotalPoints/ joined_data$TotalArea)

# Order the data frame by Frequency in descending order
#joined_data <- joined_data[order(-joined_data$density), ]
# Convert Var1 to a factor and reorder its levels by Frequency
#joined_data$Var1 <- factor(joined_data$LU, levels = joined_data$LU)

# Create a histogram using ggplot2
ggplot(joined_data, aes(x = LU, y = density, fill = LU)) +
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

#----------------------------------------------------------------------------------------------------------

# Group by land use ID and summarize the intersected data
crown_by_class <- trees_landuse %>%
  group_by(LU) %>%
  summarize(covered_area = sum(total_tree_area))

# Merge the summaries
final_summary <- left_join(as.data.frame(crown_by_class), as.data.frame(area_by_class), by = "LU")

# Calculate the percentage covered
final_summary <- final_summary %>%
  mutate(percentage_covered = (covered_area / TotalArea) * 100)

# Convert the percentage_covered column to numeric
final_summary$percentage_covered <- as.numeric(final_summary$percentage_covered)

# Order the data frame by Frequency in descending order
#final_summary <- final_summary[order(final_summary$LU), ]

# Plot the results
ggplot(final_summary, aes(x = LU, y = percentage_covered, fill = LU)) +
  geom_col() +
  labs(title = "Tree cover by class", x = "Land use", y = "Tree cover (%)") +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    plot.title = element_text(size = 24, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set3")


#--------------------------------------------------------------------------------------------


# Group by land use ID and summarize the intersected data
lst_by_class <- trees_landuse %>%
  group_by(LU) %>%
  summarize(lst = mean(na.omit(X_mean)))


# Plot the results
ggplot(lst_by_class, aes(x = LU, y = lst, fill = LU)) +
  geom_col() +
  labs(title = "Land surface temperature by class", x = "Land use", y = "LST (°C)") +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    plot.title = element_text(size = 24, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  scale_fill_brewer(palette = "Set3")

#--------------------------------------------------------------------------------------------


#Regression models

# Plot the regression models and store model summaries
gg<-ggplot(trees_landuse[trees_landuse$LU == "Residential",], aes(x = (as.numeric(percentage)), y = X_median, color = LU)) +
  geom_point() +
  labs(title = "Regression Models for Different Land Use Classes",
       x = "Tree area (%)", y = "Mean LST") +
  stat_poly_line() +
  stat_poly_eq() +
  scale_x_continuous(limits = c(0,100), expand = c(0,0)) +
  scale_y_continuous(limits = c(30,50), expand = c(0,0)) +
  theme(
    text = element_text(size = 20),
    plot.title = element_text(size = 24, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )+
  theme_minimal()

gg


get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

trees_landuse2 <- trees_landuse[,c("nutzart","LU", "X_mean","X_median","percentage")]

trees_landuse2 <- na.omit(trees_landuse2)
trees_landuse2$densityplot <- get_density(x = (as.numeric(trees_landuse2$percentage)), y = trees_landuse2$X_median, n = 400)
ggplot(trees_landuse2[trees_landuse2$LU=="Industrial",], aes(x = (as.numeric(percentage)), y = X_median,color = densityplot)) + geom_point() +
  scale_color_viridis() +
  theme_dark() +
  theme(panel.background = element_rect(fill = "black"))
