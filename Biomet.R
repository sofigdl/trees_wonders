install.packages("viridis")
library(sf)
library(ggplot2)
library(dplyr)
library(viridis)
library(RColorBrewer)
library(broom)
library(ggpmisc)
library(MASS)
library(ggpubr)

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

trees_points<- read.csv("D:/Simulations/All_trees.csv")
trees_points<- st_as_sf(trees_joint, coords = c("Lon", "Lat"), crs=4326)
  
# Ensure both layers have the same coordinate reference system (CRS)
tree_points <- st_transform(tree_points, st_crs(land_use))
# Ensure both layers have the same coordinate reference system (CRS)
tree_segments <- st_transform(tree_segments, st_crs(land_use))


#Perform spatial join between points and land use
trees_points <- st_transform(trees_points, crs = 32632)
trees_joint <- st_join(trees_points, land_use, join = st_within)

#Count points in each polygon
tree_counts <- trees_joint %>%
  group_by(gml_id) %>%
  summarize(point_count = n())

# Calculate mean height for each polygon
mean_height <- trees_joint %>%
  group_by(gml_id) %>%
  summarize(mean_height = mean(height, na.rm = TRUE))

# Calculate mean CPA for each polygon
mean_CPA <- trees_joint %>%
  group_by(gml_id) %>%
  summarize(mean_cpa = mean(CPA, na.rm = TRUE))

# Calculate mean dbh for each polygon
mean_dbh <- trees_joint %>%
  group_by(gml_id) %>%
  summarize(mean_dbh = mean(dbh, na.rm = TRUE))

# Calculate mean cooling for each polygon
mean_cool_sh <- trees_joint %>%
  group_by(gml_id) %>%
  summarize(mean_shading = mean(cool.sh..su, na.rm = TRUE))

# Calculate mean cooling for each polygon
mean_cool_tr <- trees_joint %>%
  group_by(gml_id) %>%
  summarize(mean_transpiration = mean(cool.tr..su, na.rm = TRUE))

# Join the datasets by gml_id
tree_stats <- tree_counts %>%
  st_join(mean_height, by = "gml_id")%>%
  st_join(mean_CPA, by = "gml_id")%>%
  st_join(mean_dbh, by = "gml_id")%>%
  st_join(mean_cool_sh, by = "gml_id")%>%
  st_join(mean_cool_tr, by = "gml_id")


cleaned_data <- tree_stats %>% 
  dplyr::select(1,2,3,5,7,9,11,13)
    
trees_landuse<-st_join(land_use, cleaned_data, by ="gml_id")

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
gg<-ggplot(trees_landuse[trees_landuse$LU == "Residential",], aes(x = (as.numeric(percentage)), y = X_mean)) +
  geom_point(color = "#d3692c") +
  labs(title = "Residential",
       x = "Tree canopy cover (%)", y = "Mean land surface temperature (°C)") +
  stat_poly_line(color = "#d3692c") +
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

trees_landuse2$trees<-as.numeric(trees_landuse2$percentage)

trees_landuse2 <- na.omit(trees_landuse2)
trees_landuse2$densityplot <- get_density(x = (as.numeric(trees_landuse2$percentage)), y = trees_landuse2$X_median, n = 400)
ggplot(trees_landuse2[trees_landuse2$LU=="Industrial",], aes(x = (as.numeric(percentage)), y = X_median,color = densityplot)) + geom_point() +
  scale_color_viridis() +
  theme_dark() +
  theme(panel.background = element_rect(fill = "black"))


#--------------------------------------------------------------------------------

p<-ggplot(trees_landuse2, aes(x=LU, y=X_mean, fill=LU)) +
  geom_violin(trim=FALSE) +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  scale_fill_manual(values=c("#23bdce", "#d6489b", "#7b8313", "#d3692c", "#6561da"))+
  labs(x="Land use class", y = "Mean land surface temperature (°C)", size= 16)+theme_light()+
  theme(legend.position = "none", axis.title=element_text(size=24), axis.text=element_text(size=20))+ ylim(31, 42)

p

ggplot(trees_landuse2, aes(x=LU, y=trees, fill=LU)) +
  geom_violin(trim=FALSE) +
  stat_summary(fun.y=median, geom="point", size=2, color="black")+
  scale_fill_manual(values=c("#23bdce", "#d6489b", "#7b8313", "#d3692c", "#6561da"))+
  labs(x="Land use class", y = "Tree canopy cover (%)")+theme_light()+
  theme(legend.position = "none", axis.title=element_text(size=24), axis.text=element_text(size=20))+ ylim(0, 100)

c

#Regression models

# Plot the regression models and store model summaries
ggplot(trees_landuse[trees_landuse$LU == "Residential",], aes(x = (as.numeric(percentage)), y = X_mean)) +
  geom_point(color = "#d3692c") +
  labs(title = "Residential",
       x = "Tree canopy cover (%)", y = "Mean land surface temperature (°C)") +
  geom_smooth(method = "lm", se=FALSE, color = "#d3692c") +
  stat_regline_equation(label.x = 70,label.y = 41.5, size =5, aes(label = ..eq.label..))+
  stat_regline_equation(label.x = 70,label.y = 41, size =5, aes(label = ..rr.label..))+
  scale_x_continuous(limits = c(0,100), expand = c(0,0)) +
  scale_y_continuous(limits = c(30,42), expand = c(0,0)) +
  theme(
    text = element_text(size = 24),
    plot.title = element_text(size = 40, face = "bold"),
    axis.title=element_text(size=24),
    axis.text = element_text(size=20),
    legend.position = "none"
  )+
  theme_minimal()

ggplot(trees_landuse[trees_landuse$LU == "Traffic",], aes(x = (as.numeric(percentage)), y = X_mean)) +
  geom_point(color = "#6561da") +
  labs(title = "Traffic",
       x = "Tree canopy cover (%)", y = "Mean land surface temperature (°C)") +
  geom_smooth(method = "lm", se=FALSE, color = "#6561da") +
  stat_regline_equation(label.x = 70,label.y = 41.5, size =5, aes(label = ..eq.label..))+
  stat_regline_equation(label.x = 70,label.y = 41, size =5, aes(label = ..rr.label..))+
  scale_x_continuous(limits = c(0,100), expand = c(0,0)) +
  scale_y_continuous(limits = c(30,42), expand = c(0,0)) +
  theme(
    text = element_text(size = 24),
    plot.title = element_text(size = 40, face = "bold"),
    axis.title=element_text(size=24),
    axis.text = element_text(size=20),
    legend.position = "none"
  )+
  theme_minimal()

ggplot(trees_landuse[trees_landuse$LU == "Recreational",], aes(x = (as.numeric(percentage)), y = X_mean)) +
  geom_point(color = "#7b8313") +
  labs(title = "Recreational",
       x = "Tree canopy cover (%)", y = "Mean land surface temperature (°C)") +
  geom_smooth(method = "lm", se=FALSE, color = "#7b8313") +
  stat_regline_equation(label.x = 70,label.y = 41.5, size =5, aes(label = ..eq.label..))+
  stat_regline_equation(label.x = 70,label.y = 41, size =5, aes(label = ..rr.label..))+
  scale_x_continuous(limits = c(0,100), expand = c(0,0)) +
  scale_y_continuous(limits = c(30,42), expand = c(0,0)) +
  theme(
    text = element_text(size = 24),
    plot.title = element_text(size = 40, face = "bold"),
    axis.title=element_text(size=24),
    axis.text = element_text(size=20),
    legend.position = "none"
  )+
  theme_minimal()

ggplot(trees_landuse[trees_landuse$LU == "Mixed",], aes(x = (as.numeric(percentage)), y = X_mean)) +
  geom_point(color = "#d6489b") +
  labs(title = "Mixed",
       x = "Tree canopy cover (%)", y = "Mean land surface temperature (°C)") +
  geom_smooth(method = "lm", se=FALSE, color = "#d6489b") +
  stat_regline_equation(label.x = 70,label.y = 41.5, size =5, aes(label = ..eq.label..))+
  stat_regline_equation(label.x = 70,label.y = 41, size =5, aes(label = ..rr.label..))+
  scale_x_continuous(limits = c(0,100), expand = c(0,0)) +
  scale_y_continuous(limits = c(30,42), expand = c(0,0)) +
  theme(
    text = element_text(size = 24),
    plot.title = element_text(size = 40, face = "bold"),
    axis.title=element_text(size=24),
    axis.text = element_text(size=20),
    legend.position = "none"
  )+
  theme_minimal()


ggplot(trees_landuse[trees_landuse$LU == "Industrial",], aes(x = (as.numeric(percentage)), y = X_mean)) +
  geom_point(color = "#23bdce") +
  labs(title = "Industrial",
       x = "Tree canopy cover (%)", y = "Mean land surface temperature (°C)") +
  geom_smooth(method = "lm", se=FALSE, color = "#23bdce") +
  stat_regline_equation(label.x = 60,label.y = 41.5, size =6, aes(label = ..eq.label..))+
  stat_regline_equation(label.x = 60,label.y = 41, size =6, aes(label = ..rr.label..))+
  scale_x_continuous(limits = c(0,105), expand = c(0,0)) +
  scale_y_continuous(limits = c(30,42), expand = c(0,0)) +
  theme(
    text = element_text(size = 24),
    plot.title = element_text(size = 40, face = "bold"),
    axis.title=element_text(size=24),
    axis.text = element_text(size=20),
    legend.position = "none"
  )+
  theme_minimal()


ggplot(trees_landuse, aes(x = (as.numeric(percentage)), y = X_mean))+#, color=LU)) +
  geom_point(color="#1a1042") +
  labs(x = "Tree canopy cover (%)", y = "Mean land surface temperature (°C)") +
  geom_smooth(method = "lm", se=TRUE, color="#f97b5d", linewidth = 2) +
  scale_fill_manual(values=c("#23bdce", "#d6489b", "#7b8313", "#d3692c", "#6561da"))+
  stat_regline_equation(label.x = 75,label.y = 41.5, size =6, aes(label = ..eq.label..))+
  stat_regline_equation(label.x = 75,label.y = 41, size =6, aes(label = ..rr.label..))+
  scale_x_continuous(limits = c(0,105), expand = c(0,0)) +
  scale_y_continuous(limits = c(31,42), expand = c(0,0)) +
  theme_minimal(base_size = 18)+
  theme(
    plot.title = element_text(size = 32, face = "bold"),
    axis.title=element_text(size=22),
    axis.text = element_text(size=18),
    legend.position = "none"
  )



#---------------------------------------------------------------------------------

ggplot(trees_landuse, aes(x = (as.numeric(mean_height)), y = X_mean,  color = LU))+
  geom_point(alpha = 0.4)+
  labs(x = "Mean height (m)",  y = "Mean land surface temperature (°C)", legend = "Land Use") +
  #geom_smooth(method = "lm", formula = y ~ poly(x, 3), color = "black", se = FALSE) +
 
  scale_color_manual(values=c(
    "Residential" = "#B53022", 
    "Traffic" = "#323432", 
    "Recreational" = "#81953C", 
    "Industrial" = "#DD6E42", 
    "Mixed" = "#FFBC42"))+
  #stat_regline_equation(label.x = 75,label.y = 41.5, size =6, aes(label = ..eq.label..))+
  #stat_regline_equation(label.x = 75,label.y = 41, size =6, aes(label = ..rr.label..))+
  scale_x_continuous(limits = c(0,40), expand = c(0,0)) +
  scale_y_continuous(limits = c(31,42), expand = c(0,0)) +
  theme_classic(base_size = 18)+
  theme(
    plot.title = element_text(size = 32, face = "bold"),
    axis.title=element_text(size=22),
    axis.text = element_text(size=18)
  )+annotate("text", x = 25, y = 41, label = paste("r =-0.44"))
 



  ggplot(trees_landuse, aes(x = (as.numeric(mean_dbh)), y = X_mean,  color = LU))+
    geom_point(alpha = 0.4)+
    labs(x = "Mean dbh (cm)",  y = "Mean land surface temperature (°C)", legend = "Land Use") +
    #geom_smooth(method = "lm", formula = y ~ poly(x, 3), color = "black", se = FALSE) +
    
    scale_color_manual(values=c(
      "Residential" = "#B53022", 
      "Traffic" = "#323432", 
      "Recreational" = "#81953C", 
      "Industrial" = "#DD6E42", 
      "Mixed" = "#FFBC42"))+
    #stat_regline_equation(label.x = 75,label.y = 41.5, size =6, aes(label = ..eq.label..))+
    #stat_regline_equation(label.x = 75,label.y = 41, size =6, aes(label = ..rr.label..))+
    #scale_x_continuous(limits = c(0,40), expand = c(0,0)) +
    scale_y_continuous(limits = c(31,42), expand = c(0,0)) +
    theme_classic(base_size = 18)+
    theme(
      plot.title = element_text(size = 32, face = "bold"),
      axis.title=element_text(size=22),
      axis.text = element_text(size=18)
    )+annotate("text", x = 100, y = 41, label = paste("r =-0.54"))
  
  cor.test(trees_landuse$mean_dbh, trees_landuse$X_mean)
  
  
ggplot(trees_landuse, aes(x = (as.numeric(mean_shading)), y = X_mean,  color = LU))+
    geom_point(alpha = 0.4)+
    labs(x = "Mean CPA (cm)",  y = "Mean land surface temperature (°C)", legend = "Land Use") +
    #geom_smooth(method = "lm", formula = y ~ poly(x, 3), color = "black", se = FALSE) +
    
    scale_color_manual(values=c(
      "Residential" = "#B53022", 
      "Traffic" = "#323432", 
      "Recreational" = "#81953C", 
      "Industrial" = "#DD6E42", 
      "Mixed" = "#FFBC42"))+
    #stat_regline_equation(label.x = 75,label.y = 41.5, size =6, aes(label = ..eq.label..))+
    #stat_regline_equation(label.x = 75,label.y = 41, size =6, aes(label = ..rr.label..))+
    scale_x_continuous(limits = c(0,150000), expand = c(0,0)) +
    scale_y_continuous(limits = c(31,42), expand = c(0,0)) +
    theme_classic(base_size = 18)+
    theme(
      plot.title = element_text(size = 32, face = "bold"),
      axis.title=element_text(size=22),
      axis.text = element_text(size=18)
    )+annotate("text", x = 150000, y = 41, label = paste("r =-0.56"))
  
  cor.test(trees_landuse$mean_cpa, trees_landuse$X_mean)
  


