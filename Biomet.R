library(sf)
library(ggplot2)

nutzung<-st_read("C:/Users/ang58gl/Documents/Data/nutzung_MittlererRing.gpkg")

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


joined_data <- st_join(density_by_class, area_by_class, by = "nutzart")

joined_data$density <- joined_data$density / joined_data$area