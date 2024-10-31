library(sf)
library(dplyr)

joined_1<-st_read("D:/Trees_data/Soil sealing/trees_LU_joined_1.shp")
joined_2<-st_read("D:/Trees_data/Soil sealing/trees_LU_joined_2.shp")

unique(joined_1$nutzart)
unique(joined_2$nutzart)

table(joined_1$nutzart)
table(joined_2$nutzart)

# Step 1: Extract 'Soil.seali' and 'nutzart' from 'joined_1'
extracted_1 <- joined_1 %>%
  select(Soil.seali, nutzart)

# Step 2: Extract 'Soil.seali' and 'nutzart' from 'joined_2'
extracted_2 <- joined_2 %>%
  select(Soil.seali, nutzart)

# Combine the two datasets using rbind (row-bind)
combined_data <- rbind(extracted_1, extracted_2)

# Group by 'nutzart' and calculate mean of 'Soil.seali'
aggregated_data <- combined_data %>%
  group_by(nutzart) %>%
  summarise(mean_Soil_seali = mean(Soil.seali, na.rm = TRUE))

combined_data %>%
  filter(nutzart == "Fließgewässer")
