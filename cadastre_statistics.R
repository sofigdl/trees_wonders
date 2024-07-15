library(sf)
library(dplyr)

#Load vector
TBK_trees<-st_read("D:/Trees_data/baumkataster_stadt_wuerzburg.shp")

head(TBK_trees)

TBK_trees <- TBK_trees %>%
  mutate(gattung = sub("^(\\w+).*", "\\1", baumart_la))

aoi<- st_read("C:/Users/ang58gl/Documents/Data/SG_AOI_Würzburg.gpkg")


new_trees<-st_intersection(TBK_trees, aoi)

# Create a table of frequencies
gattung_freq <- table(new_trees$gattung)

# Order the unique values by frequency (from most frequent to least frequent)
ordered_gattung <- names(gattung_freq[order(-gattung_freq)])

# Create a new data frame with the ordered values and their frequencies
ordered_gattung_df <- data.frame(gattung = ordered_gattung, frequency = gattung_freq[order(-gattung_freq)])
ordered_gattung_df

