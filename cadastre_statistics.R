library(sf)
library(dplyr)

########################## Würzburg #######################################

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

#################################München #######################################

trees<-read.csv("D:/Trees_data/Baumstandorte_katastre.csv")

summary(trees)

#------------------------------------------------------------------------#

species_freq <- table (trees$BAUMART_LAT)
species_freq
# Order the unique values by frequency (from most frequent to least frequent)
ordered_species <- names(species_freq[order(-species_freq)])

# Create a new data frame with the ordered values and their frequencies
ordered_gattung_df <- data.frame(species = ordered_species, frequency = species_freq[order(-species_freq)])
ordered_gattung_df


# Create a table of frequencies
gattung_freq <- table(trees$GATTUNG)
gattung_freq
# Order the unique values by frequency (from most frequent to least frequent)
ordered_gattung <- names(gattung_freq[order(-gattung_freq)])

# Create a new data frame with the ordered values and their frequencies
ordered_gattung_df <- data.frame(gattung = ordered_gattung, frequency = gattung_freq[order(-gattung_freq)])
ordered_gattung_df


# #---------------------------------------------------------------------
#                        Height
#------------------------------------------------------------------------#

trees_grouped_means <- trees %>%
  group_by(GATTUNG) %>%
  summarise(mean_HOEHE_M = mean(HOEHE_M, na.rm = TRUE))

trees_grouped_means<-as.data.frame(trees_grouped_means)

print(trees_grouped_means, n=25)


trees_grouped_mins <- trees %>%
  group_by(GATTUNG) %>%
  summarise(min_HOEHE_M = min(HOEHE_M, na.rm = TRUE))

trees_grouped_mins<-as.data.frame(trees_grouped_mins)


trees_grouped_maxs <- trees %>%
  group_by(GATTUNG) %>%
  summarise(max_HOEHE_M = max(HOEHE_M, na.rm = TRUE))

trees_grouped_maxs<-as.data.frame(trees_grouped_maxs)

#--------------------------------------------------------
trees_grouped_means <- trees %>%
  group_by(BAUMART_LAT) %>%
  summarise(mean_HOEHE_M = mean(HOEHE_M, na.rm = TRUE))

trees_grouped_means<-as.data.frame(trees_grouped_means)
trees_grouped_means


trees_grouped_mins <- trees %>%
  group_by(BAUMART_LAT) %>%
  summarise(min_HOEHE_M = min(HOEHE_M, na.rm = TRUE))

trees_grouped_mins<-as.data.frame(trees_grouped_mins)
trees_grouped_mins


trees_grouped_maxs <- trees %>%
  group_by(BAUMART_LAT) %>%
  summarise(max_HOEHE_M = max(HOEHE_M, na.rm = TRUE))

trees_grouped_maxs<-as.data.frame(trees_grouped_maxs)
trees_grouped_maxs

# -----------------------------------------------------------------------
#                          DBH
#------------------------------------------------------------------------#

trees_grouped_means <- trees %>%
  group_by(GATTUNG) %>%
  summarise(mean_DBH = mean(STAMMDURCHM_1_CM, na.rm = TRUE))

trees_grouped_means<-as.data.frame(trees_grouped_means)

trees_grouped_means


trees_grouped_mins <- trees %>%
  group_by(GATTUNG) %>%
  summarise(min_DBH = min(STAMMDURCHM_1_CM, na.rm = TRUE))

trees_grouped_mins<-as.data.frame(trees_grouped_mins)
trees_grouped_mins

trees_grouped_maxs <- trees %>%
  group_by(GATTUNG) %>%
  summarise(max_DBH = max(STAMMDURCHM_1_CM, na.rm = TRUE))

trees_grouped_maxs<-as.data.frame(trees_grouped_maxs)
trees_grouped_maxs

#--------------------------------------------------------
trees_grouped_means <- trees %>%
  group_by(BAUMART_LAT) %>%
  summarise(mean_Diam = mean(STAMMDURCHM_1_CM, na.rm = TRUE))

trees_grouped_means<-as.data.frame(trees_grouped_means)
trees_grouped_means


trees_grouped_mins <- trees %>%
  group_by(BAUMART_LAT) %>%
  summarise(min_Diam = min(STAMMDURCHM_1_CM, na.rm = TRUE))

trees_grouped_mins<-as.data.frame(trees_grouped_mins)
trees_grouped_mins


trees_grouped_maxs <- trees %>%
  group_by(BAUMART_LAT) %>%
  summarise(max_Diam = max(STAMMDURCHM_1_CM, na.rm = TRUE))

trees_grouped_maxs<-as.data.frame(trees_grouped_maxs)
trees_grouped_maxs
