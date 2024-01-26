install.packages("viridis")
library(sf)
library(tidyverse)
library(terra)
library(dplyr)
library(raster)
library(exactextractr)
library(RColorBrewer)
library(ggplot2)

#Load data
str_trees<-st_read("D:/Paper_1/Class_streets_t1.gpkg")
prk_trees<-st_read("D:/Paper_1/Class_parks_t1.gpkg")
res_trees<-st_read("D:/Paper_1/Class_resi_t1.gpkg")
oth_trees<-st_read("D:/Paper_1/Class_other_t1.gpkg")


str_class<-rast("D:/Classifications/Escalonada/RF/rf_12_st_oficial_v71.tif")
prk_class<-rast("D:/Classifications/Escalonada/RF/rf_12_prk_oficial_v71.tif")
res_class<-rast("D:/Classifications/Escalonada/RF/rf_12_resi_oficial_v72.tif")
oth_class<-rast("D:/Classifications/Escalonada/RF/rf_12_oth_oficial_v71.tif")


trees_chm<-rast("D:/nDSM_Muc.tif")
pcth <- function(x, p=0.90, na.rm = TRUE) { quantile(x, p, na.rm = na.rm) }

str_trees$height_90 <- (extract(trees_chm, str_trees, fun=pcth))[[2]]
prk_trees$height_90 <- (extract(trees_chm, prk_trees, fun=pcth))[[2]]
res_trees$height_90 <- (extract(trees_chm, res_trees, fun=pcth))[[2]]
oth_trees$height_90 <- (extract(trees_chm, oth_trees, fun=pcth))[[2]]

#Extract the majoritarian class with the function modal
str_trees$genus <- (terra::extract(str_class, str_trees, fun="modal", na.rm=TRUE))[[2]]
prk_trees$genus <- (terra::extract(prk_class, prk_trees, fun="modal", na.rm=TRUE))[[2]]
res_trees$genus <- (terra::extract(res_class, res_trees, fun="modal", na.rm=TRUE))[[2]]
oth_trees$genus <- (terra::extract(oth_class, oth_trees, fun="modal", na.rm=TRUE))[[2]]

#Extract the fractions with the function "frac"
str_fractions<-exactextractr::exact_extract(str_class, str_trees, append_cols=TRUE, fun='frac')
prk_fractions<-exactextractr::exact_extract(prk_class, prk_trees, append_cols=TRUE, fun='frac')
res_fractions<-exactextractr::exact_extract(res_class, res_trees, append_cols=TRUE, fun='frac')
oth_fractions<-exactextractr::exact_extract(oth_class, oth_trees, append_cols=TRUE, fun='frac')

str_fractions$geom<-str_trees$geom
prk_fractions$geom<-prk_trees$geom
res_fractions$geom<-res_trees$geom
oth_fractions$geom<-oth_trees$geom

#Join the four data frames 
trees_fractions <- rbind(str_fractions, prk_fractions, res_fractions, oth_fractions)

# Specify the columns you want to consider for finding the maximum
#cols_to_max <- c("frac_1", "frac_2", "frac_3",  "frac_4", "frac_5", "frac_7", "frac_8", "frac_9", "frac_10",  "frac_11",  "frac_12",  "frac_13")  
cols_to_max <- c("frac_1", "frac_2",  "frac_4", "frac_6", "frac_7", "frac_10")



# Create a new column "max_value" containing the maximum value among selected columns
trees_fractions <- trees_fractions %>%
  mutate(max_value = pmax(!!!select(., all_of(cols_to_max))))

#-------------------------------------------------------------------------------------------------


#Calculate 
trees_fractions$entropy <- -rowSums(trees_fractions[, c("frac_1", "frac_2",  "frac_4", "frac_6", "frac_7", "frac_10")] * log(trees_fractions[, c("frac_1", "frac_2", "frac_4", "frac_6", "frac_7", "frac_10")]), na.rm = TRUE)
head(trees_fractions)

# Find the highest value in the "entropy" column
summary(trees_fractions)
max_entropy <- 1.7377

# Create the "index" column by dividing "entropy" by the highest value
trees_fractions$index <- 1-trees_fractions$entropy / max_entropy


# Export the joined data to a geopackage
st_write(trees_fractions, "D:/Paper_1/purity_index_official.gpkg")



#-------------------------------------------------------------------------------------------------

summary(trees_fractions)

#Extract the stats of trees per genus
counts_per_genus <- trees_fractions %>%
  group_by(genus) %>%
  summarise(count = n())

trees_fractions %>%
  group_by(genus) %>%
  summarise(min = min(index))

trees_fractions %>%
  group_by(genus) %>%
  summarise(mean = mean(index))

trees_fractions %>%
  group_by(genus) %>%
  summarise(median = median(index))

trees_fractions %>%
  group_by(genus) %>%
  summarise(mean = mean(max_value))

trees_fractions %>%
  group_by(genus) %>%
  summarise(median = median(max_value))




#-------------------------------

#Extract the stats of trees per genus
trees_fractions %>%
  group_by(LU_Cat_pat) %>%
  summarise(count = n())

trees_fractions %>%
  group_by(LU_Cat_pat) %>%
  summarise(min = min(index))

trees_fractions %>%
  group_by(LU_Cat_pat) %>%
  summarise(mean = mean(index))

trees_fractions %>%
  group_by(LU_Cat_pat) %>%
  summarise(median = median(index))
#Create an histogram of the distribution of the purity index



#------------------------------
bin_width <- 10

trees_fractions %>%
  +     mutate(elev = cut(height_90, breaks = seq(0, max(height_90) + bin_width, bin_width), include.lowest = TRUE)) %>%
  +     group_by(elev) %>%
  +     summarise(count = n())


trees_fractions %>%
  +     mutate(elev = cut(height_90, breaks = seq(min(height_90), max(height_90) + bin_width, bin_width), include.lowest = TRUE)) %>%
  +     group_by(elev) %>%
  +     summarise(mean = mean(index))







ggplot(subset(trees_fractions), aes(x = index, fill =after_stat(x))) +
  geom_histogram(binwidth = 0.05, boundary = 0, color = "#262626") +
  scale_fill_distiller(name="Purity index", palette="RdYlBu", trans="reverse")+
  theme_minimal() +
  theme(
    panel.background = element_rect(fill ="#262626", color = "gray"),
    axis.text = element_text(color = "gray"),
    axis.title = element_text(color = "gray"),
    plot.title = element_text(color = "gray"),
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_line(color = "gray"),
    plot.background = element_rect(fill = "#262626", colour = '#262626'),
    legend.text = element_text(colour="gray"),
    legend.title = element_text(colour = "gray")
  ) +
  xlim(0, 1) +
  ylim(0, 25000)+
  labs(title = "Frequency of purity index",
       x = "Index",
       y = "Frequency")



ggplot(subset(trees_fractions,LU_Cat_pat==1), aes(x = index, fill =after_stat(x))) +
  geom_histogram(binwidth = 0.05, boundary = 0, color = "#262626") +
  scale_fill_distiller(name="Purity index", palette="RdYlBu", trans="reverse")+
  theme_minimal() +
  theme(
    panel.background = element_rect(fill ="#262626", color = "gray"),
    axis.text = element_text(color = "gray"),
    axis.title = element_text(color = "gray"),
    plot.title = element_text(color = "gray"),
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_line(color = "gray"),
    plot.background = element_rect(fill = "#262626", colour = '#262626'),
    legend.text = element_text(colour="gray"),
    legend.title = element_text(colour = "gray")
  ) +
  xlim(0, 1) +
  ylim(0, 11000)+
  labs(title = "Frequency of purity index for trees in woodlands",
       x = "Index",
       y = "Frequency")

ggplot(subset(trees_fractions,LU_Cat_pat==2), aes(x = index, fill =after_stat(x))) +
  geom_histogram(binwidth = 0.05, boundary = 0, color = "#262626") +
  scale_fill_distiller(name="Purity index", palette="RdYlBu", trans="reverse")+
  theme_minimal() +
  theme(
    panel.background = element_rect(fill ="#262626", color = "gray"),
    axis.text = element_text(color = "gray"),
    axis.title = element_text(color = "gray"),
    plot.title = element_text(color = "gray"),
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_line(color = "gray"),
    plot.background = element_rect(fill = "#262626", colour = '#262626'),
    legend.text = element_text(colour="gray"),
    legend.title = element_text(colour = "gray")
  ) +
  xlim(0, 1) +
  ylim(0, 11000)+
  labs(title = "Frequency of purity index for trees in streets",
       x = "Index",
       y = "Frequency")

ggplot(subset(trees_fractions,LU_Cat_pat==3), aes(x = index, fill =after_stat(x))) +
  geom_histogram(binwidth = 0.05, boundary = 0, color = "#262626") +
  scale_fill_distiller(name="Purity index", palette="RdYlBu", trans="reverse")+
  theme_minimal() +
  theme(
    panel.background = element_rect(fill ="#262626", color = "gray"),
    axis.text = element_text(color = "gray"),
    axis.title = element_text(color = "gray"),
    plot.title = element_text(color = "gray"),
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_line(color = "gray"),
    plot.background = element_rect(fill = "#262626", colour = '#262626'),
    legend.text = element_text(colour="gray"),
    legend.title = element_text(colour = "gray")
  ) +
  xlim(0, 1) +
  ylim(0, 11000)+
  labs(title = "Frequency of purity index for trees in residential areas",
       x = "Index",
       y = "Frequency")

ggplot(subset(trees_fractions,LU_Cat_pat==4), aes(x = index, fill =after_stat(x))) +
  geom_histogram(binwidth = 0.05, boundary = 0, color = "#262626") +
  scale_fill_distiller(name="Purity index", palette="RdYlBu", trans="reverse")+
  theme_minimal() +
  theme(
    panel.background = element_rect(fill ="#262626", color = "gray"),
    axis.text = element_text(color = "gray"),
    axis.title = element_text(color = "gray"),
    plot.title = element_text(color = "gray"),
    panel.grid.major = element_line(color = "gray"),
    panel.grid.minor = element_line(color = "gray"),
    plot.background = element_rect(fill = "#262626", colour = '#262626'),
    legend.text = element_text(colour="gray"),
    legend.title = element_text(colour = "gray")
  ) +
  xlim(0, 1) +
  ylim(0, 11000)+
  labs(title = "Frequency of purity index for trees in other areas",
       x = "Index",
       y = "Frequency")





