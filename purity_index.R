
library(sf)


data_trees<-st_read("D:/Tree_data_test/Official_test1/input_points.gpkg")


head(data_trees)

data_trees$total<- data_trees$HISTO_1 %>% + data_trees$HISTO_2 %>% + data_trees$HISTO_3 %>% + data_trees$HISTO_4 %>% + data_trees$HISTO_5 %>% + data_trees$HISTO_7 %>% + data_trees$HISTO_8 %>% + data_trees$HISTO_9 %>% + data_trees$HISTO_10 %>% + data_trees$HISTO_11 %>% + data_trees$HISTO_12 %>% + data_trees$HISTO_13 %>% + data_trees$HISTO_NODATA

data_trees$per1<- data_trees$HISTO_1/data_trees$total
data_trees$per1<- data_trees$HISTO_1/data_trees$total
data_trees$per1<- data_trees$HISTO_1/data_trees$total
data_trees$per1<- data_trees$HISTO_1/data_trees$total
