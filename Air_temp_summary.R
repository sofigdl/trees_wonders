pacman::p_load(tidyverse, lubridate, sf, dplyr)

#----------------------------- Dataset preparation -----------------------------

coords<-read.csv("D:/3-Paper/Arzberger/MUC_plot_info.csv")
coords<-coords[c("plot_id", "utm_E_logger", "utm_N_logger", "utm_E_center", "utm_N_center", "distance_to_center")]

air<-read.csv("D:/3-Paper/Arzberger/timeseries_total_2023.csv")
air<-air[ ,!(names(air) %in% c("TS_UTC"))]

air <- air %>% mutate(
  date=as.Date(TS_local),
  hour=hour(TS_local)
)

air<-air %>% 
  left_join(coords, by ="plot_id")

#------------------------------- Quality control -------------------------------

n_distinct(air$plot_id)
range(air$TS_local)
summary(air$T_air)

#Check missing values
air %>%
  summarise(
    n=n(),
    na_T = sum(is.na(T_air))
  )

#Check outliers
air %>%
  filter(T_air < -20 | T_air > 50)


#Check temporal completeness
air %>%
  count(plot_id) %>%
  summary()

#----------------------------- Visual exploration ------------------------------
#Time series
air %>%
  filter(plot_id %in% c("GF01", "GF05", "GF10")) %>%
  ggplot(aes(date, T_air, color = plot_id)) +
  geom_line(alpha = 0.7) +
  labs(x = "Date", y = "Air temperature (°C)")

#mean diurnal cycle
air %>%
  group_by(hour) %>%
  summarise(T_mean = mean(T_air, na.rm = TRUE)) %>%
  ggplot(aes(hour, T_mean)) +
  geom_line() +
  labs(x = "Hour of day", y = "Mean air temperature (°C)")

#seasonal cycle
air %>%
  mutate(month = month(TS_local, label = TRUE)) %>%
  group_by(month) %>%
  summarise(T_mean = mean(T_air, na.rm = TRUE)) %>%
  ggplot(aes(month, T_mean, group = 1)) +
  geom_line() +
  geom_point()

#----------------------------- Spatial exploration -----------------------------

air_sf <- st_as_sf(
  air,
  coords = c("utm_N_logger", "utm_E_logger"),
  crs = 32632
)

air_plot_mean <- air_sf %>%
  group_by(plot_id) %>%
  summarise(T_mean = mean(T_air, na.rm = TRUE))

ggplot(air_plot_mean) +
  geom_sf(aes(color = T_mean), size = 3) +
  scale_color_viridis_c()


#------------------------------- Derived metrics -------------------------------

air_daily <- air %>%
  group_by(plot_id, date) %>%
  summarise(
    T_mean = mean(T_air, na.rm = TRUE),
    T_max  = max(T_air, na.rm = TRUE),
    T_min  = min(T_air, na.rm = TRUE),
    DTR    = T_max - T_min
  )



air_summer <- air %>%
  filter(month(date) >= 3 & month(date) <= 9)

table(month(air_summer$TS_local))

#-------------------------------------------------------------------------------
# SUMMER TEMPERATURE

air_summer <- air %>%
  filter(month(TS_local) >= 3 & month(TS_local)<= 9)

table(month(air_summer$TS_local))

#DAYTIME TEMPERATURE

air_summer_day <- air_summer %>%
  filter(hour(TS_local) >= 8 & hour(TS_local) <= 18)
