# Load necessary libraries
library(sf)
library(dplyr)

# Load your points and polygons data
# Replace 'points.shp' and 'polygons.shp' with your actual shapefile paths or other data sources
points <- st_read("C://Users//ang58gl//Documents/MEGAsync//PhD//Data Processing//trees_aoi.shp")
polygons <- st_read("C://Users//ang58gl//Documents//MEGAsync//PhD//Data Processing//Blocks_Munich_aoi.shp")

# Ensure both layers have the same CRS
points <- st_transform(points, st_crs(polygons))

# Add a unique ID to each polygon
polygons <- polygons %>%
  mutate(polygon_id = row_number())

# Perform spatial join to get the polygon each point falls into
points_joined <- as.data.frame(st_join(points, polygons, join = st_nearest_feature))

# Summarize the biomass by polygon ID
biomass_summary <- points_joined %>%
  group_by(polygon_id) %>%
  summarize(total_biomass = sum(Biomasse.., na.rm = TRUE))

# Summarize the Biomassezuwachs by polygon ID
bio_growth_summary <- points_joined %>%
  group_by(polygon_id) %>%
  summarize(Biomassezuwachs = sum(Biomassezu, na.rm = TRUE))

# Summarize the jährliche Wachstumsrate by polygon ID
growth_summary <- points_joined %>%
  group_by(polygon_id) %>%
  summarize(total_growth = round(mean(jaehrliche, na.rm = TRUE),1))

# Summarize the co2 fixierung by polygon ID
co2fix_summary <- points_joined %>%
  group_by(polygon_id) %>%
  summarize(total_co2 = sum(CO2.Fixier, na.rm = TRUE))

# Summarize the wasserverbrauch by polygon ID
water_summary <- points_joined %>%
  group_by(polygon_id) %>%
  summarize(total_water = sum(Wasserverb, na.rm = TRUE))

# Summarize the Abfluss unter dem Baum by polygon ID
drainage_summary <- points_joined %>%
  group_by(polygon_id) %>%
  summarize(total_drainage = round(mean(Abfluss.un, na.rm = TRUE),0))

# Summarize the Kühlung durch Transpiration (Sommer) by polygon ID
cooling_transp_summary <- points_joined %>%
  group_by(polygon_id) %>%
  summarize(total_transp = sum(Kuehlung.d, na.rm = TRUE))

# Summarize the Kühlung durch Beschattung (Sommer) by polygon ID
cooling_shade_summary <- points_joined %>%
  group_by(polygon_id) %>%
  summarize(total_shade = sum(Kuehlung_1, na.rm = TRUE))

# Summarize the Zuwachs Kronenprojektionsfläche by polygon ID
crown_proj_summary <- points_joined %>%
  group_by(polygon_id) %>%
  summarize(total_crown = sum(Zuwachs.Kr, na.rm = TRUE))


# Merge the biomass summary back to the polygon layer
polygons_with_es <- polygons %>%
  left_join(as.data.frame(biomass_summary), by = "polygon_id")%>%
  left_join(as.data.frame(bio_growth_summary), by = "polygon_id")%>%
  left_join(as.data.frame(growth_summary), by = "polygon_id")%>%
  left_join(as.data.frame(co2fix_summary), by = "polygon_id")%>%
  left_join(as.data.frame(water_summary), by = "polygon_id")%>%
  left_join(as.data.frame(drainage_summary), by = "polygon_id")%>%
  left_join(as.data.frame(cooling_transp_summary), by = "polygon_id")%>%
  left_join(as.data.frame(cooling_shade_summary), by = "polygon_id")%>%
  left_join(as.data.frame(crown_proj_summary), by = "polygon_id")

polygons_with_es <- polygons_with_es %>%
  mutate(area = as.numeric(st_area(.)))

polygons_with_es <- polygons_with_es %>%
  mutate(volume = area*25*1.2)

polygons_with_es <- polygons_with_es %>%
  mutate(energy = ((total_transp/2.77778E-07)/1000)/(92*15.4))

polygons_with_es <- polygons_with_es %>%
  mutate(temp = energy/volume/1.03)

head(polygons_with_es)

names(polygons_with_es)<- c("BL_NUMMER", "FLAECHE_QM", "BL_NR_2011", "ET_ID", "id",  "area", "polygon_id", "Biomasse (kg C)", "Biomassezuwachs (kg/Jahr)", "jaehrliche Wachstumsrate (%)", "CO2-Fixierung (kg CO2/Jahr)", 
                            "Wasserverbrauch (m³ /Jahr)","Abfluss unter dem Baum (mm/Jahr)", "Kuehlung durch Transpiration-Sommer (kWh)", "Kuehlung durch Beschattung-Sommer (kWh)", "Zuwachs Kronenprojektionsflaeche (m2/Jahr)", "geometry", "volume","energy","Temperaturabnahme (°C)" )

# Save the result to a GeoJSON file
st_write(polygons_with_es, "C:/Users/ang58gl/Documents/MEGAsync/PhD/Presentations/WochederUmwelt/Kepler/polygons_with_es_v3.geojson", driver = "GeoJSON")
