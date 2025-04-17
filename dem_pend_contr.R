library(sf)
library(tidyverse)
library(lubridate)



dem_contr9 <- st_read("data/csv/data_stacontribui.gpkg",layer="estadsticas")%>% 
  rename(cell = zone) %>% 
  mutate(cell = sprintf("px%02d", cell)) %>% 
  arrange(cell) %>% 
  select(cell,contrimax= max, contrimean= mean) %>%
#

data_contri <- st_set_dimensions(dem_contr9, "cell", values = fechas) 
names(data_contri) <- "contri"
plot(data_contri)

data_contri <- st_warp(dem_contr9, temp, method = "average", use_gdal = TRUE, no_data_value = -9999)
data_contri <- st_set_dimensions(data_contri, "band", values = fechas) 
names(data_contri) <- "contri"

temp_pol <- st_as_sf(temp) 
###

dem_pend <- st_read("C:/Users/adolf/Documents/joao/data/csv/data_stacontribui.gpkg", layer = "pendiente") %>% 
  rename(cell = zone) %>% 
  mutate(cell = sprintf("px%02d", cell)) %>% 
  select(cell,penmax= max, penmean= mean)

save(dem_contr9, dem_pend,file = "data/output/datos_dem.rda")
