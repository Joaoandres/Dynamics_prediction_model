library(stars)
library(patchwork)
library(tidyverse)

area <- st_read("data/shp/TENA.shp")
data <-  stars::read_ncdf("output/P4x4.nc",var = "prec")
caudal <- read_csv("data/csv/H001_JUNIO2023.csv",
                   na=c("7999", "NAN", "NA"), 
                   locale = locale(tz = "America/Guayaquil"))
data <- data[st_transform(area, crs = st_crs(data))]

datos_df <- as.data.frame(data) |> 
  na.omit() |> 
  arrange(longitude,latitude,time) %>% 
  group_by(time) %>% 
  mutate(cell = sprintf("px%02d", 1:n()), 
         prec = units::drop_units(prec)) %>% 
  left_join(select(caudal, time = fecha, 
                   nivel = level_Avg, 
                   descarga = water_discharge_Avg)) %>%
  filter(time < "2023-04-01 00:00")


#Detectar vacios
ggplot(filter(datos_df, cell == "px01")) +
  aes(time, 1, fill = !is.na(prec)) +
  geom_tile() +
  geom_tile(aes(y = 2, fill = !is.na(descarga)))

save(datos_df, file = "data/output/datos_df.rda")
caudal <- read_csv("data/csv/Data_2005-2014.csv")
