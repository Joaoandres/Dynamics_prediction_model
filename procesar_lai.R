library(stars)
library(tidyverse)

## Cargar la capa base tif 
temp <- st_read("data/gpkg/template.gpkg", "template") 
st_crs(temp) <- 4326

##Generar un lista donde se contenga los tiff LAI a lo largo del tiempo y pasarlos a star
ficheros1 <- list.files("LAI", pattern = ".tif$", full.names = TRUE)
data_lai<- read_stars(ficheros1, along = "time")

##Generar una lista  que contenga las fechas que se encuentran en los nombres de los tiff
fechas <- str_remove_all(ficheros1, ".*doy|_ai.*") %>% 
           as.Date(format = "%Y%j")

##dar valores a las dimensiones y unir la varible fechas al  star
data_lai <- st_set_dimensions(data_lai, "time", values = fechas) 
names(data_lai) <- "LAI"
#plot(data_lai[,,,1])
##

## extraer los datos de estadisticas zonales

lai_mean <- cbind(st_drop_geometry(temp), 
      data_lai %>% 
        aggregate(by = temp, FUN = mean, na.rm = TRUE) %>% 
        st_as_sf() %>% 
        st_drop_geometry()) %>% 
  pivot_longer(-cell, names_to = "time", values_to = "LAIm")


lai_max <- cbind(st_drop_geometry(temp), 
                  data_lai %>% 
                    aggregate(by = temp, FUN = max, na.rm = TRUE) %>% 
                    st_as_sf() %>% 
                    st_drop_geometry()) %>%
  pivot_longer(-cell, names_to = "time", values_to = "LAIx")


datos_lai <- left_join(lai_mean, lai_max, by = c("cell", "time")) %>% 
  mutate(cell = sprintf("px%02d", cell), 
         time = as.POSIXct(time, tz = "UTC"))

save(datos_lai, file = "data/output/datos_laidf.rda")  
