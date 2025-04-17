library(stars)
library(patchwork)
library(tidyverse)
source("R/99_funciones.R")

load("data/output/datos_df.rda")

tst <- datos_df %>%
  group_by(longitude, latitude) %>% 
  group_modify(.f = ~tiempo_retardo(.x$prec, .x$descarga, lag.max = 180), 
               .keep = TRUE)


 ggplot() +
   geom_stars(data = data[,,,1:24], na.action = na.omit) +
   geom_sf(data = st_geometry(st_as_sf(mask)), color = "orange", fill = NA) +
   scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "Spectral")), 
                        na.value = NA) +
   coord_sf() +
   facet_wrap(~time) +
   theme_void()

pcp <- datos_df %>% 
  group_by(longitude, latitude, time = floor_date(time, "year")) %>%
  summarise(prec = sum(prec)) %>% 
  group_by(longitude, latitude) %>%
  summarise(prec = mean(prec)) %>% 
  ggplot(aes(longitude, latitude)) +
  geom_raster(aes(fill = prec)) +
  geom_point(data = tibble(latitude = -0.990775, longitude = -77.814390), 
             color = "red") +
  scale_fill_viridis_c(direction = -1) +
  coord_equal()

pv <- tst %>% ggplot(aes(longitude, latitude)) +
  geom_raster(aes(fill = P_VALUE < 0.05)) +
  geom_point(data = tibble(latitude = -0.990775, longitude = -77.814390), 
             color = "red") +
  coord_equal()

acf <- tst %>% ggplot(aes(longitude, latitude)) +
  geom_raster(aes(fill = ACF)) +
  geom_point(data = tibble(latitude = -0.990775, longitude = -77.814390), 
             color = "red") +
  coord_equal()
lag <- tst %>% ggplot(aes(longitude, latitude)) +
  geom_raster(aes(fill = LAG)) +
  geom_point(data = tibble(latitude = -0.990775, longitude = -77.814390), 
             color = "red") +
  coord_equal() +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "YlGnBu"))

pcp + lag +  acf + pv + 
  plot_layout(ncol = 2, guides = "collect")
