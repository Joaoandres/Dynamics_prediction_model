library(stars)
library(patchwork)
library(tidyverse)
source("R/99_funciones.R")
theme_set(theme_classic(base_family = "times", base_size = 10))
load("data/output/datos_rf.rda")

tst <- datos %>%
  group_by(longitude, latitude) %>% 
  group_modify(.f = ~tiempo_retardo(.x$prec, .x$descarga, lag.max = 180), 
               .keep = TRUE)

pcp <- datos %>% 
  group_by(longitude, latitude, time = floor_date(time, "year")) %>%
  summarise(prec = sum(prec)) %>% 
  group_by(longitude, latitude) %>%
  summarise(prec = mean(prec)) %>% 
  ggplot(aes(longitude, latitude)) +
  geom_raster(aes(fill = prec)) +
  geom_point(data = tibble(latitude = -0.990775, longitude = -77.814390), 
             color = "red") +
  scale_fill_viridis_c(direction = -1) +
  coord_equal() +
  labs(fill = "Lluvia [mm]", x = "Longitud", y = "latitud")

pv <- tst %>% ggplot(aes(longitude, latitude)) +
  geom_raster(aes(fill = P_VALUE < 0.05)) +
  geom_point(data = tibble(latitude = -0.990775, longitude = -77.814390), 
             color = "red") +
  coord_equal() +
  labs(fill = "pVal < 0.05", x = "Longitud", y = "latitud")

acf <- tst %>% ggplot(aes(longitude, latitude)) +
  geom_raster(aes(fill = ACF)) +
  geom_point(data = tibble(latitude = -0.990775, longitude = -77.814390), 
             color = "red") +
  coord_equal() +
  labs(x = "Longitud", y = "latitud", fill="CCF")
lag <- tst %>% ggplot(aes(longitude, latitude)) +
  geom_raster(aes(fill = LAG/2)) +
  geom_point(data = tibble(latitude = -0.990775, longitude = -77.814390), 
             color = "red") +
  coord_equal() +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(9, "YlGnBu")) +
  labs(fill = "LAG", x = "Longitud", y = "latitud")

pcp + acf + lag  + pv + 
  plot_layout(ncol = 2, guides = "collect") &
  plot_annotation(tag_levels = "a", tag_suffix = ")")

ggsave("output/png/resultados_ccf.png", 
       width = 14, height = 12, 
       units = "cm", dpi = 300, 
       scale = 1.5)
