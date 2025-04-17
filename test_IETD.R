library(tidyverse)
source("R/ietd_rain_events.R")
load("data/output/datos_df.rda")

eventos <- datos_df |> 
  # group_by(longitude, latitude, cell) |> 
  group_by(time) %>% 
  summarise(prec = sum(prec, na.rm = TRUE)) %>% 
  #group_modify(\(x, ...) ietd_re(x)[[1]]) |> 
  #ungroup()
  ietd_re(thres = 10)

eventos <- eventos$Rainfall_Characteristics

evs <- 1:20
datos_df %>% 
  filter(cell == "px01") %>% 
  ggplot() +
  geom_rect(mapping = aes(xmin = start, xmax = end,
                          ymin = 0, ymax = Inf,
                          fill = "Evento"),
            data = filter(eventos, cell == "px01"), 
            color = NA, alpha = 0.5
  ) +
  geom_segment(aes(x = time, xend = time, 
                   y = 0, yend = prec), 
               color = "steelblue") +
  scale_fill_manual(values = c("Evento" = "grey70")) +
  #coord_cartesian(xlim = c(eventos[[1]][evs,]$start[1], eventos[[1]][evs,]$end[max(evs)])) +
  #coord_cartesian(xlim = as.POSIXct(c("2022-05-08", "2022-05-16"))) +
  theme_light()


count(eventos, longitude, latitude,  cell) |> 
  ggplot() +
  aes(longitude, latitude, fill = n) +
  geom_raster() +
  coord_equal()


eventos <- eventos %>%
  mutate(Number.Event = row_number())


save(eventos, file = "C:/Users/adolf/Documents/joao/R/eventos.RData")
