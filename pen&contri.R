library(stars)
library(tidyverse)
terrain_files <- c("dems/dem_correc.tif", 
                   "dems/dem_contribu.tif",
                   "dems/pendiente.tif")

data_dtena <-read_stars(terrain_files, along = "band")
####
data_dtena <- st_set_dimensions(data_dtena, "band", 
                                values = c("dem", "contribucion", "pendiente")) 
names(data_dtena) <- "terreno"
data_dtena <- st_warp(data_dtena, crs = st_crs(temp))

medias <- cbind(st_drop_geometry(temp),
                    data_dtena  %>% 
                    aggregate(by = temp, FUN = mean, na.rm = TRUE) %>% 
                    st_as_sf() %>%
                    st_drop_geometry()) %>% 
  rename_with(.fn = ~paste0(.x,"_avg"), .cols = -cell)

maximas <- cbind(st_drop_geometry(temp), 
                   data_dtena %>% 
                   aggregate(by = temp, FUN = max, na.rm = TRUE) %>% 
                   st_as_sf() %>% 
                   st_drop_geometry()) %>% 
  rename_with(.fn = ~paste0(.x,"_max"), .cols = -cell)


datos_dems<- left_join(medias, maximas, by = "cell") %>% 
  mutate(cell = sprintf("px%02d", cell))

save(datos_dems,file="data/output/datos_dems.rda")

