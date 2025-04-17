library(stars)
library(ggplot2)
library(ncdf4)
library(terra)
source("R/99_funciones.R")
fast_strptime()
parse_datetime(format = "%Y/%m/%d")


ficheros1 <- list.files("LAI", pattern = ".tif$", full.names = TRUE)
rs<-rast(ficheros1)

df<-as.data.frame(rs,xy=TRUE) %>% 
  pivot_longer(cols = -c(x,y), names_to = "fecha") %>% 
  mutate(fecha = str_remove_all(fecha, "MCD15A2H.061_Lai_500m_doy|_aid0001") %>% 
           as.Date(format = "%Y%j"))

plot(df)

  
separate(b,c("anio","dia"))
  mutate(name=as.date())

fechas <- gsub("LAI/MCD15A2H.061_Lai_500m_doy.|.tif$",'', ficheros1) %>% 
  parse_datetime(format = "%Y/%m/%d")|> 
  lubridate::with_tz("America/Guayaquil")

mask <- vect("data/shp/TENA.shp") |> project("EPSG:4326")
base <- nc_to_stars(ficheros1[1], 
                    var = "Var_LAI",
                    resample = FALSE) |> 
  crop(mask) |> 
  mask(mask)

filename = "LAI/"
init_nc(base,
        times = fechas,
        filename = filename,
        varname = "LAI",
        longname = "Leaf area index",
        zname = "time",
        missval = -9,
        precision = "float",
        compression = 9,
        overwrite = TRUE)

pb <- prog_bar(length(fechas))
con <- nc_open(filename, write = TRUE)



for (i in seq_along(ficheros1)){
  prd <- nc_to_stars(ficheros[i], resample = TRUE, 
                     res_factor = c(4, 4)) |> 
    crop(mask) |> 
    mask(mask)
  update_nc(con, x = prd, i = i, missval = -9)
  pb$tick()
}
nc_close(con)








id <- 1

ggplot() +
  geom_stars(data = data[,,,id:(id+24)], na.action = na.omit) +
  geom_sf(data = st_geometry(st_as_sf(mask)), color = "orange", fill = NA) +
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11, "Spectral")), 
                       na.value = NA) +
  coord_sf() +
  facet_wrap(~time) +
  theme_void()