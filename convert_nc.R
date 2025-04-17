library(stars)
library(ggplot2)
library(ncdf4)
library(terra)
source("R/99_funciones.R")

ficheros <- list.files("data/nc", pattern = "nc4$", full.names = TRUE)
fechas <- gsub("data/nc/3B.HHR.L.MS.MRG.3IMERG.|.E[0-9]+.[0-9]+.V06B.HDF5.nc4",
               '', ficheros) |> 
  gsub("\\_", '-', x = _) |> 
  as.POSIXct( tz = "UTC", format = "%Y%m%d-S%H%M%S") |> 
  lubridate::with_tz("America/Guayaquil")

mask <- vect("data/shp/TENA.shp") |> project("EPSG:4326")
base <- nc_to_stars(ficheros[1], 
                    resample = TRUE, 
                    res_factor = c(4, 4)) |> 
  crop(mask) |> 
  mask(mask)

filename = "data/output/P4x4.nc"
init_nc(base,
        times = fechas,
        filename = filename,
        varname = "prec",
        longname = "Precipitation (mm)",
        units = "mm",
        zname = "time",
        missval = -9,
        precision = "float",
        compression = 9,
        overwrite = TRUE)

pb <- prog_bar(length(fechas))
con <- nc_open(filename, write = TRUE)



for (i in seq_along(ficheros)){
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

