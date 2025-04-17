library(ncdf4)
library(terra)
library(readr)
library(dplyr)  
library(stars)
library(tidyverse)
library(dplyr)
library(caret)
#install.packages("caret")

#install.packages("ncmeta")
# Funcion para interpolar un paso de tiempo
interpolacion_prec <- function(prec, 
                               puntos = pluvios, 
                               base = base){
  
  val <- as.matrix(prec[1, -1]) |>  as.vector()  |> unique()
  if(length(val) == 1) {
    prd <- base * val
    names(prd) <- "rain"
    return(prd[[1]])
  }
  
  data <- merge(
    x = puntos, 
    y = prec |> 
      pivot_longer(cols = -datetime, 
                   names_to = "Codigo", 
                   values_to = "prec"),
    by = "Codigo")
  
  mdl <- gstat(formula = prec ~ 1, data = data, 
               locations = ~x+y, 
               set = list(idp = 2)) 
  prd <- interpolate(base, mdl, debug.level = 0)[[1]]
  names(prd) <- "rain"
  return(prd * base)
}

nc_to_stars <- function(fichero){
  nc <- ncdf4::nc_open(fichero)
  lat <- ncdf4::ncvar_get(nc, "lat")
  lon <- ncdf4::ncvar_get(nc, "lon")
  time <- ncdf4::ncvar_get(nc, "time")
  origin <- as.POSIXct("1970-01-01 00:00:00 UTC", tz = "UTC")
  data <- ncdf4::ncvar_get(nc, "precipitationCal")
  nc_close(nc)
  r <- terra::rast(nrows = length(lat), ncols = length(lon),
                   xmin = min(lon), xmax = max(lon), 
                   ymin = min(lat), ymax = max(lat))
  
  r[] <- as.vector(t(data))
  r <- flip(r)
  terra::time(r) = as.POSIXct(as.vector(time), origin = origin, tz = "UTC")
  names(r) <- "precipitation"
  crs(r) <- "EPSG:4326"
  r
}

init_nc <- function(.x,
                    times = prec$datetime,
                    filename = "data/outputs/P.nc",
                    varname = "prec",
                    longname = "Precipitation (mm)",
                    units = "mm",
                    zname = "time",
                    missval = -1.175494e38,
                    precision = "float",
                    compression = 9,
                    overwrite = FALSE){
  
  if(overwrite && file.exists(filename)) file.remove(filename)
  
  nc <- ncol(.x)
  nr <- nrow(.x)
  nl <- length(times)
  prj <- gsub("\n", "", crs(.x))
  
  xname = "longitude"
  yname = "latitude"
  xunit = "degrees_east"
  yunit = "degrees_north"
  zunit <- paste("seconds since", format(lubridate::origin, "%Y-%m-%d %H:%M:%S"))
  cal <- "standard"
  
  xdim <- ncdf4::ncdim_def( xname, xunit, xFromCol(.x, 1:nc) )
  ydim <- ncdf4::ncdim_def( yname, yunit, yFromRow(.x, 1:nr) )
  zdim <- ncdf4::ncdim_def(zname, zunit, as.numeric(times), 
                           unlim=FALSE, create_dimvar=TRUE, calendar=cal)
  
  ncvar <- list()
  ncvar[[1]] <- ncdf4::ncvar_def(name = varname, units = units, 
                                 dim = list(xdim, ydim, zdim), missval = missval,
                                 longname = longname, prec = precision, 
                                 compression = compression)
  ncvar[[2]] <- ncdf4::ncvar_def("crs", "", list(), NULL, prec="integer")
  
  ncobj <- ncdf4::nc_create(filename, ncvar, force_v4=TRUE, verbose=FALSE)
  on.exit(ncdf4::nc_close(ncobj))
  
  
  if (prj != "") {
    ncdf4::ncatt_put(ncobj, ncvar[[2]], "crs_wkt", prj, prec="text")
    # need for older gdal?
    ncdf4::ncatt_put(ncobj, ncvar[[2]], "spatial_ref", prj, prec="text")
    ncdf4::ncatt_put(ncobj, ncvar[[2]], "proj4", terra:::.proj4(.x), prec='text')
    ncdf4::ncatt_put(ncobj, ncvar[[2]], "grid_mapping", "crs", prec="text")
  }
  e <- ext(.x)
  rs <- res(.x)
  gt <- paste(trimws(formatC(as.vector(c(e$xmin, rs[1], 0, e$ymax, 0, -1 * rs[2])), 22)), collapse=" ")
  ncdf4::ncatt_put(ncobj, ncvar[[2]], "GeoTransform", gt, prec="text")
  ncdf4::ncatt_put(ncobj, 0, "Conventions", "CF-1.4", prec="text")
  ncdf4::ncatt_put(ncobj, 0, "date", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), prec="text")
}

update_nc <- function(con, x, i = 1, varname = "prec", missval = -1.175494e38){
  d <- readValues(x)
  #d[is.na(d)] <- -1.175494e38
  ncdf4::ncvar_put(con, varname, d, start=c(1,1,i), count=c(-1,-1,1))
}

prog_bar <- function(x){
  progress::progress_bar$new(format = "(:spin) [:bar] :percent [Elapsed time: :elapsedfull || Estimated time remaining: :eta]",
                             total = x,
                             complete = "=",   # Completion bar character
                             incomplete = " ", # Incomplete bar character
                             current = ">",    # Current bar character
                             clear = FALSE,    # If TRUE, clears the bar when finish
                             width = 100)
}

