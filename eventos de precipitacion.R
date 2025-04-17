library(tidyverse)
library(extRemes)
library(ggplot2)


load("data/output/datos_rf.rda")

datosprueba <- datos %>%
  mutate(time = as.POSIXct(time))

# Crea un nuevo conjunto de datos solo con la información de precipitación
datos_precip <- datosprueba %>%
  select(time, starts_with("prec"))

# Agrega una nueva columna "sum_precip" con la suma de precipitaciones por intervalo de tiempo
datos_precip$sum_precip <- rowSums(select(datos_precip, starts_with("prec")), 
                                   na.rm = TRUE)

# Muestra los primeros elementos del conjunto de datos actualizado
head(datos_precip)

datos_precip <- datos_precip %>%
  select(-starts_with("prec"))


# Convertir los datos a un formato aceptable para drawre
datos_drawre <- data.frame(
  time = rep(datos_precip$time, each = ncol(datos_precip) - 1),
  value = c(as.matrix(datos_precip[, -1]))
)

# Usar drawre con los nuevos datos
Prec_Events <- drawre(datos_drawre, IETD = 3, Thres = 30)
  
  all_events1 <- Prec_Events$Rainfall_Events
  all_events1 <- lapply(all_events1, function(event) {
    event$time <- as.POSIXct(event$time)
    return(event)
  })
  
  # Función para realizar análisis de regresión y generar gráfico
  analyze_event <- function(event) {
    if (any(!is.na(event$prec))) {
      max_prec <- max(event$prec, na.rm = TRUE)  # Obtener la precipitación máxima
      max_prec_time <- event$time[which.max(event$prec)]  # Obtener el tiempo de la precipitación máxima
      
      # Obtener datos para la regresión lineal
#  ##    regression_data <- subset(event, time <= max_prec_time)
 ##     regression_line <- lm(prec ~ time, data = regression_data)
      
      ggplot(event, aes(x = time, y = prec)) +
        geom_line(color = "blue") +
        geom_point(aes(x = max_prec_time, y = max_prec), color = "red", size = 2)+
        geom_ribbon(data = regression_data, aes(x = time, ymin = prec, ymax = predict(regression_line)), fill = "lightblue", alpha = 0.5) +
        scale_x_datetime(labels = scales::date_format("%Y-%m-%d %H")) +
        ylab("Precipitación [mm]") +
        ggtitle("Precipitación a lo largo del tiempo durante el evento")
    } else {
      # Si no hay datos de precipitación, devuelve un gráfico vacío
      ggplot() +
        ggtitle("Evento sin datos de precipitación")
    }
  }
  
  # Aplicar la función a cada evento y mostrar los gráficos en una cuadrícula
  plots_list <- lapply(all_events1, analyze_event)
  grid.arrange(grobs = plots_list, ncol = 3)

  