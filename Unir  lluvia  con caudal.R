load("C:/Users/adolf/Documents/joao/R/Caudal_Events.RData")##datos de eventos de cuadal
load("C:/Users/adolf/Documents/joao/R/eventos.RData")## datos de eventos de precipitacion


unir_eventos <- function(caudal_evento, eventos, intervalo_horas = 2) {
  primer_valor_caudal <- caudal_evento$valor[1]
  intervalo_tiempo <- lubridate::hours(intervalo_horas)
  
  # Filtrar eventos de precipitación que están dentro del intervalo de tiempo
  eventos_preci_filtro <- eventos[eventos$time >= caudal_evento$time &
                                    eventos$time <= (caudal_evento$time - intervalo_tiempo), ]
  
  # Crear un dataframe con eventos de caudal y sus eventos de precipitación relacionados
   
  
  return(data.frame(evento_caudal = caudal_evento$valor,
    time_caudal = caudal_evento$time,
    evento_precipitacion = eventos_preci_filtro$valor,
    time_precipitacion = eventos_preci_filtro$time
  ))
}
resultados <- lapply(1:nrow(Caudal_Events), function(i) {
  unir_eventos(Caudal_Events[i, , drop = FALSE], eventos)
})

# Combina los resultados en un solo dataframe
resultados_df <- do.call(rbind, resultados)









############################################
#Unir eventos de lluvia a eventos de caudal
qevents
eventos

qev <- qevents[1,]

unir_eventos <- function(qev, rainevents){
  feven <- rainevents[qev$Starting > rainevents$start,]
  lasteven <- feven[nrow(feven),]
  bind_cols(qev, 
            rename_all(lasteven, ~paste0("rain.", .x)))
}

datosRF2 <- qevents %>% 
  group_by(Number.Event) %>% 
  group_modify(~unir_eventos(.x, rainevents = eventos)) %>% 
  group_by(rain.event) %>% 
  slice_tail() %>% 
  ungroup() %>% 
  mutate(semaforo = Qr.peak > 300)
save(datosRF2, file="C:/Users/adolf/Documents/joao/R/datosRF2.RData")  
load("C:/Users/adolf/Documents/joao/R/datosRF2.RData")
