  library(caret)
  unir_eventos <- function(eventos, Caudal_Events, intervalo_horas = 2) {
    # Obtener el primer valor de caudal del evento de caudal  
    primer_valor_caudal <- Caudal_Events$valor[1]
    
    # Definir el intervalo de tiempo a considerar (2 horas)
    intervalo_tiempo <- lubridate::hours(intervalo_horas)
    
    # Filtrar eventos de caudal que están dentro del intervalo de tiempo
    eventos_caudal_filtrados <- Caudal_Events[Caudal_Events$time >= eventos$time & Caudal_Events$time <= (eventos$time + intervalo_tiempo), ]
    
    # Asignar el primer valor de caudal al evento de lluvia
    eventos$primer_valor_caudal <- primer_valor_caudal
    
    # Agregar los eventos de caudal dentro del intervalo de tiempo al evento de lluvia
    eventos$Caudal_Events <- eventos_caudal_filtrados
    
    return(data.frame(eventos))
  }








cl <- makePSOCKcluster(10)
registerDoParallel(cl)


set.seed(s)
samp<- sample(nrow(merged_data),0.7*nrow(merged_data))
train<- merged_data[samp,]
test<- merged_data[-samp,]



# Ajustar el modelo utilizando merged_data
Pmodel <- train(time ~., 
                data = merged_data, 
                method = "rf", 
                importance = TRUE,
                na.action = na.omit,
                trControl = trainControl(method = "cv", number = 5, allowParallel = TRUE))
# Detener el clúster paralelo
stopCluster(cl)

# Imprimir información sobre el modelo ajustado
print(Pmodel)

# Hacer predicciones en nuevos datos (por ejemplo, en el mismo conjunto de datos para demostración)
predictions <- predict(Pmodel, newdata = merged_data)

# Imprimir las predicciones
print(predictions)
 