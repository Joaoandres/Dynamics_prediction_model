library(tidyverse)
  library(rpart)
  library(pROC)
  load("C:/Users/adolf/Documents/joao/R/datosRF2.RData")
  # Crear el modelo de árbol de clasificación con la variable 'formula'
  set.seed(567)
  train <- group_by(datosRF2, semaforo) %>% 
    slice_sample(prop = 0.7)
  test <- anti_join(datosRF2, train, by = "Number.Event")
  
  formula <- semaforo ~ Duration + Volume + Intensity + ind.peak + Qr.peak + duration.peak +
    growth.slope + antecedent.flow + rain.volume + rain.intensity
  
  modelo_arbol <- rpart(formula, data = train, method = "anova")
  plot(modelo_arbol, uniform = TRUE, margin = 0.1)
  text(modelo_arbol, use.n = TRUE, all = TRUE, cex = 0.8)
  test$pred <- predict(modelo_arbol,newdata = test)
  train$pred <- predict(modelo_arbol,newdata = train)
  
  #tabla de contingencia de entrenamiento
  count(train, observado=semaforo, simulado=as.logical(pred))%>%
    mutate(across(c(observado, simulado),~ifelse(.x,"Peligroso","No Peligroso"))) %>% 
    group_by(observado) %>% 
    mutate(p=n/sum(n)*100) %>% 
    pivot_wider(id_cols = observado, names_from = simulado, values_from = p, 
                values_fill = 0)
  
  #tabla de contingencia de test
  count(test, observado=semaforo, simulado=as.logical(pred))%>%
    mutate(across(c(observado, simulado),~ifelse(.x,"Peligroso","No Peligroso"))) %>% 
    group_by(observado) %>% 
    mutate(p=n/sum(n)*100) %>% 
    pivot_wider(id_cols = observado, names_from = simulado, values_from = p, 
                values_fill = 0)
  
  AUC_Test <- auc(roc(test$semaforo, test$pred))
  AUC_Train <- auc(roc(train$semaforo, train$pred))
    
  
  ##ROC
  #creo las curvas sobre las que trabajaré.
  ## Curva ROC
  roc <- roc(test$semaforo, test$pred)
  roc1 <- roc(test$semaforo, test$pred, smooth = TRUE)
  
  
  
  
  # Graficar la curva ROC
  plot(roc, main = "Curva ROC", col = "blue")
  lines(roc1, col = "green")
  
  # Calcular el área bajo la curva (AUC)
  auc_value <- auc(roc)
  print(paste("AUC:", auc_value))
  
  # Intervalo de confianza
  roc_obj <- plot.roc(datosRF2$semaforo, datosRF2$pred, 
                      main = "Intervalo de confianza", percent = TRUE, ci = TRUE, print.auc = TRUE) 
  
  # Crear objeto para graficar el intervalo de confianza
  ci_obj <- ci.se(roc_obj, specificities = seq(0, 100, 5))
  
  # Graficar el intervalo de confianza
  plot(ci_obj, type = "shape", col = "#1c61b6AA")
  plot(ci(roc_obj, of = "thresholds", thresholds = "best"))
  
  