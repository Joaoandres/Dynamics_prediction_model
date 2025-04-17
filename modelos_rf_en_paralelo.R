library(tidyverse)
library(randomForest)
library(hydroGOF)
library(furrr)
load("data/output/datos_rf.rda")

rndfor <- function(datos, s=1, lag = 1, arb = 200, var = 20, 
                   lfun = "lag", output = NULL){
  
  datos_w <-datos %>% 
    select(time, cell, prec) %>% 
    group_by(cell) %>% 
    mutate(prec = match.fun(lfun)(prec, lag)) %>% 
    ungroup() %>% 
    pivot_wider(id_cols = time, 
                names_from = "cell", 
                names_prefix = "prc",
                values_from = "prec") %>% 
    left_join(datos %>% 
                select(time, cell, LAIm) %>% 
                pivot_wider(id_cols = time, 
                            names_from = "cell",
                            names_prefix = "lai",
                            values_from = "LAIm"), 
              by = "time") %>% 
    left_join(datos %>% 
                filter(cell == "px01") %>% 
                select(time, descarga, pendiente_avg, contribucion_avg) %>% 
                mutate(antec = zoo::rollapplyr(descarga, 5, FUN = mean, 
                                               fill = min(descarga))), 
              by = "time")
  
  set.seed(s)
  samp<- sample(nrow(datos_w),0.7*nrow(datos_w))
  train<- datos_w[samp,]
  test<- datos_w[-samp,]
  
  modelw<- randomForest(descarga~.,
                        data = select(train, -time), 
                        importance=TRUE,
                        ntree= arb, mtry= var,
                        na.action=na.omit)
  
  sim_data <- test%>% 
    mutate(pred = predict(modelw, newdata = test))
  
  sim_data_summary <- sim_data %>% 
    na.omit() %>% 
    summarise(seed = s, 
              lag = lag,
              abr = arb, 
              var = var, 
              Varexp = round(100 * modelw$rsq[length(modelw$rsq)], 2),
              KGE = hydroGOF::KGE(pred, descarga), 
              NSE = hydroGOF::NSE(pred, descarga),
              pBIAS = hydroGOF::pbias(pred, descarga),
              NRMSE = hydroGOF::nrmse(pred, descarga)
    )
  
  if(is.null(output)){
    return(list(mdl = modelw,
                train = train,
                test = sim_data,
                stats = sim_data_summary))
  } else {
    write_csv(sim_data_summary, file = output, append = TRUE)
  }
  
}


valores <- expand_grid(s = sample(1:1000,100), 
                       lag = 6, 
                       arb = 50, 
                       var =  30)


# Definir el plan para ejecutar en paralelo
# options(future.rng.onMisuse = "ignore")
plan(multisession, gc = TRUE)

# Ejecutar en paralelo
furrr::future_pwalk(valores[1:50,],
                    rndfor, 
                    datos = datos, 
                    output = "output/csv/modelos_estadisticas.csv")
