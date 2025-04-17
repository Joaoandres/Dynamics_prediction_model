library(tidyverse)
library(randomForest)
library(hydroGOF)
library(ggplot2)
load("data/output/datos_rf.rda")

rndfor <- function(datos, s=1, lag = 1, arb = 200, var = 20, 
                   lfun = "lag", output = NULL){
  
  datos_w <-datos %>% 
    select(time, cell, prec) %>% 
    group_by(cell) %>% 
    mutate(prec = match.fun(lfun)(prec, 3)) %>% 
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
                select(time, descarga, pendiente_avg, contribucion_avg), 
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
              test = test,
              stats = sim_data_summary))
  } else {
    write_csv(sim_data_summary, file = output, append = TRUE)
  }
  
}

best <- rndfor(datos, s = 102, lag = 1, var = 24, arb = 100, lfun = "lag")

best[[3]]$pred <- predict(best[[1]], newdata =  best[[3]])
best[[3]] %>% 
  ggplot(aes(time,descarga))+
  geom_line(aes(color="obs"))+
  geom_line(aes(y=pred, color="pred"))
ggplot()
test_lag <- map_dfr(1:6, rndfor, datos = datos, arb = 10)
test_lead <- map_dfr(1:6, rndfor, datos = datos, arb = 10, lfun = "lead")

test_arb <- map_dfr(c(100, 150, 200, 300, 500, 1000, 2000), rndfor, 
                    datos = datos, lag = 1)

test_var <- map_dfr(10:70, rndfor, datos = datos, lag = 1, arb = 100)

test_rep <- map_dfr(1:500, rndfor, var=24, datos = datos, lag = 1, arb = 100)

#buscar mejor ntree y mtry

valores <- expand_grid(s=1:10, 
                       lag = 1:5, 
                       arb = c(10, 20, 50, 100, 200, 300), 
                       var = c(5, 10, 20, 30, 50, 70))
pmap(valores[-(1:635),], 
     rndfor, datos = datos, output = "output/csv/modelos_estadisticas.csv")



+save(test_rep, file ="data/test_rep")
load("data/test_rep")
  bind_rows(test_lag, test_lead)
  view("test_rep")

  
sim_data %>% 
  ggplot(aes(time,descarga))+
  geom_line(aes(color="obs"))+
  geom_line(aes(y=pred, color="pred"))

plotly::ggplotly()
test_rep <- as.data.frame(test_var)
ggplot(data = test_var, aes(x = var, y = KGE)) +
  geom_line() +
  labs(x = "var", y = "KGE", title = "VS") 
