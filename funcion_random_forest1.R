library(tidyverse)
library(randomForest)
library(hydroGOF)
library(ggplot2)
load("data/output/datos_rf.rda")

rndfor <- function(datos, s=1, lag = 1, arb = 200, var = 20, lfun = "lag"){
  
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

  
  samp<- 1:round(0.7*nrow(datos_w))
  train<- datos_w[samp,]
  test<- datos_w[-samp,]
  
  #set.seed(s)
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
 
  return(list(mdl = modelw,
              train = train,
              test = sim_data,
              stats = sim_data_summary))
  #return(sim_data_summary)
}

best <- rndfor(datos, s = 1, lag = 7, var = 73, arb = 100, lfun = "lag")
best$stats

best[[4]]
best[[2]]$pred <- predict(best[[1]], newdata =  best[[2]])
bind_rows(mutate(best[[2]], periodo = "01Entrenamineto"), 
          mutate(best[[3]], periodo = "02 validación")) %>%  
  ggplot(aes(time,descarga))+
  geom_line(aes(color="obs"))+
  geom_line(aes(y=pred, color="pred"))+
  facet_grid(~periodo, scales = "free_x") +
  theme_classic()
importance(best[[1]])
view(best[[3]][,50:75])
plotly::ggplotly()
test_lag <- map_dfr(1:6, rndfor, s = 1, datos = datos, arb = 10, lfun = "lag")
test_lead <- map_dfr(1:6, rndfor, s = 1, datos = datos, arb = 10, lfun = "lead")

bind_rows(test_lag, test_lead) %>% view()

test_arb <- map_dfr(c(50, 100, 150, 200, 300, 500, 1000, 2000), rndfor, 
                    datos = datos, s = 1, lag = 2, var = 20, lfun = "lag")

test_var <- map_dfr(10:70, rndfor, datos = datos, lag = 1, arb = 100)

test_rep <- map_dfr(1:500, rndfor, var=24, datos = datos, lag = 1, arb = 100)


seed


save(test_rep, file ="data/test_rep")
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
