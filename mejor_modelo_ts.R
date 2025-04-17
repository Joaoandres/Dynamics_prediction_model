library(tidyverse)
library(randomForest)
library(hydroGOF)
library(ggplot2)
theme_set(theme_classic(base_family = "times", base_size = 10))

load("data/output/datos_rf.rda")

rndfor <- function(datos, s=1, lag = 1, arb = 200, var = 20, 
                   lfun = "lag", window = 5, output = NULL){
  
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
                mutate(antec = zoo::rollapplyr(descarga, window, FUN = mean, 
                                               fill = min(descarga))), 
              by = "time")
  
  samp<- 1:round(0.7*nrow(datos_w))
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
              KGE = KGE(pred, descarga), 
              NSE = NSE(pred, descarga),
              pBIAS = pbias(pred, descarga),
              NRMSE = nrmse(pred, descarga)
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

best <- rndfor(datos, s =341, lag = 6, var = 30, arb = 50, lfun = "lag")

best[[2]]$pred <- predict(best[[1]], newdata =  best[[2]])

bind_rows(mutate(best[[2]], periodo = "Perído de Entrenamiento"), 
          mutate(best[[3]], periodo = "Perído de Validación")) %>%  
  ggplot(aes(time,descarga))+
  geom_line(aes(color="Observado"), linewidth = 0.5)+
  geom_line(aes(y=pred, color="Predicción"), linewidth = 0.2, linetype="dashed")+
  scale_x_datetime(date_breaks = "4 months")+
  scale_color_manual(values = c("steelblue","orange"), 
                     guide = guide_legend(override.aes = list(linetype = c(1, 2))))+
  coord_cartesian(expand = FALSE) +
  facet_grid(~periodo, scales = "free_x", space = "free_x") +
  labs(y = expression("Caudal [m"^3/"s]")) +
  theme(panel.spacing = unit(0, "npc"),
        panel.border = element_rect(color = "#c0c0c0", fill = NA),
        legend.title = element_blank(), legend.position = "bottom")

plotly::ggplotly()

ggsave("output/png/ts_prediccion.png", units = "cm", width = 15.6, height = 8, 
       dpi = 300, scale = 1.2)
