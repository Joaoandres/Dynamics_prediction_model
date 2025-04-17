library(tidyverse)
load("data/output/datos_dems.rda")
load("data/output/datos_laidf.rda")
load("data/output/datos_df.rda")

datos <- left_join(datos_df,
                   datos_lai, by=c("time", "cell")) %>%
  left_join(datos_dems,by = "cell") %>% 
  group_by(cell) %>% 
  fill(LAIm, LAIx, .direction = "downup") %>% 
  ungroup()

save(datos,file="data/output/datos_rf.rda")
