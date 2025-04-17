library(tidyverse)
library(patchwork)
theme_set(theme_classic(base_family = "times", base_size = 10))

modelos <- read_csv("output/csv/modelos_estadisticas2.csv") %>% 
filter(Var!=73) %>%
  mutate(Var= factor(Var),
           Lag= factor(Lag),
         Arb=factor(Arb)) %>% 
  rename(Variables=Var, Desfase=Lag,Arboles= Arb)

var_kge <- modelos |> 
  group_by(Variables) |> 
  top_n(20, wt = KGE) |>
  ggplot() +
  aes(x = Variables, y = KGE) +
  geom_boxplot(linewidth = 0.2)

lag_kge <- modelos |> 
  group_by(Desfase) |> 
  top_n(30, wt = KGE) |>
  ggplot() +
  aes(x = Desfase, y = KGE) +
  geom_boxplot(linewidth = 0.2) 

arb_kge <- modelos |> 
  group_by(Arboles) |> 
  top_n(20, wt = KGE) |>
  ggplot() +
  aes(x = Arboles, y = KGE) +
  geom_boxplot(linewidth = 0.2) 



##NSE graficos
var_nse <- modelos |> 
  group_by(Variables) |> 
  top_n(20, wt = NSE) |>
  ggplot() +
  aes(x = Variables, y = NSE) +
  geom_boxplot(linewidth = 0.2)

lag_nse <- modelos |> 
  group_by(Desfase) |> 
  top_n(30, wt = NSE) |>
  ggplot() +
  aes(x = Desfase, y = NSE) +
  geom_boxplot(linewidth = 0.2) 

arb_nse <- modelos |> 
  group_by(Arboles) |> 
  top_n(20, wt = NSE) |>
  ggplot() +
  aes(x = Arboles, y = NSE) +
  geom_boxplot(linewidth = 0.2) 

##graficos de pbias

var_pbias <- modelos |> 
  group_by(Variables) |> 
  top_n(20, wt = pBias) |>
  ggplot() +
  aes(x = Variables, y = pBias) +
  geom_boxplot(linewidth = 0.2)

lag_pbias <- modelos |> 
  group_by(Desfase) |> 
  top_n(30, wt = pBias) |>
  ggplot() +
  aes(x = Desfase, y = pBias) +
  geom_boxplot(linewidth = 0.2) 

arb_pbias <- modelos |> 
  group_by(Arboles) |> 
  top_n(20, wt = pBias) |>
  ggplot() +
  aes(x = Arboles, y = pBias) +
  geom_boxplot(linewidth = 0.2) 
#RMSE

var_nrmse <- modelos |> 
  group_by(Variables) |> 
  top_n(20, wt = NRMSE) |>
  ggplot() +
  aes(x = Variables, y = NRMSE) +
  geom_boxplot(linewidth = 0.2)

lag_nrmse <- modelos |> 
  group_by(Desfase) |> 
  top_n(30, wt = NRMSE) |>
  ggplot() +
  aes(x = Desfase, y = NRMSE) +
  geom_boxplot(linewidth = 0.2) 

arb_nrmse <- modelos |> 
  group_by(Arboles) |> 
  top_n(20, wt = NRMSE) |>
  ggplot() +
  aes(x = Arboles, y = NRMSE) +
  geom_boxplot(linewidth = 0.2) 


  arb_kge+var_kge +lag_kge+ 
  arb_nse+var_nse + lag_nse+
  arb_pbias+var_pbias + lag_pbias + 
  arb_nrmse+var_nrmse + lag_nrmse +
  plot_layout(ncol = 3) +
  plot_annotation(tag_levels = "a", tag_suffix = ")")

ggsave("output/png/resultados_boxplot.png", 
       width = 15, height = 12, 
       units = "cm", dpi = 300, 
       scale = 1.5)