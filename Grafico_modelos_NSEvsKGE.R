library(tidyverse)
theme_set(theme_classic(base_family = "times", base_size = 10))

modelos <- read_csv("output/csv/modelos_estadisticas3.csv") 
  
  mejor5 <- modelos |> 
  mutate(pareto = sqrt((1-KGE)^2 + (1-NSE)^2)) |> 
  top_n(5, -pareto)

mejor1 <- modelos |> 
  mutate(pareto = sqrt((1-KGE)^2 + (1-NSE)^2)) |> 
  top_n(1, -pareto)

modelos |> 
  ggplot() +
  aes(x = 1 - KGE, y = 1 - NSE) +
  geom_point(aes(color = "100 mejores")) +
  geom_point(data = mejor5, aes(color = "5 mejores")) +
  geom_point(data = mejor1, aes(color = "El mejor")) +
  scale_color_manual(values = c("El mejor" = "#e43831", 
                                "5 mejores" = "#619cff", 
                                "100 mejores" = "#c0c0c0")) +
  theme(legend.title = element_blank(),
        legend.position = "bottom")


ggsave("output/png/NSEvsKGE.png", 
               width = 10, height = 10, 
               units = "cm", dpi = 300, 
               scale = 1)

