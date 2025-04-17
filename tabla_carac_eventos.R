tmp <- datosRF2[,-c(1:3, 8, 13:15, 19:20)] %>% 
  summarise(across(everything(), 
                   .fns = list(avg=mean, sd = sd, max=max, min=min, med = median))) %>% 
  mutate(across(everything(), function(x) {as.numeric(x)})) %>% 
  pivot_longer(cols = everything(), names_to = c("var", "stat"), names_sep = "_") %>% 
  pivot_wider(id_cols = var, names_from = stat, values_from = value)

tmp
write.csv(tmp, file = "output/csv/summary_statistics.csv", row.names = FALSE)
