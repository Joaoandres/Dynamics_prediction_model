library(hydroTSM)
load("data/output/datos_rf.rda")
head(datos)
Flowdc= fdc(datos$descarga,lQ.thr=0.9,hQ.thr=0.1)
caudal <- datos$nivel %>% 
  select('time', 'prec', 'cell', 'nivel','descarga') %>% 
  ungroup() %>% 
  mutate(umbral = nivel >= 1.1, 
         bsf = bsf,
         excess = excess)

ggplot(caudal) +
  aes(time, descarga, color = excess > 0) +
  geom_point(shape = ".") +
  geom_point(aes(y = bsf), shape = ".", color = "blue")

ggplot(caudal) +
  aes(time, excess, color = excess > 0 ) +
  geom_point(shape = ".")

ecdf(caudal$excess) %>% plot()
abline(h = 0.9, col = "red")
