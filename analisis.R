library(ncdf4)
library(terra)
library(readr)
library(dplyr)  
library(stars)
library(tidyverse)
library(dplyr)
library(caret)

data <- stars::read_ncdf("data/output/P.nc", var = "prec")

datos_df <- as.data.frame(data) |> 
  na.omit() |> 
  arrange(longitude,latitude,time) %>% 
  group_by(time) %>% 
  mutate(cell = sprintf("px%02d", 1:n()), 
         prec = units::drop_units(prec))



####  
practicas<- read_csv("estacion/atacapi_min1.dat",
                     col_names=c("TIMESTAMP","RECORD","nivel_Max",
                                 "nivel_Min","nivel_Avg","nivel","nivel_Std","battVolt_Min"),
                     skip=4, na="-7999")

practicas30 <- practicas%>% 
  group_by(TIMESTAMP=lubridate::floor_date(TIMESTAMP,"30 minutes")) %>% 
  summarise(Qavg=mean(nivel_Avg,na.rm=FALSE) %>% round(2)) 

data_rf <- pivot_wider(datos_df, id_cols = c(time), values_from = prec, names_from = cell) %>% 
  left_join(practicas30, by = c("time" = "TIMESTAMP"))

# practicas$CR300=as.numeric(prac$CR300)
# practicas$"11262"=as.numeric(prac$"11262")
#unir 


c <- ccf(datos_df$prec,datos_df$caudal,lag.max = 48,na.action = na.omit)
c$acf[which.max(c$acf)]
c$lag[which.max(c$acf)]

ct <- cor.test(dplyr::lead(datos_df$prec, 22), datos_df$caudal)
ct$estimate
help(train)
MODELO_1<- train(x= datos_df$prec, datos_df$caudal, method= "rf")


plot(caudal,x<- caudal$CR300,y<- caudal$'11262' )
crlcruz<- cff(caudal$X,caudal$Y,lag.max=3, type="correlation")




## correlated predictors
predictores <- as.data.frame(data_rf[,-c(1, 76)])
nzv <- nearZeroVar(predictores)
predictores_f <- predictores
descrCor <-  cor(predictores)
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .999)

