library(randomForestSRC)
load("data/output/datos_rf.rda")
data(datos)
set.seed(123)
samp<- sample(nrow(datos),0.8*nrow(datos))
train<- datos[samp,]
test<- datos[-samp,]
dim(train)
dim(test)



o <-tune(nivel~ prec+LAI+penmean+longitude+latitude,  data =  )
o$optimal

##generar modelo random forest
modeloss<-rfsrc.cart(nivel~prec+LAI+penmean+longitude+latitude,data=datos,  importance = TRUE,
                     ntree=1, 
           bootstrap = "none",na.action="na.omit")

##identificar las variables mas relevantes
o <-rfsrc(nivel ~prec+LAI+penmean+longitude+latitude, iris, importance = TRUE)
o$importance

##grafico del arbol del modelo. OJO tarda mucho tiempo en generarlo
plot(get.tree(modeloss, 1))


## 70% de varianza explicada


modelos11<-rfsrc(nivel~prec+LAI+penmean+longitude+latitude, data= datos, ntree= 1000,
      mtry= NULL, ytry= NULL, nodesize= NULL,
      nodedepth= NULL, splitrule= NULL,
      nsplit= 10, importance ="TRUE",
      ensemble = c("all", "oob", "inbag"), bootstrap = c("by.root", "none", "by.user"),
      samptype= c("swor", "swr"), samp= NULL, membership = FALSE, na.action= c("na.omit", "na.impute"),
      nimpute= 1, ntime= 250, proximity = FALSE, distance = FALSE, forest.wt= FALSE, xvar.wt= NULL, 
      yvar.wt= NULL, split.wt= NULL, case.wt= NULL, forest = TRUE, var.used= c(FALSE, "all.trees", "by.tree"), 
      split.depth= c(FALSE, "all.trees", "by.tree"), seed = NULL, do.trace= FALSE, statistics = FALSE)
##como guarda el modelo, dentro de file coloca la ruta donde se va a guardar
save(modelos11,file="")

modelos11