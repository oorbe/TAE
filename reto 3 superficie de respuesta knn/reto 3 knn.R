
'Attribute Information:

The inputs are as follows
X1=the transaction date (for example, 2013.250=2013 March, 2013.500=2013 June, etc.)
X2=the house age (unit: year)
X3=the distance to the nearest MRT station (unit: meter)
X4=the number of convenience stores in the living circle on foot (integer)
X5=the geographic coordinate, latitude. (unit: degree)
X6=the geographic coordinate, longitude. (unit: degree)

The output is as follow
Y= house price of unit area (10000 New Taiwan Dollar/Ping, where Ping is a local unit, 1 Ping = 3.3 meter squared)'
# librerias encesarias, selecciono las varaibles que me interesarán:
library(caret)
library(tidyverse)
setwd('C:/Users/Oliver/Documents/9/TAE/reto 3 superficie de respuesta knn')
datos_completos <- readxl::read_xlsx('Real estate valuation data set (1).xlsx',
                           col_names = c('n', 'x1', 'x2', 'x3', 'x4', 'x5', 'x6', 'y'), skip = 1)
datos <- datos_completos %>% 
              select('x1', 'x2', 'x3', 'x4', 'y')

# un análisis rapido a lo que son los datos:
dim(datos)
names(datos)
head(datos)
tail(datos)
summary(datos)
str(datos_completos)
table(datos_completos$x4)

# normalizando los datos para evitar confusiones por la diferencia de las escalas al modelar
# y más presiscion en la variabildad del error de validacion. Extraigo media desviacion:
datoc <- scale(datos[,c("x1", "x2", "x3", "x4", "y" )], center = T, scale = T)
centro<-attr(datoc,"center")
escala<-attr(datoc,"scale")
datos<-as.data.frame(datoc)

#-----------------------------------------------------------------------------------------------------
# Buscando el k optimo
#-----------------------------------------------------------------------------------------------------

# Se usará CV repetido 3 veces con k-folds=10:
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# seleccion de k optimo con 4 variables usanco 3 CV con k=10, y con k vecinos de 1-30:
knn_fit <- train(y ~., data = datos, method = "knn",
                  trControl=trctrl,
                  preProcess = c( "knnImpute"),
                 tuneGrid   = expand.grid(k = 1:30))

#Su resultado en los diferentes criterios de seleccion y el k vecimo con mejor resultado:
knn_fit$bestTune
knn_fit

#-----------------------------------------------------------------------------------------------------
### Selección del número de variables óptimas
#-----------------------------------------------------------------------------------------------------

# RemoveRedundant Features
correlationMatrix <- cor(datos[,1:4])
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.6)
# print indexes of highly correlated attributes
print(highlyCorrelated)

# la caracteristica x3  podria considerarse descartarla por mostrar correlacion -0.602519145 con x4
# pero no supera limite que seria 0.75%, para descartar.

#-----------------------------------------------------------------------------------------------------
## Gráfico de la súperficie de respuesta
#-----------------------------------------------------------------------------------------------------


## Seleccione el mejor modelo de dos variables y grafique la superficie de respuesta:.

# realizo 6 modelos para las conbinaciones de variables:
knn_fit1 <- train(y ~., data = datos[,c(1,2,5)], method = "knn",
                 trControl=trctrl,
                 preProcess = c( "knnImpute"),
                 tuneGrid   = expand.grid(k = 1:30))

knn_fit1
knn_fit1$bestTune
plot(knn_fit1)


knn_fit2 <- train(y ~., data = datos[,c(1,3,5)], method = "knn",
                  trControl=trctrl,
                  preProcess = c("knnImpute"),
                  tuneGrid   = expand.grid(k = 1:30))


knn_fit2
knn_fit2$bestTune
plot(knn_fit2)



knn_fit3 <- train(y ~., data = datos[,c(1,4,5)], method = "knn",
                  trControl=trctrl,
                  preProcess = c("knnImpute"),
                  tuneGrid   = expand.grid(k = 1:30))


knn_fit3
knn_fit3$bestTune
plot(knn_fit3)

knn_fit4 <- train(y ~., data = datos[,c(2,3,5)], method = "knn",
                  trControl=trctrl,
                  preProcess = c("knnImpute"),
                  tuneGrid   = expand.grid(k = 1:30))


knn_fit4
knn_fit4$bestTune
plot(knn_fit4)

knn_fit5 <- train(y ~., data = datos[,c(2,4,5)], method = "knn",
                  trControl=trctrl,
                  preProcess = c("knnImpute"),
                  tuneGrid   = expand.grid(k = 1:30))


knn_fit5
knn_fit5$bestTune[[1]]
plot(knn_fit5)


knn_fit6 <- train(y ~., data = datos[,c(3,4,5)], method = "knn",
                  trControl=trctrl,
                  preProcess = c("knnImpute"),
                  tuneGrid   = expand.grid(k = 1:30))


knn_fit6
knn_fit6$bestTune
plot(knn_fit6)

# con esto peudo resumir el codigo hecho, primero el par de variables luego sus criterios
# para seleccion de varaibles:
for (i in list(knn_fit1,knn_fit2,knn_fit3,knn_fit4,knn_fit5,knn_fit6)){
  print(i$coefnames)
  print(i$results %>% filter(k==i$bestTune[[1]]))
}

# segun estos resultados, fijandonos primeramente en RMSE las variables
# que optimizan con menor resupesta son x2 y x3:
#-----------------------------------------------

ctrl<-trainControl(method = "LGOCV",p=0.75,number = 20)
modelo_entrenamiento<-train(y~x2+x3,
                            data       = datos,
                            method     = "knn",
                            preProcess = c("center","scale"),
                            tuneGrid   = expand.grid(k = 1:30),
                            trControl  = ctrl,
                            metric     = "RMSE")

plot(modelo_entrenamiento)
importancia<-varImp(modelo_entrenamiento)
dotPlot(importancia)

#-----------------------------------------------

(medias<-knn_fit4$preProcess$mean)
(dsv_es<-knn_fit4$preProcess$std)

x2 <- seq(min(datos$x2), max(datos$x2), length.out = 100)
x3 <- seq(min(datos$x3), max(datos$x3), length.out = 100)
test.df<-expand.grid(x2,x3)
names(test.df)<-c("x2","x3")
test_pred <- predict(knn_fit4, newdata = test.df)
test.df$y <- test_pred
z <- matrix(test_pred,ncol=length(x3),nrow = length(x2))

persp(x2,x3,z,xlab="Edad vivienda",ylab="Distancia",zlab="Precio",
      main="Superficie de respuesta para un modelo con dos variables",theta=45,shade=0.3)

confusionMatrix(test_pred, test.df$Type )

library(plotly)
p <- plot_ly(x=x2,y=x3,z = z) 
p <- add_surface(p)
p<-layout(p,title='Precio vs dis MRT y Edad de viviend',
          xaxis=list(title="Edad"),
          yaxis=list(title="Distancia al MRT (m)"))
p

ggplot(datos_completos, aes(x=x2, y=x3)) + 
  geom_point()+
  labs(title="Antiguedad vs Distancia", x= 'Edad de Vivienda', y = "Distancia")+
  theme_light()




ctrl<-trainControl(method = "LGOCV",p=0.75,number = 20)
modelo_entrenamiento<-train(Y ~ X2+X3+X4,
                            data       = precios_red,
                            method     = "knn",
                            preProcess = c("center","scale"),
                            tuneGrid   = expand.grid(k = 1:30),
                            trControl  = ctrl,
                            metric     = "RMSE")
#  NO SUPE COMO HACE LA SUPERFICIE :()
