#' Script de la segunda sesión del curso Técnicas en Aprendizaje Estadístico
#' Universidad Nacional de Colombia
#' Profesor: Juan David Ospina Arango
#' Fecha: 25 de febrero de 2021
#' 
#' Objetivo: introducir algunas funcionalidades de R y simular variables
#' aleatorias "famosas" y compararlas con su distribución teórica


#' Introduce la capacidad de R para generar secuencias de números,
#' funciones incorporadas y parámetros gráficos
x <- seq(-2*pi, 2*pi, length.out = 1000)
y <- sin(x)
plot(x,y,type="l",lwd=2,las=1,xlab="x",ylab="Sin(x)",main="Gráfica de la función seno")
lines(x,0.5*y,col="red")
grid()


#' Gráfica de números provenientes de una distribución normal 
#' con diferentes valores de media y varianza:
set.seed(20210225) # Esto fija la semilla del generador de números aleatorios
x1 <- rnorm(1000,mean=0,sd=1)
x2 <- rnorm(1000,mean=1,sd=1)
x3 <- rnorm(1000,mean=0,sd=2)

# Gráfico de los tres histogramas correspondientes:
par(mfrow=c(1,3))
hist(x1,xlim = c(-7,7),las=1);points(x=0,y=0,pch=2,col="red",lwd=5)
hist(x2,xlim = c(-7,7),las=1);points(x=1,y=0,pch=2,col="red",lwd=5)
hist(x3,xlim = c(-7,7),las=1);points(x=0,y=0,pch=2,col="red",lwd=5)


# Gráfico del histograma real:
library(MASS)
par(mfrow=c(1,3))
# Gr1
truehist(x1,xlim = c(-7,7),ylim=c(0,0.4),las=1)
points(x=0,y=0,pch=2,col="red",lwd=5)
# Gr2
truehist(x2,xlim = c(-7,7),ylim=c(0,0.4),las=1)
points(x=1,y=0,pch=2,col="red",lwd=5)
# Gr3
truehist(x3,xlim = c(-7,7),ylim=c(0,0.4),las=1)
points(x=0,y=0,pch=2,col="red",lwd=5)


# Cálculo de las densidades teóricas
x <- seq(-7,7,length.out=100)
y1 <- dnorm(x,mean=0,sd=1)
y2 <- dnorm(x,mean=1,sd=1)
y3 <- dnorm(x,mean=0,sd=2)


# Gráfico del histograma real sobreponiendo la densidad teórica:
par(mfrow=c(1,3))
# Gr1
truehist(x1,xlim = c(-7,7),ylim=c(0,0.4),las=1)
points(x=0,y=0,pch=2,col="red",lwd=5)
lines(x,y1,col="red",lwd=3)
# Gr2
truehist(x2,xlim = c(-7,7),ylim=c(0,0.4),las=1)
points(x=1,y=0,pch=2,col="red",lwd=5)
lines(x,y2,col="red",lwd=3)
# Gr3
truehist(x3,xlim = c(-7,7),ylim=c(0,0.4),las=1)
points(x=0,y=0,pch=2,col="red",lwd=5)
lines(x,y3,col="red",lwd=3)

# Comparemos las tres densidades:
plot(x,y1,type="l",col="red",las=1)
lines(x,y2,col="blue")
lines(x,y3,col="green")



