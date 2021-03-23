# Valores aleatorios de la normal y sus histogramas:

set.seed(123)
n1 <- rnorm(10000,0,1)
n2 <- rnorm(10000,2,1)
n3 <- rnorm(10000,2,2)
n4 <- rnorm(10000,3,4)

# En esllos se observa el engaño visual
par(mfrow=c(2,2))
hist(n1,xlim = c(-7,7),ylim=c(0,2000),las=1)
hist(n2,xlim = c(-7,9),ylim=c(0,2000),las=1)
hist(n3,xlim = c(-9,9),ylim=c(0,2000),las=1)
hist(n4,xlim = c(-9,12),ylim=c(0,2000),las=1)

# Para esto usamos truehist
truehist(n1,xlim = c(-7,7),ylim=c(0,0.4),las=1)
truehist(n2,xlim = c(-7,9),ylim=c(0,0.4),las=1)
truehist(n3,xlim = c(-9,9),ylim=c(0,0.4),las=1)
truehist(n4,xlim = c(-9,12),ylim=c(0,0.4),las=1)

#Con ggplot:
library(ggplot2)
ggplot(as.data.frame(n1), aes(x=n1)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="lightblue")+
  geom_density(alpha=.2, fill="#FF6666") +
  geom_vline(aes(xintercept=mean(n1)),
             color="blue", linetype="dashed", size=1)

g1 <- rgamma(1000,1,1)
g2 <- rgamma(1000,2,1)
g3<- rgamma(1000,3,0.5)
g4 <- rgamma(1000,3,1)

par(mfrow=c(2,2))
hist(g1,xlim = c(0,12),ylim=c(0,1000),las=1)
hist(g2,xlim = c(0,12),ylim=c(0,1000),las=1)
hist(g3,xlim = c(0,12),ylim=c(0,1000),las=1)
hist(g4,xlim = c(0,12),ylim=c(0,1000),las=1)

truehist(g1,xlim = c(0,12),ylim=c(0,1.5),las=1)
truehist(g2,xlim = c(0,12),ylim=c(0,1.5),las=1)
truehist(g3,xlim = c(0,12),ylim=c(0,1.5),las=1)
truehist(g4,xlim = c(0,12),ylim=c(0,1.5),las=1)

# Trabajamos las densidades:
x <- seq(0,12,length.out=100000)
dg1 <- dgamma(x,1,1)
dg2 <- dgamma(x,2,1)
dg3 <- dgamma(x,3,0.5)
dg4 <- dgamma(x,3,1)

truehist(g1,xlim = c(0,12),ylim=c(0,1.5),las=1);lines(x,dg1, col= 'red', lwd=1.5)
truehist(g2,xlim = c(0,12),ylim=c(0,1.5),las=1);lines(x,dg2, col= 'red', lwd=1.5)
truehist(g3,xlim = c(0,12),ylim=c(0,1.5),las=1);lines(x,dg3, col= 'red', lwd=1.5)
truehist(g4,xlim = c(0,12),ylim=c(0,1.5),las=1);lines(x,dg4, col= 'red', lwd=1.5)

install.packages('gridExtra')
library(gridExtra)

g1 <- ggplot(as.data.frame(dg1), aes(x=dg1)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="lightblue")+
  geom_density(alpha=.2, fill="#FF6666")+
  coord_cartesian(xlim =c(0, 1), ylim = c(0, 30))


g2 <- ggplot(as.data.frame(dg2), aes(x=dg2)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="lightblue")+
  geom_density(alpha=.2, fill="#FF6666")+
  coord_cartesian(xlim =c(0, 1), ylim = c(0, 30))


g3 <- ggplot(as.data.frame(dg3), aes(x=dg3)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="lightblue")+
  geom_density(alpha=.2, fill="#FF6666")+
  coord_cartesian(xlim =c(0, 1), ylim = c(0, 30))


g4 <- ggplot(as.data.frame(dg4), aes(x=dg4)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="lightblue")+
  geom_density(alpha=.2, fill="#FF6666")+
  coord_cartesian(xlim =c(0, 5), ylim = c(0, 30))


grid.arrange(g2, g2, g3, g4,nrow = 2)

