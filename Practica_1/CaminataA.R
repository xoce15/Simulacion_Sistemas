##############################################################################
###  Caminata Aleatoria para la asignatura de simulacion de sistemas        ##
###                    Numero de veces de retorno de una particula          ##
###                     07/08/2017                                          ##
###                                                                         ##
##############################################################################

#install.packages("pryr")
#library(parallel)
#library(stats)
#install.packages("foreach")
#install.packages("doParallel")

setwd("C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas") 

library(stats)
library(moments)
library(nortest)
library(e1071)
library(parallel)
library(pryr)

cluster <- makeCluster(detectCores() - 1)
datos <- data.frame()
inf<- data.frame()


dura1<-c(7,7*3,7*9,7*27)
repet1<-c(5,10,30,50,100)

for (duracion in dura1) {
  clusterExport(cluster, "duracion")
for (repetir in repet1 ) {
for (dimension in 1:8) {
  clusterExport(cluster, "dimension")
  clusterExport(cluster, "repetir")

  resultado <- parSapply(cluster, 1:repetir,
                         function(r) {
                           pos <- rep(0, dimension)
                           mayor <- 0
                           cont <-0
                          
                           for (t in 1:duracion) {
                              cambiar <- sample(1:dimension, 1)
                             cambio <- 1
                             if (runif(1) < 0.5) {
                               cambio <- -1
                             }
                             pos[cambiar] <- pos[cambiar] + cambio
                             origen<-rep(0,dimension)
                             if (all(pos==origen)){
                               cont<-cont+1
                          
                             }
                           }
                           return(cont)
                         })
  datos <- rbind(datos, resultado)
  
  for (i in 1:length(resultado) )
  {
  inf<-rbind( inf, c(repetir,dimension, duracion, resultado[i]))
  colnames(inf)<-c("repeticion","dimension","duracion",  "resultado")
  }
}
}
}

stopCluster(cluster)

alpha=0.05
a<-lm(inf[,4]~inf[,1]+inf[,2]+inf[,3])
summary(a)
c<-ad.test(resid(a))[2]
if(c>alpha){

#### Generación de graficos  prueba de normalidad y no normalidad#####
pdf(file = "Normalidad_C1.pdf", bg = "transparent")
par(mfcol=c(1,2))
hist(b, freq=F, ylim=c(0,0.35), main="Histograma", ylab="Densidad", xlab="Residuos");lines(density(b),col=2)
text(12,0.1,"AD Valor p:");text(26,0.1,c)
qqnorm(b, main="");qqline(b)
dev.off()
sink("lm.txt")
print(summary(a))
sink() 
print("Normales")
}else {
  cluster <- makeCluster(detectCores() - 1)
  clusterExport(cluster,"inf")
  az<-parSapply(cluster,1, function(r) {
      Valor_p<-apply(inf[,1:3],2, function(x) kruskal.test(inf[,4]~x)$p.value)
      Estadistico<-apply(inf[,1:3],2, function(x) kruskal.test(inf[,4]~x)$statistic )
      ar<-c(Estadistico,Valor_p)
  return(ar) 
})
stopCluster(cluster)
az<-matrix(az,ncol=2,nrow = 3)
row.names(az)<-c("Duración","Dimensión","Repetición")
colnames(az)<-c("Kruskal","Valor P")
write.csv (az, file="Kruskal_WalliParalelizado.csv")
print("No normales")
}






