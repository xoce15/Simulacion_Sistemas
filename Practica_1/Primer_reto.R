##############################################################################
###  Caminata Aleatoria para la asignatura de simulacion de sistemas        ##
###                    Primer reto                                          ##
###                     07/08/2017                                          ##
###                                                                         ##
##############################################################################

#install.packages("pryr")
#library(parallel)
#library(stats)
#install.packages("foreach")
#install.packages("doParallel")
#library(moments)
#library(nortest)
#library(e1071)

setwd("C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas") 

library(stats)
library(parallel)
library(pryr)

cluster <- makeCluster(detectCores() - 1)
datos <- data.frame()
inf<- data.frame()


dura1<-c(7,7*3,7*9,7*27)
repet1<-c(5,10,30,50,100)
  
#  for (repetir in repet1 ) {
#    clusterExport(cluster, "repetir")
    
    for (duracion in dura1) {
      clusterExport(cluster, "duracion")
    for (dimension in 1:8) {
      clusterExport(cluster, "dimension")
     
      resultado <- parSapply(cluster, 1,
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
                                cont<-Sys.time()
                                # origen<-rep(0,dimension)
                                # if (all(pos==origen)){
                                #   cont<-cont+1
                                   
                                # }
                               }
                               return(cont)
                             })
      datos <- rbind(datos, resultado)
      
      for (i in 1:length(resultado) )
      {
        inf<-rbind( inf, c(dimension, duracion, resultado[i]))
        colnames(inf)<-c("dimension","duracion",  "resultado")
      }
    }
  }


stopCluster(cluster)

alpha=0.05

tr<-lm(resultado~dimension+duracion, data=inf)
summary(tr)
al<-resid(tr)
v1<-shapiro.test(al)
v2<-ad.test(al)



if(v1>alpha && v2>alpha)
{  
hist(al, col=rainbow(10), main="Histograma",xlab="Clases",ylab="Frecuencia")
sink("Primer_reto_lm.txt")
print(summary(tr))
sink() 
pdf(file ="Grafica.pdf", bg = "transparent")
par(mfcol=c(1,2))
boxplot(inf[,3]~inf[,1], xlab="Dimensiones",ylab="Tiempo",col=rainbow(8))
boxplot(inf[,3]~inf[,2], xlab="Duración",ylab="Tiempo",col=rainbow(5))
dev.off()
print("SA")
}else {
  cluster <- makeCluster(detectCores() - 1)
  clusterExport(cluster,"inf")
  az<-parSapply(cluster,1, function(r) {
    Valor_p<-apply(inf[,1:2],2, function(x) kruskal.test(inf[,1]~x)$p.value)
    Estadistico<-apply(inf[,1:2],2, function(x) kruskal.test(inf[,1]~x)$statistic )
    ar<-c(Estadistico,Valor_p)
    return(ar) 
  })
  stopCluster(cluster)
  az
  az<-matrix(az,ncol=2,nrow = 2)
  row.names(az)<-c("Dimensión","Duración")
  colnames(az)<-c("Kruskal","Valor P")
  az
  write.csv (az, file="Kruskal_WalliParalelizado_1reto.csv")
  print("No normales")
}


