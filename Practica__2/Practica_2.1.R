##############################################################################
###                          Practica 2. Autómatas celular                  ##
###                                                                         ##
###                     14/08/2017                                          ##
###                                                                         ##
##############################################################################


library(sna)
library(magick)
library(parallel)
rm(list=ls())  #### Borramos los datos de memoria
dim <- c(50)
num <-  dim^2
probavivo<-seq(0,1, by=0.05)



paso <- function(pos) {
  fila <- floor((pos - 1) / dim) + 1
  columna <- ((pos - 1) %% dim) + 1
  vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),
                      max(columna - 1, 1): min(columna + 1, dim)]
  return(1 * ((sum(vecindad) - actual[fila, columna]) == 3))
}

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")

aq<-data.frame()
for ( dim in dim){ 
  clusterExport(cluster, "dim")
for (vivos  in  probavivo){
  clusterExport(cluster, "vivos")
  resultados = numeric()
  for (replica in 1:30) {
    actual <- matrix(1*(runif(num)<vivos), nrow=dim, ncol=dim)
    for (iteracion in 1:20) {
      clusterExport(cluster, "actual")  
      siguiente <- parSapply(cluster, 1:num, paso)  
      if (sum(siguiente) == 0) { # todos murieron
         resultados = c(resultados, iteracion, vivos)
         aq<-rbind(aq,c(dim,replica,vivos,iteracion))
         break;
      }
      actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
    }
  }
}
}

stopCluster(cluster)
colnames(aq)<-c("dimension","Replica","Probabilidad", "iteracion")







setwd("C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_2") 

w2<-paste("P2_N_D",dim,  ".pdf", sep="")
pdf(file = w2, bg = "transparent")



w1=paste("Dimensión ", dim,sep="")
b<-matrix(c(2,0,1,3),2,2,byrow=TRUE)
nf <- layout(b, widths=c(3,1), heights=c(1,3), respect=TRUE)
top <- max(c(xhist$counts, yhist$counts))
xhist <- hist(aq[,3], plot=FALSE)
yhist = hist(aq[,4], plot=FALSE)

par(mar=c(4,4,1,1))
boxplot(aq[,4]~aq[,3], ylab="iteración", xlab="Probabilidad", cex.lab=1.5)


par(mar=c(0,4,1,1))
plot(density(aq[,4]), xlab="", ylab="", main="", axes = F, col=4, lwd=2)
title(w1)
abline(v =mean(aq[,4]), col = "red", lty = 3)


par(mar=c(4,0,1,1))
barplot(yhist$counts, axes=FALSE, space=0, horiz=TRUE, col=topo.colors( length( yhist$breaks)))
abline(h =mean((aq[,3])), col = "green", lty = 3, lwd=2)


dev.off()
  