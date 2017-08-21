##############################################################################
###                          Practica 2 Autómatas celular                  ##
###                         Primer Reto                                     ##
###                     14/08/2017                                          ##
###                                                                         ##
##############################################################################


library(sna)
library(magick)
library(parallel)
rm(list=ls())

dim <- 25
num <-  dim^2
proba<-0.1
nucleos<-c(1,2,3,4,5,6,7)

paso <- function(pos) {
  fila <- floor((pos - 1) / dim) + 1
  columna <- ((pos - 1) %% dim) + 1
  vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),
                      max(columna - 1, 1): min(columna + 1, dim)]
   if (actual[fila,columna]==0) {
    for ( aq1 in vecindad )
    {
    if(aq1!=0){
      if(runif(1)<proba){
       return(aq1) 
        }
      }### Primer IF
    }   
      return(0)
    }             ### If externo
  if (actual[fila,columna]!=0){
    return(actual[fila,columna])
  }
} 
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")
clusterExport(cluster, "proba")

    actual<- matrix(0,  nrow=dim, ncol=dim)
    for ( i in 1: length(nucleos)){
    actual[(round((dim)*runif(1))),(round((dim)*runif(1)))]=nucleos[i]  }
    salida = paste("p2_t", 0, ".png", sep="")
    png(salida)
    plot.sociomatrix(actual, diaglab=FALSE, main="Paso 0",drawlab=F)
    graphics.off()
    
for (iteracion in 1:(dim+floor(dim*0.2)) ) {
      clusterExport(cluster, "actual")  
      siguiente <- parSapply(cluster, 1:num, paso)  
      actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
      salida = paste("p2_t", iteracion, ".png", sep="")
      tiempo = paste("Paso", iteracion)
      png(salida)
      plot.sociomatrix(actual, diaglab=FALSE, main=tiempo,drawlab=F)
     graphics.off()
} 
   stopCluster(cluster)
      
   setwd("C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_2") 
   
   
   frames=lapply(0:iteracion,function(x) image_read(paste("p2_t",x,".png",sep="")))
   animation <- image_animate(image_join(frames))
   print(animation)
   w3<-paste("P2_R1_D", dim, ".gif"  )
   image_write(animation, w3)
   sapply(0:iteracion,function(x) file.remove(paste("p2_t",x,".png",sep="")))
      
   write.csv (actual, file="actual.csv")

   
   ### Elementos que tocaron los bordes
   
  
   adq<-as.numeric(names( table( c( actual[1,], actual[,dim],actual[dim,],actual[,1]))))
   qv<-matrix(t((table(actual) )), nrow=length((table(actual) )),ncol=2)
   qv[,2]<-as.numeric(names(table(actual)))
  
  
   
fil<- qv[,2] %in% adq

tt<-matrix(qv[ fil==TRUE],ncol=2,nrow=length(adq))
tt2<-matrix(qv[ fil==FALSE],ncol=2,nrow=length(qv[ fil==FALSE])/2)



w21<-paste("P2_R1_Dis",dim,  ".pdf", sep="")
pdf(file = w21, bg = "transparent")


par(mfcol=c(1,2))
barplot(tt[,1], xlab="Automatas", ylab="casillas" ,main="Tocan el borde", names.arg=tt[,2], col=topo.colors(length(qv[ fil==TRUE])/2))


barplot(tt2[,1], ylim=c(0, max(qv[,1] )) , xlab="Automatas", ylab="casillas" ,main="No tocan el borde", names.arg=tt2[,2], col=topo.colors(length(qv[ fil==FALSE])/2))

dev.off()






        
        
  