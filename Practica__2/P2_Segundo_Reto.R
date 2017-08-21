##############################################################################
###                          Practica 2. Autómatas celular                  ##
###                         Segundo  Reto                                   ##
###                     14/08/2017                                          ##
###                                                                         ##
##############################################################################


library(sna)
library(magick)
library(parallel)


dim <- 25
num <-  dim^2
proba<-0.1
proba2<-0.9


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

nucleos<-seq(1:10)



actual<- matrix(0,  nrow=dim, ncol=dim)
for ( i in 1: length(nucleos)){
  actual[(round((dim)*runif(1))),(round((dim)*runif(1)))]=nucleos[i]  }
  salida = paste("p2_t", 0, ".png", sep="")
  png(salida)
  plot.sociomatrix(actual, diaglab=FALSE, main="Paso 0",drawlab=F)
  graphics.off()



for (iteracion in 1:(dim+floor(dim*0.2))) {
  clusterExport(cluster, "actual")  
  siguiente <- parSapply(cluster, 1:num, paso)  
  
  actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
  if( runif(1)<proba2){
    nucleos<-c(nucleos,length(nucleos)+1)
    fila1<-runif(1)
    colum1<-runif(1)
    if(actual[(1+floor((dim)*fila1)),(1+floor((dim)*colum1))]==0) {
  actual[(1+floor((dim)*fila1)),(1+floor((dim)*colum1))]<-length(nucleos)
  }
  }
  salida = paste("p2_t", iteracion, ".png", sep="")
  tiempo = paste("Paso", iteracion)
  png(salida)
  plot.sociomatrix(actual, diaglab=FALSE, main=tiempo, drawlab=F)
 
 ### Si se quiere generar  la matriz con diferentes colores  ########  
  # image(actual,  main=tiempo,  col=rainbow(length(nucleos)+1)  , xaxt='n', yaxt='n')
  graphics.off()
} 

stopCluster(cluster)

frames=lapply(0:iteracion,function(x) image_read(paste("p2_t",x,".png",sep="")))
animation <- image_animate(image_join(frames))
print(animation)
image_write(animation, "R1_crecimiento.gif")
sapply(0:iteracion,function(x) file.remove(paste("p2_t",x,".png",sep="")))

#  file.remove(list.files(pattern=".png")) 



### Elementos que tocaron los bordes


adq<-as.numeric(names( table( c( actual[1,], actual[,dim],actual[dim,],actual[,1]))))
qv<-matrix(t((table(actual) )), nrow=length((table(actual) )),ncol=2)
qv[,2]<-as.numeric(names(table(actual)))



fil<- qv[,2] %in% adq

tt<-matrix(qv[ fil==TRUE],ncol=2,nrow=length(adq))
tt2<-matrix(qv[ fil==FALSE],ncol=2,nrow=length(qv[ fil==FALSE])/2)



w21<-paste("P2_R2_Dis",dim,  ".pdf", sep="")
pdf(file = w21, bg = "transparent")


par(mfcol=c(2,1))
barplot(tt[,1], xlab="Automatas", ylab="casillas" ,main="Tocan el borde", names.arg=tt[,2], col=topo.colors(length(qv[ fil==TRUE])/2))



barplot(tt2[,1], ylim=c(0, max(qv[,1] )) , xlab="Automatas", ylab="casillas" ,main="No tocan el borde", names.arg=tt2[,2], col=rainbow(length(qv[ fil==FALSE])/2)   )

dev.off()

