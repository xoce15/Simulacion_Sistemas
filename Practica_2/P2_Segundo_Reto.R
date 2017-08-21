##############################################################################
###                          Practica 2. Autómatas celular                  ##
###                         Segundo  Reto                                   ##
###                     14/08/2017                                          ##
###                                                                         ##
##############################################################################


library(sna)
library(magick)
library(parallel)


dim <- 10
num <-  dim^2
proba<-0.1
proba2<-0.01


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

nucleos<-c(1,2,3,4)



actual<- matrix(0,  nrow=dim, ncol=dim)
for ( i in 1: length(nucleos)){
  actual[(round((dim)*runif(1))),(round((dim)*runif(1)))]=nucleos[i]  }
salida = paste("p2_t", 0, ".png", sep="")
png(salida)
plot.sociomatrix(actual, diaglab=FALSE, main="Paso 0")
graphics.off()



for (iteracion in 1:12) {
  clusterExport(cluster, "actual")  
  siguiente <- parSapply(cluster, 1:num, paso)  
  
  actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
  if( runif(1)<proba2){
    nucleos<-c(nucleos,length(nucleos)+1)
    return(actual[fila,columna]=round((length(nucleos))*runif(1))) 
  }
  
  
  
  salida = paste("p2_t", iteracion, ".png", sep="")
  tiempo = paste("Paso", iteracion)
  png(salida)
  plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
  graphics.off()
} 

stopCluster(cluster)

frames=lapply(0:iteracion,function(x) image_read(paste("p2_t",x,".png",sep="")))
animation <- image_animate(image_join(frames))
print(animation)
image_write(animation, "R1_crecimiento.gif")
sapply(0:iteracion,function(x) file.remove(paste("p2_t",x,".png",sep="")))

#  file.remove(list.files(pattern=".png"))      
