##############################################################################
###                          Practica 7. Busqueda Local                     ##
###                                                                         ##
###                     23/09/2017                                          ##
###                                                                         ##
##############################################################################

setwd("C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_7/Graficas")
#file.remove(list.files(pattern=".png")) 
rm(list=ls())  
library(parallel)

library(rugarch)
library(rgl)
library(fGarch)
requiere(rgl)


g <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}

x <- seq(-6, 5, 0.25)
y<-x



low <- -6
high <- 5
t <- 100

step <- 0.1


replica <- function(j) {
  x <- runif(1, low, high)
  y <- runif(1, low, high)
  bestpos <- c(x, y)
  bestval <- g(x, y)
  trayectoria = c(bestval)
  camino = c(x, y)
  
  for (tiempo in 1:t) {
    d <- runif(1, 0, step)
    op = c(x - d, y, x + d, y, x, y - d, x, y + d)
    posibles = numeric() 
    for (i in 1:4) { 
      posibles <- c(posibles, g(op[2*i - 1], op[2*i]))
    }
    mejor <- which.max(posibles)
    nuevo = posibles[mejor] 
    x<-op[2*mejor - 1]
    y<-op[2*mejor]
    if((x<= high & y <= high) & ( x>= low & y>= low)){ 
      
      if (nuevo > bestval) { # minimizamos
        bestpos <- c(op[(2*mejor - 1) ],op[2*mejor])
        bestval <- nuevo
      }
      
      trayectoria <- c(trayectoria, bestval)
    }else { 
      x <- runif(1, low, high)
      y <- runif(1, low, high)
    }
    camino = c(camino, x, y)
  }
  return(camino) 
}




replicas<-10
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "replica")
clusterExport(cluster, "g")
clusterExport(cluster, "high")
clusterExport(cluster, "low")
clusterExport(cluster, "step")
clusterExport(cluster, "t")
dat<-data.frame()
resultados <-parSapply(cluster, 1:replicas, replica)
dat<-rbind(dat,t(resultados))
stopCluster(cluster)

######################### GRAFICA ##########################################################



setwd("C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_7/Graficas")

r<-0:t
r1<-ifelse(r<10,paste("00",r, sep = ""),ifelse(r<100,paste(0,r, sep="" ),t))
z <- outer(x, y, g)



for (i in 0:t) { 
  png(paste("Practica7_",i,".png", sep = ""),width=500, height=500)
 image(x,y,z, col=heat.colors(20)[20:1], main=paste("Iteracción ",i,sep=""   ))
 contour(x,x,z,add=T, xlim=c(low,high),ylim=c(low,high))
  for (j in 1:replicas){
    po = dat[j,]
    points(po[2*(i + 1) - 1], po[2*(i + 1)], col=j, pch=16, cex=1)
  
  }
  graphics.off()
}


library(magick)

frames=lapply(1:t,function(w) image_read(paste("Practica7_",w,".png",sep="")))
animation <- image_animate(image_join(frames),fps=4)
print(animation)
w3<-paste("P7_", replicas,".gif",sep=""  )
image_write(animation, w3 )
sapply(1:t,function(x) file.remove(paste("Practica7_",x,".png",sep="")))



##############################################################################
###                          Practica 7. Busqueda Local                     ##
###                                Segundo Reto                             ##
###                     23/09/2017                                          ##
###                                                                         ##
##############################################################################

setwd("C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_7/Graficas")
#file.remove(list.files(pattern=".png")) 
rm(list=ls())  
library(parallel)

library(rugarch)
library(rgl)
library(fGarch)
requiere(rgl)


g <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}

x <- seq(-6, 5, 0.25)
y<-x



low <- -6
high <- 5
t <- 100

step <- 0.1
tempe<-100

replica <- function(j) {
  x <- runif(1, low, high)
  y <- runif(1, low, high)
  bestpos <- c(x, y)
  bestval <- g(x, y)
  trayectoria = c(bestval)
  camino = c(x, y)
  
  for (tiempo in 1:t) {
    d <- runif(1, 0, step)
    op = c(x - d, y, x + d, y, x, y - d, x, y + d)
    posibles = numeric() 
    for (i in 1:4) { 
      posibles <- c(posibles, g(op[2*i - 1], op[2*i]))
    }
    
    #posibles<-c(2,2.2,3.5,4.8,5)
    aux<-floor(runif(1, min=1,max=length(posibles)))
    aux1<-posibles[aux]
    
    if(aux1==max(posibles)){
    mejor<-posibles[posibles==aux1]
    }else if (runif(1)>exp(-mejor/tempe)) { 
    mejor<-posibles[posibles==aux1]
      tempe<-tempe*0.01
    } 
    
    nuevo = posibles[mejor]
   
     
    x<-op[2*mejor - 1]
    y<-op[2*mejor]
    if((x<= high & y <= high) & ( x>= low & y>= low)){ 
      
      if (nuevo > bestval) { # minimizamos
        bestpos <- c(op[(2*mejor - 1) ],op[2*mejor])
        bestval <- nuevo
        
        
      }
      
      trayectoria <- c(trayectoria, bestval)
    }else { 
      x <- runif(1, low, high)
      y <- runif(1, low, high)
    }
    camino = c(camino, x, y)
  }
  return(camino) 
}




replicas<-10
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "replica")
clusterExport(cluster, "g")
clusterExport(cluster, "high")
clusterExport(cluster, "low")
clusterExport(cluster, "step")
clusterExport(cluster, "t")
dat<-data.frame()
resultados <-parSapply(cluster, 1:replicas, replica)
dat<-rbind(dat,t(resultados))
stopCluster(cluster)

######################### GRAFICA ##########################################################



setwd("C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_7/Graficas")

r<-0:t
r1<-ifelse(r<10,paste("00",r, sep = ""),ifelse(r<100,paste(0,r, sep="" ),t))
z <- outer(x, y, g)



for (i in 0:t) { 
  png(paste("Practica7_",i,".png", sep = ""),width=500, height=500)
  image(x,y,z, col=heat.colors(20)[20:1], main=paste("Iteracción ",i,sep=""   ))
  contour(x,x,z,add=T, xlim=c(low,high),ylim=c(low,high))
  for (j in 1:replicas){
    po = dat[j,]
    points(po[2*(i + 1) - 1], po[2*(i + 1)], col=j, pch=16, cex=1)
    
  }
  graphics.off()
}


library(magick)

frames=lapply(1:t,function(w) image_read(paste("Practica7_",w,".png",sep="")))
animation <- image_animate(image_join(frames),fps=4)
print(animation)
w3<-paste("P7_", replicas,".gif",sep=""  )
image_write(animation, w3 )
sapply(1:t,function(x) file.remove(paste("Practica7_",x,".png",sep="")))





