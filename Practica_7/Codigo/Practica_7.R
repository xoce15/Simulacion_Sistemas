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

#library(rugarch)
library(rgl)
library(fGarch)



g <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}

x <- seq(-6, 5, 0.25)
y<-x



low <- -6
high <- 5


step <- 0.1
tempe<-12

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
      
    trayectoria <- c(trayectoria, nuevo)
    }else { 
      x <- runif(1, low, high)
      y <- runif(1, low, high)
    }
    camino = c(camino, x, y)
   
  }
  return(camino) 
}




t <- 300
replicas<-20
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "replica")
clusterExport(cluster, "g")
clusterExport(cluster, "high")
clusterExport(cluster, "low")
clusterExport(cluster, "tempe")
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
w3<-paste("P7_", t,".gif",sep=""  )
image_write(animation, w3 )
sapply(1:t,function(x) file.remove(paste("Practica7_",x,".png",sep="")))











png("superficie.png", )
persp(x,y,z, shade=0.2, col='orange', theta=40, phi=30)
graphics.off()












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

library(rgl)


g <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}

x <- seq(-6, 5, 0.25)
y<-x



low <- -6
high <- 5
t <- 100

step <- 0.1
tempe<-12

replica <- function(j) {
  x <- runif(1, low, high)
  y <- runif(1, low, high)
  bestpos <- c(x, y)
  bestval <- g(x, y)
  trayectoria = c(bestval)
  camino = c(x, y)
  actual<-c(x,y)
  mejor<-bestval
  for (tiempo in 1:t) {
    d <- runif(1, 0, step)
    op = c( max(x - d,low), y, min(x + d, high), y, x,max(y - d,low) , x,min( y + d, high))
    posibles = numeric() 
    for (i in 1:4) { 
      posibles <- c(posibles, g(op[2*i - 1], op[2*i]))
    }
    
    
    aux<-floor(runif(1, min=1,max=length(posibles)))
    aux1<-posibles[aux]
    
    delta<-aux1-g(actual[1],actual[2] )
    
    if(delta>0){
      mejor<-posibles[posibles==aux1]
      actual<- c(op[2*mejor - 1],op[2*mejor])
      
    } else if ( runif(1)<exp(delta/tempe) ) { 
      actual<-c(op[2*aux - 1],op[2*aux])
      tempe<-tempe*0.995
    }
   
    
    nuevo = posibles[mejor]
   
     
    x<-actual[1]
    y<-actual[2]
     
      
      trayectoria <- c(trayectoria, bestval)
      camino = c(camino, x, y )
  }
  return(camino) 
}




replicas<-20
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "replica")
clusterExport(cluster, "g")
clusterExport(cluster, "high")
clusterExport(cluster, "low")
clusterExport(cluster, "step")
clusterExport(cluster, "t")
clusterExport(cluster, "tempe")

dat<-data.frame()
resultados <-parSapply(cluster, 1:replicas, replica)
dat<-rbind(dat,t(resultados)  ) 
stopCluster(cluster)

t=100
dat1<-resultados[resultados>6]
plot(dat1[1:100],col=1, ylab="Temperatura", xlab="Itera * replica", xlim=c(0, t*replicas), type="l", ylim=c(min(dat1), max(dat1)) )

for(i in 1:4 ){ }

xa<-101:200 
lines(dat1[101:200]~xa , col=2 )


xa<-201:300 
lines(dat1[201:300]~xa , col=3 )
xa<-301:400 
lines(dat1[301:400]~xa , col=4 )
xa<-401:500 
lines(dat1[401:500]~xa , col=5 )








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





