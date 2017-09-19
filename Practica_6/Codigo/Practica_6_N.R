##############################################################################
###                Practica 6. Sistema multiagente                          ##
###                         Normal                                          ##
###                     12/09/2017                                          ##
###                                                                         ##
##############################################################################
setwd("C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_6/Graficas")
#file.remove(list.files(pattern=".png")) 
rm(list=ls())  
library(parallel)

l <- 1.5
n <- 50
pi <- 0.05
pr <- 0.02
v <- l / 30
S = 2
I = 3
R = 4

initial = c(S, I)
#initial= c(S,I,R)
p = 0.2
#p1= 0.1
iprobs = c(1-p, p)
#iprobs = c(1-p-p1,p,p1)

agentes = data.frame(x = runif(n, 0, l), y = runif(n, 0, l), dx = runif(n, -v, v), 
                     dy = runif(n, -v, v), estado = sample(initial, n, replace=TRUE, iprobs))


epidemia <- integer()
r <- 0.1
tmax <- 10
digitos <- floor(log(tmax, 10)) + 1



cluster <- makeCluster(detectCores() - 1)
#clusterExport(cluster, "l")
#cluster <- makeCluster(detectCores() - 1)
#clusterExport(cluster, "contagios")
clusterExport(cluster, "agentes")
clusterExport(cluster, "l")
clusterExport(cluster, "r")
clusterExport(cluster, "pr")

data1<-data.frame(matrix(vector(), 0,2  ))
tinicial<-Sys.time()
nt<-c(50,10,20,30)


for ( k in 1:5){
#for ( n in c(50,10,20,30) ){ 
#clusterExport(cluster, "n") 
  
for (tiempo in 1:tmax) {
  infectados <- dim(agentes[agentes$estado == 3,])[1]
  epidemia <- c(epidemia, infectados)
  if (infectados == 0) {
    break
  }
  
 # contagios <- rep(FALSE, n)
  contagios<-parSapply(cluster, 1:n,function(i) {  
    
    if(agentes$estado[i]==3){ 
      return(FALSE)} 
    aux1<-which(agentes$estado==3)
    for ( j in  aux1 ) { 
      dx<-agentes$x[i]-agentes$x[j]
      dy<-agentes$y[i]-agentes$y[j]
      d <- sqrt(dx^2 + dy^2)
      p <- (r - d) / r
      if (runif(1) < p & d<r) {
        return(TRUE)
              }
    }
    return(FALSE)
  }
  )
  
  clusterExport(cluster, "contagios")
  clusterExport(cluster, "agentes")
  
  ac<-parSapply(cluster, 1:n, function(i){     #movimientos y actualizaciones
    a <- agentes[i, ]
    if (contagios[i]) {
      a$estado <- 3
    } else if (a$estado == 3) { # ya estaba infectado
      if (runif(1) < pr) {
        a$estado <- 4 # recupera
      }
    }
    a$x <- a$x + a$dx
    a$y <- a$y + a$dy
    if (a$x > l) {
      a$x <- a$x - l
    }
    if (a$y > l) {
      a$y <- a$y - l
    }
    if (a$x < 0) {
      a$x <- a$x + l
    }
    if (a$y < 0) {
      a$y <- a$y + l
    }
    #a$y<-a$y-floor(a$y)
    
    return(a)
  })
  aux <-unlist(t(ac))
  agentes<-data.frame(matrix(aux, ncol=5, nrow=n) )
  colnames(agentes)=c("x", "y", "dx", "dy","estado" ) 
  
  
  aS <- agentes[agentes$estado == 2,]
  aI <- agentes[agentes$estado == 3,]
  aR <- agentes[agentes$estado == 4,]
  tl <- paste(tiempo, "", sep="")
  tfinal<-Sys.time()
  data1<-rbind(data1,cbind(tfinal-tinicial,n))
}
}
 

#} 
stopCluster(cluster)
#}
  
 
  
  
  
  
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  salida <- paste("p6_t", tl, ".png", sep="")
  tiempo <- paste("Paso", tiempo)
  png(salida)
  plot(l, type="n", main=tiempo, xlim=c(0, l), ylim=c(0, l), xlab="x", ylab="y")
  if (dim(aS)[1] > 0) {
    points(aS$x, aS$y, pch=15, col="chartreuse3", bg="chartreuse3")
  }
  if (dim(aI)[1] > 0) {
    points(aI$x, aI$y, pch=16, col="firebrick2", bg="firebrick2")
  }
  if (dim(aR)[1] > 0) {
    points(aR$x, aR$y, pch=17, col="goldenrod", bg="goldenrod")
  }
  graphics.off()
}
png("p6e.png", width=600, height=300)
plot(1:length(epidemia), 100 * epidemia / n, xlab="Tiempo", ylab="Porcentahe de infectados")
graphics.off()











library(RColorBrewer) ## Libreria de colores 
library(ggplot2)
library(parallel)
library(magick)

setwd("C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_6")
ab<-seq(0:99)
ab1<-ifelse(ab<10,paste("00",ab, sep = ""),ifelse(ab<100,paste("0",ab, sep = ""),ab ))
frames=lapply(1:100,function(x) image_read(paste("p6_t",ab1[x],".png",sep="")))
animation <- image_animate(image_join(frames),fps=20 )
print(animation)
w3<-paste("P6_N", ".gif"  )
image_write(animation, w3)