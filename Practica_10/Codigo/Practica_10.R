##############################################################################
###                          Practica 10. Algoritmo genetico                ##
###                                                                         ##
###                     16/10/2017                                          ##
###                                                                         ##
##############################################################################

setwd("C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_10/Graficas")
#file.remove(list.files(pattern=".png")) 
rm(list=ls())
gc(TRUE)
library(parallel)




library(testit)

knapsack <- function(cap, peso, valor) {
  n <- length(peso)
  pt <- sum(peso) 
  assert(n == length(valor))
  vt <- sum(valor) 
  if (pt < cap) { 
    return(vt)
  } else {
    filas <- cap + 1 
    cols <- n + 1 
    tabla <- matrix(rep(-Inf, filas * cols),
                    nrow = filas, ncol = cols) 
    for (fila in 1:filas) {
      tabla[fila, 1] <- 0 
    }
    rownames(tabla) <- 0:cap 
    colnames(tabla) <- c(0, valor) 
    for (objeto in 1:n) { 
      for (acum in 1:(cap+1)) { # consideramos cada fila de la tabla
        anterior <- acum - peso[objeto]
        if (anterior > 0) { # si conocemos una combinacion con ese peso
          tabla[acum, objeto + 1] <- max(tabla[acum, objeto], tabla[anterior, objeto] + valor[objeto])
        }
      }
    }
    return(max(tabla))
  }
}

factible <- function(seleccion, pesos, capacidad) {
  return(sum(seleccion * pesos) <= capacidad)
}

objetivo <- function(seleccion, valores) {
  return(sum(seleccion * valores))
}

normalizar <- function(data) {
  menor <- min(data)
  mayor <- max(data)
  rango <- mayor - menor
  data <- data - menor # > 0
  return(data / rango) # entre 0 y 1
}

generador.pesos <- function(cuantos, min, max) {
  return(sort(round(normalizar(rnorm(cuantos)) * (max - min) + min)))
}

generador.valores <- function(pesos, min, max) {
  n <- length(pesos)
  valores <- double()
  for (i in 1:n) {
    media <- pesos[n]
    desv <- runif(1)
    valores <- c(valores, rnorm(1, media, desv))
  }
  valores <- normalizar(valores) * (max - min) + min
  return(valores)
}

poblacion.inicial <- function(n, tam) {
  pobl <- matrix(rep(FALSE, tam * n), nrow = tam, ncol = n)
  for (i in 1:tam) {
    pobl[i,] <- round(runif(n))
  }
  return(as.data.frame(pobl))
}

mutacion <- function(sol, n) {
  pos <- sample(1:n, 1)
  mut <- sol
  mut[pos] <- (!sol[pos]) * 1
  return(mut)
}

reproduccion <- function(x, y, n) {
  pos <- sample(2:(n-1), 1)
  xy <- c(x[1:pos], y[(pos+1):n])
  yx <- c(y[1:pos], x[(pos+1):n])
  return(c(xy, yx))
}

mut1<-function(i){  

  if (runif(1) < pm) {
    p2 <- mutacion(p[i,], n)
    return(p2)
  }
  return(p[i,])
}


n <- 50
pesos <- generador.pesos(n, 15, 80)
valores <- generador.valores(pesos, 10, 500)
capacidad <- round(sum(pesos) * 0.65)
optimo <- knapsack(capacidad, pesos, valores)
init <- c(200,400,800,1600)


#assert(tam == init)
pm <- 0.05
rep <- 30

tmax <- c(50,100,200,400,800,1600)
mejores <- double()

cluster <- makeCluster(detectCores() - 1)
#clusterExport(cluster, "tam")
clusterExport(cluster, c("n", "pesos", "capacidad"))
clusterExport(cluster, c( "reproduccion",  "factible", "valores"))
clusterExport(cluster, c("mutacion", "tmax"))
clusterExport(cluster, c("rep","pm", "init", "mut1" , "objetivo"))


dat2<-data.frame()

for( init in init){
  
  p <- poblacion.inicial(n, init)
  tam <- dim(p)[1]
  prob1<-rep(1/init,init)
  clusterExport(cluster, c("p","prob1"))
  k1<-floor(init)*0.1
  
for (iter in tmax) {
  for ( rep1 in 1:30){
  tinicial<-Sys.time()
  clusterExport(cluster, c("prob1"))
  p$obj <- NULL
  p$fact <- NULL
  
  mut<-parSapply(cluster, 1:tam, mut1)  
  p<-data.frame(matrix(unlist(mut), ncol=n))
  
  tam <- dim(p)[1]
  
  clusterExport(cluster, c("p","tam"))
  
  
  p1<-parSapply(cluster, 1:rep, function(i) { # una cantidad fija de reproducciones
    padres <- sample(1:tam, 2, replace=FALSE, prob=prob1)
    hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
    
    return(hijos)
  })
  
  aq<-data.frame(matrix(unlist(p1), ncol=n    ))
  p<-rbind(p,aq)
  
  tam <- dim(p)[1]
  obj <- double()
  fact <- integer()
  
  clusterExport(cluster, c("p", "tam"))
  
  
  objaux<-parSapply(cluster, 1:tam, function(i) {   
    obj <- objetivo(unlist(p[i,]), valores)
    return(obj)
  })
  
  factaux<-parSapply(cluster, 1:tam, function(i) {   
    fact <- factible(unlist(p[i,]), pesos, capacidad)
    return(fact)
  })
  
  obj<-objaux
  fact<-factaux
  
  
  p <- cbind(p, obj)
  p <- cbind(p, fact)
  
  #mantener <- order(-p[, (n + 2)], -p[, (n + 1)])[1:init]

  
  
   
  # w2<-order(p$obj)
  # length(w2)
  # wt<-w2[1:(floor(length(w2)*0.9)) ]
  # 
  # w3<-order(p$fact, decreasing=FALSE)
  # wt2<-w3[1:(init-length(wt))]
  # length(c(wt,wt2))
  
 
  
  prob1<-c()
  
  for ( i in 1: tam){  
    if(p$fact[i]==TRUE){
      prob1 <- c(prob1, p$obj[i]/sum(p$obj))
      }
    else{
      prob1 <- c(prob1, (p$obj[i]/sum(p$obj))*0.55)
    }
      
  }
  
  mantener<-sample(1:tam, size=k1, replace=FALSE, prob = prob1)
  
  p <- p[mantener,]
  tam <- dim(p)[1]
  #assert(tam == init)
  factibles <- p[p$fact == TRUE,]
  mejor <- max(factibles$obj)
  mejores <- c(mejores, mejor)
  prob1<-c()
  
  
  
  
  tfinal<-Sys.time()
  aux1<-((optimo - mejor) / optimo) 
  dat2<-rbind(dat2, cbind(iter, "R1", init, rep1, aux1, mejor,  tfinal-tinicial))
  
  }
 }
}
stopCluster(cluster)




 write.csv(dat2, "reto1Bc.csv")


 ww="C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_10/Graficas/reto1.csv"
 reto1=read.csv (ww, header=T)


 ww="C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_10/Graficas/reto1Bc.csv"
 reto1Bc=read.csv (ww, header=T)



 reto1$V2<-"Anterior"
 reto1Bc$V2<-"Supervivencia"

 pq<-rbind(reto1,reto1Bc)
 lineal<-aov(mejor~V2+init+iter,data = pq)
 summary(lineal)
 shapiro.test(resid(lineal))


 wilcox.test(mejor~V2,data = pq)




  maxd=ceiling(max(as.numeric(pq$mejor)))
  minxd=floor(min(as.numeric(pq$mejor)))
  setwd("C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_10/Graficas")





  for ( i in tmax){
  png(paste("P10_R2", i, ".png", sep=""), width=500, height=500)
  aq<-subset(pq, iter==i)
  boxplot(as.numeric(aq$mejor)~as.factor(aq$V2), ylim=c(minxd,maxd), col=cm.colors(2),
          dat=aq,ylab="Función Objetivo", main=paste("Generaciones ",i, sep = ""))
          graphics.off()
 
  }
 
 
  library(magick)
 
  frames=lapply(tmax,function(tmax) image_read(paste("P10_R2",tmax,".png",sep="")))
  animation <- image_animate(image_join(frames),fps=2)
  print(animation)
  w3<-paste("P10R2", ".gif",sep=""  )
  image_write(animation, w3 )






# library(lattice)
# z=outer(as.numeric(dat1$iter),as.numeric(dat1$init))
# 
# x=dat1$iter
# z=dat1$init
# y=dat1$aux1
# 
# angles <- seq(from=1,to=360,by=1)
# for(i in 1:length(angles)){
#   
#   png(paste("superfice_",i, ".png", sep=""  ),width=500, height=500)
# print(wireframe(y ~ x + z,
#          
#           scales = list(arrows=FALSE,cex=.5,tick.number = 10, z = list(arrows=T)),
#          screen=list(z=angles[i],x=-60), 
#           xlab="iter",
#           ylab="init",zlab="GAP",main="",
#           light.source = c(10,10,10), drape=T,
#           col.regions = rainbow(100, s = 1, v = 1, start = 0, end = max(1,100 - 1)/100, alpha = 1)))
# 
# graphics.off()
#   }
# 
# 
# 
# 
# 
# library(magick)
# 
# frames=lapply(angles,function(angles) image_read(paste("Superfice_",angles,".png",sep="")))
# animation <- image_animate(image_join(frames),fps=25)
# print(animation)
# w3<-paste("Superficie", ".gif",sep=""  )
# image_write(animation, w3 )
# 
# 
# 
# 
# 
# ww="C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_10/Graficas/secuencial.csv"
# secuencial=read.csv (ww, header=T)
# 
# secuencial$X<-NULL
# 
# datf<-rbind(secuencial,dat1)
# datf
# 
# maxd=ceiling(max(as.numeric(datf$time)))
# setwd("C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_10/Graficas")
# 
# for ( i in tmax){
# png(paste("P10_", i, ".png", sep=""), width=500, height=500)
# aq<-subset(datf, tmax==i)
# boxplot(as.numeric(aq$time)~as.factor(aq$metodo), ylim=c(0,maxd), col=terrain.colors(2), 
#         dat=aq,ylab="Tiempo (s)", main=paste("Generaciones ",i, sep = ""))
#         graphics.off()
# 
# }
# 
# 
# library(magick)
# 
# frames=lapply(tmax,function(tmax) image_read(paste("P10_",tmax,".png",sep="")))
# animation <- image_animate(image_join(frames),fps=2)
# print(animation)
# w3<-paste("P10", ".gif",sep=""  )
# image_write(animation, w3 )
# 
# 
# 
# 

