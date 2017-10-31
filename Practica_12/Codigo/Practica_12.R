##############################################################################
###                          Practica 12. REd Neuronal                      ##
###                                                                         ##
###                     28/10/2017                                          ##
###                                                                         ##
##############################################################################

setwd("C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_12/Graficas")
#file.remove(list.files(pattern=".png")) 
#rm(list=ls())
#gc(TRUE)
library(parallel)
library(magick)












binario <- function(d, l) {
  b <- rep(FALSE, l)
  while (l > 0 | d > 0) {
    b[l] <- (d %% 2 == 1)
    l <- l - 1
    d <- bitwShiftR(d, 1)
  }
  return(b)
}

decimal <- function(bits, l) {
  valor <- 0
  for (pos in 1:l) {
    valor <- valor + 2^(l - pos) * bits[pos]
  }
  return(valor)
}



setwd("C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_12/Graficas")
ww="C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_12/Graficas/digitos.modelo.csv"

modelos<-read.csv (ww, header=FALSE, stringsAsFactors=F)


 
modelos[modelos=='n'] <- 90.95
modelos[modelos=='g'] <- 0.92
modelos[modelos=='b'] <- 0.002
r <-5
c <- 3
dim <- r * c

tasa <- 0.15
tranqui <- 0.99

tope <- 18
digitos <- 0:tope
k <- length(digitos)
contadores <- matrix(rep(0, k*(k+1)), nrow=k, ncol=(k+1))
rownames(contadores) <- 0:tope
colnames(contadores) <- c(0:tope, NA)

n <- floor(log(k-1, 2)) + 1
neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim) # perceptrones

dat<-list()

prueba<-c(300)
#prueba<-c(3,5)

cluster <- makeCluster(detectCores() - 1)

dat14<-data.frame()
for ( h in 1:5){ 
  tinicial<-0
  tinicial<-Sys.time()
 
  
  for (t in 1:5000) { # entrenamiento
  d <- sample(0:tope, 1)
  pixeles <- runif(dim) < modelos[d + 1,]
  correcto <- binario(d, n)
  for (i in 1:n) {
    w <- neuronas[i,]
    deseada <- correcto[i]
    resultado <- sum(w * pixeles) >= 0
    if (deseada != resultado) {
      ajuste <- tasa * (deseada - resultado)
      tasa <- tranqui * tasa
      neuronas[i,] <- w + ajuste * pixeles
    }
  }
}

clusterExport(cluster, c("neuronas", "n", "decimal", "k", "dim", "contadores", "modelos", "resultado","binario",  "deseada","tope"))

 #for ( kt in prueba) { 
  # clusterExport(cluster,"kt")
 ti<- parSapply(cluster, 1:300, function(t1) {
 
    d <- sample(0:tope, 1)
    pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
    correcto <- decimal(binario(d, n),n)
    salida <- rep(FALSE, n)
 #clusterExport(cluster, c("pixeles","correcto","salida", "d","n","neuronas", "contadores"))
  for ( i in 1:n){  
  w <- neuronas[i,]
    deseada <- correcto[i]
    resultado <- sum(w * pixeles) >= 0
    salida[i] <- resultado
     }
   
  r <- min(decimal(salida, n), k) # todos los no-existentes van al final
 # contadores[d+1, r+1] <- contadores[d+1, r+1] + 1
  return(r==correcto)
})
 
  ag<-as.numeric(ti)
  aciertos<-sum(ag)/300
  tfinal<-Sys.time()

dat14<-rbind(dat14,c(h,1, aciertos))

}




stopCluster(cluster)
dat14




dattime$p<-"Paralelo"
dats$s<-"Secuencial"


colnames(dattime)<-c("Replica","Prueba","Porcentaje", "Tiempo", "tipo")
colnames(dats)<-c("Replica","Prueba","Porcentaje", "Tiempo", "tipo")


write.csv(dattime, "datParel.csv")

################################################ Comparación de tiempos ##########################

fin<-rbind(dattime, dats)

      for ( j in prueba){ 
        df<-subset(fin, fin$Prueba==j)
        png(paste("p12_tiempo_", j,   ".png", sep="") ,width=500, height=500)
        boxplot(df$Tiempo~df$tipo, ylab="Tiempo (s)", xlab="Estrategia", col=rainbow(2), main=( paste("Prueba ", j, sep="" ) ))
        graphics.off()
        }



library(magick)
frames=lapply(prueba,function(i) image_read(paste("p12_tiempo_",i,".png",sep="")))
animation <- image_animate(image_join(frames),fps=1)
print(animation)
w3<-paste("P12Tiempos", ".gif",sep=""  )
image_write(animation, w3 )




colnames(dat14)<-c("replicas", "f", "Prueba", "Porcentaje")

dat14$replicas<-3

st<-rbind(dat12,dat13,dat14)

png(paste("p12_R1", ".png", sep="") ,width=800, height=500)
boxplot(st[,4]*100~st[,1], col=topo.colors(3), ylab="Porcentaje", xlab="Cambios")
graphics.off()


kruskal.test(fin$tipo~fin$Tiempo)

setwd("C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_12/Graficas")
png(paste("p12_R1B", ".png", sep="") ,width=800, height=500)
boxplot(fin$Tiempo~fin$tipo, col=c(2,5), ylab="Tiempos (s)", xlab="Estrategia")
graphics.off()
############################## Matriz de confución #########


# 
# 
# dat1<-data.frame()
# for( j in 1: h){ 
#     dat1<-rbind(dat1, dat[[j]])
#     }
# dat1<-round(dat1[,1:11]/apply(dat1[,1:11], 1, sum),2)
# dat1$iter<-rep(seq(from=0, to=9),h)
# 
# aux1<-vector()
# for ( i in 0:9){
#     cero<-subset(dat1, dat1[,12]==i)      ### seleccion un conjunto 
#     aux<-t(apply(cero[,1:10], 2, mean))   ### Determino el promedio del conjunto 
#     aux1[i+1]<-aux[1,i+1]
#   }
# rf<-0:9
# 
# barplot(aux1*100, xlab="Números",ylim=c(0,110), ylab="Porcentaje promedio", names.arg=c(0:9), col=rainbow(10))
# plot(rf,aux1*100, xlim=c(0,10.5), type="l",lty=3,xlab="Números", main="", ylab="Porcentaje promedio", col="blue")
# 
# for ( i in 0:9){ 
#   cero<-subset(dat1, dat1[,12]==i)
#   png(paste("p12_", i,   ".png", sep="") ,width=800, height=500)
#   boxplot(cero[,1:11]*100, ylim=c(0,100), main=paste("Número " ,i, sep=""), xlab="Número", ylab="Porcentaje", col=rainbow(10))
#   graphics.off()
# }
# 
# 
# library(magick)
# frames=lapply(0:9,function(i) image_read(paste("p12_",i,".png",sep="")))
# animation <- image_animate(image_join(frames),fps=1)
# print(animation)
# w3<-paste("P12R", ".gif",sep=""  )
# image_write(animation, w3 )
# 
# 
# 
# for ( i in 0:(bq-1) ){
# plot(c(0:10), (contadores[i+1,]/sum(contadores[i+1,]))*100 , xlim=c(0,12), type="l", xlab="Número", ylab="Porcentaje", ylim = c(0,100), main=paste("Número ",i , sep="") )
#   q<-which.max((contadores[i+1,]/sum(contadores[i+1,])))  #   x
# qw<-max(contadores[i+1,]/sum(contadores[i+1,]))*100     #   y
# points( y=qw, x=(q-1), col="red", pch=16) 
# abline(v=i,col=3)
# legend(q, qw+10, round(qw,2), bty= "n")
# }
# 
