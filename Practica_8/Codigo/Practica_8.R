##############################################################################
###                          Practica 8. Modelo de urnas                    ##
###                                                                         ##
###                     27/09/2017                                          ##
###                                                                         ##
##############################################################################

setwd("C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_8/Graficas")
#file.remove(list.files(pattern=".png")) 
rm(list=ls())
gc(TRUE)
library(parallel)
library(testit) # para pruebas, recuerda instalar antes de usar
######################### Funciones utilizadas    ##################################################
rotura <- function(x) {
  return (1 / (1 + exp((c - x) / d)))
}

union <- function(x) {
  return (exp(-x / c))
}

romperse <- function(tam,cuantos) {

  romper <- round(rotura(tam) * cuantos) # independientes
  resultado <- rep(tam, cuantos - romper) # los demas
  if (romper > 0) {
    for (cumulo in 1:romper) { # agregar las rotas
      t <- 1
      if (tam > 2) { # sample no jala con un solo valor
        t <- sample(1:(tam-1), 1)
      }
      resultado <- c(resultado, t, tam - t)
    }
  }
  assert(sum(resultado) == tam * cuantos) # no hubo perdidas
  return(resultado)
}
unirse <- function(i) {
  urna<-freq[i,]
  tam<-urna$tam
  cuantos<-urna$num

   unir <- round(union(tam) * cuantos) # independientes
  if (unir > 0) {
    division <- c(rep(-tam, unir), rep(tam, cuantos - unir))
    assert(sum(abs(division)) == tam * cuantos)
    return(division)
  } else {
    return(rep(tam, cuantos))
  }
}

######################################## ############################################
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "romperse")
clusterExport(cluster, "rotura")
clusterExport(cluster, "assert")
clusterExport(cluster, "unirse")
clusterExport(cluster, "union")
duracion <- 30
k1 <- c(10000, 15000,30000,90000)
#k1<-c(2,3,4,5)
n1 <- 30*k
dat<-data.frame()

for ( k in k1) {
  n1 <- 30*k
for ( n in n1) { 
 
  tinicial<-Sys.time()
  
  originales <- rnorm(k)
  print(k)
  cumulos <- originales - min(originales) + 1
  cumulos <- round(n * cumulos / sum(cumulos))
  assert(min(cumulos) > 0)
  diferencia <- n - sum(cumulos)
  if (diferencia > 0) {
    for (i in 1:diferencia) {
      p <- sample(1:k, 1)
      cumulos[p] <- cumulos[p] + 1
    }
  } else if (diferencia < 0) {
    for (i in 1:-diferencia) {
      p <- sample(1:k, 1)
      if (cumulos[p] > 1) {
        cumulos[p] <- cumulos[p] - 1
      }
    }
  }
  assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
  assert(sum(cumulos) == n)
#  print("A")
  c <- median(cumulos) # tamanio critico de cumulos
  d <- sd(cumulos) / 4 # factor arbitrario para suavizar la curva
  freq <- as.data.frame(table(cumulos))
  names(freq) <- c("tam", "num")
  freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
  digitos <- floor(log(duracion, 10)) + 1
  
  clusterExport(cluster, "c")
  clusterExport(cluster, "d")
  clusterExport(cluster, "freq")
 #  clusterExport(cluster, "cumulos")
for (paso in 1:duracion) {
  clusterExport(cluster, "freq")
 
   assert(sum(cumulos) == n)
  cumulos <- integer()
# print("B")

  farotura<-parSapply(cluster, 1:dim(freq)[1], function(i) {
    urna <- freq[i,]
    if (urna$tam > 1) { # no tiene caso romper si no se puede
      cumulos <- romperse(urna$tam, urna$num)
    } else {
      cumulos <- rep(1, urna$num)
    }
 return(cumulos) 
  })
  cumulos<- unlist(farotura)
  assert(sum(cumulos) == n)
  # print("C")
  assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
  freq <- as.data.frame(table(cumulos)) # actualizar urnas
  names(freq) <- c("tam", "num")
  freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
# print("C1")
  assert(sum(freq$num * freq$tam) == n)
  cumulos <- integer()
  clusterExport(cluster, "freq")
  faunion<-parSapply(cluster, 1:dim(freq)[1], function(i) { # fase de union
  cumulos <- unirse(i)
   return(cumulos)
        })
  
#  stopCluster(cluster)
  cumulos<-unlist(faunion)
#  print("D1")
  assert(sum(abs(cumulos)) == n)
  assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
  juntarse <- -cumulos[cumulos < 0]
  cumulos <- cumulos[cumulos > 0]
  assert(sum(cumulos) + sum(juntarse) == n)
  nt <- length(juntarse)
# print("D2")
   if (nt > 0) {
    if (nt > 1) {
      juntarse <- sample(juntarse)
      for (i in 1:floor(nt / 2) ) {
        cumulos <- c(cumulos, juntarse[2*i-1] + juntarse[2*i])
      }
    }
    if (nt %% 2 == 1) {
      cumulos <- c(cumulos, juntarse[nt])
    }
  }
  assert(sum(cumulos) == n)
 # print("E")
  
  freq <- as.data.frame(table(cumulos))
  names(freq) <- c("tam", "num")
  freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
  assert(sum(freq$num * freq$tam) == n)
######################################## Graficas de Salida del sistema ####################################### 
  tl <- paste(paso, "", sep="")
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  png(paste("p8_ct", tl, ".png", sep=""), width=300, height=300)
  tope <- 50 * ceiling(max(cumulos) / 50)
  hist(cumulos, breaks=seq(0, tope, 50), 
       main=paste("Paso", paso, "con ambos fen\u{00f3}menos"), freq=FALSE,
       ylim=c(0, 0.05), xlab="Tama\u{00f1}o", ylab="Frecuencia relativa")
  graphics.off()
}
 
   tfinal<-Sys.time()
   dat<-rbind(dat, cbind(n, k, "P", tfinal-tinicial))  
}
}
stopCluster(cluster)

colnames(dat)<-c("n", "k", "Admini", "Tiempo")
write.csv(dat, file="dat_paralelo.csv")





ww="C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_8/Graficas/dat_paralelo.csv"
para=read.csv(ww,header = T)



ww="C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_8/Graficas/dat_secuencial.csv"
png("comparacion.png", width=600, height=600)
secu=read.csv(ww,header = T)
lim<-c(secu[,5],para[,5])
plot(secu[,5], type="l", xlab="k",cex=1, lwd=2, ylab="Tiempo (s)", main="",axes=F, ylim=c(0, max(para[,5],secu[,5])))
legend(2, 200, c("Secuencial", "Paralelo"), col=c(1,3), bty="n" ,lty=1, cex=1,lwd=2 )
axis(1, at=1:4, labels=c(10000, 15000,30000,90000),cex.lab=1, lwd=2)
axis(2,cex.lab=1.5, lwd=2)
lines(para[,5], type="l", col=3)
box(lwd=2)
graphics.off()



dt<-rbind(secu,para)
dt
dim(dt)
boxplot(dt[,5]~dt[,4], col=rainbow(2))
res<-dt[,5]
met<-dt[,4]

wilcox.test(res~met)
kruskal.test(met~res)

