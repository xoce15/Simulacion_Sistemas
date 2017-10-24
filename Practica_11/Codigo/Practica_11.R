##############################################################################
###                          Practica 11. Frente de Pareto                  ##
###                                                                         ##
###                     21/10/2017                                          ##
###                                                                         ##
##############################################################################

setwd("C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_11/Graficas")
#file.remove(list.files(pattern=".png")) 
rm(list=ls())
gc(TRUE)
library(parallel)







pick.one <- function(x) {
  if (length(x) == 1) {
    return(x)
  } else {
    return(sample(x, 1))
  }
}

poli <- function(maxdeg, varcount, termcount) {
  f <- data.frame(variable=integer(), coef=integer(), degree=integer())
  for (t in 1:termcount) {
    var <- pick.one(1:varcount)
    deg <- pick.one(1:maxdeg)
    f <-  rbind(f, c(var, runif(1), deg))
  }
  names(f) <- c("variable", "coef", "degree")
  return(f)
}

eval <- function(pol, vars, terms) {
  value <- 0.0
  for (t in 1:terms) {
    term <- pol[t,]
    value <-  value + term$coef * vars[term$variable]^term$degree
  }
  return(value)
}

domin.by <- function(target, challenger, total) {
  if (sum(challenger < target) > 0) {
    return(FALSE) # hay empeora
  } # si no hay empeora, vemos si hay mejora
  return(sum(challenger > target) > 0)
}








############################################# Experimentación ######################################
####################################################################################################



vc <- 4
md <- 3
tc <- 5
k <- seq( from= 2, to=10) # cuantas funciones objetivo
k<-2
frente<-data.frame()


dat<-data.frame()
for ( k in k ){ 
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, c("vc", "md", "k", "tc","poli","pick.one"))
obj1<-list()

obj<-parLapply(cluster, 1:k, function(i){ 
    poli(vc, md, tc)
    }
  )
  stopCluster(cluster)

minim <- (runif(k) > 0.5)
sign <- (1 + -2 * minim)
n <- 200 # cuantas soluciones aleatorias
sol <- matrix(runif(vc * n), nrow=n, ncol=vc)
val <- matrix(rep(NA, k * n), nrow=n, ncol=k)


cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, c("obj", "sol", "k", "tc", "n", "eval"))

val1<-parSapply(cluster, 1:n, function(i){  
  af<-rep(0,k) 
  for (j in 1:k) { # para todos los objetivos
        af[j]<-eval(obj[[j]], sol[i,], tc)
         }
  return(af) 
  }
   )
stopCluster(cluster)
val<-t(val1)

mejor1 <- which.max(sign[1] * val[,1])
mejor2 <- which.max(sign[2] * val[,2])
cual <- c("max", "min")
#xl <- paste("Primer objetivo (", cual[minim[1] + 1], ")", sep="")
#yl <- paste("Segundo objetivo (", cual[minim[2] + 1], ")", sep="")
#png(paste("p11_init",k,  ".png"))
#plot(val[,1], val[,2], xlab=xl, ylab=yl, main="Ejemplo bidimensional")
#graphics.off()
#png("p11_mejores.png")
#plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
#     ylab=paste(yl,"mejor con bolita naranja"),
#     main="Ejemplo bidimensional")
#points(val[mejor1, 1], val[mejor1, 2], col="blue", pch=15, cex=1.5)
#points(val[mejor2, 1], val[mejor2, 2], col="orange", pch=16, cex=1.5)
#graphics.off()

no.dom <- logical()
dominadores <- integer()

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, c("obj", "sol", "k", "tc", "n", "dominadores", "sign", "no.dom" , "domin.by", "val", "eval"))


d <- logical()
clusterExport(cluster, c("d"))
  for (i in 1:n) {
       clusterExport(cluster, c("i"))
  d<- parSapply(cluster, 1:n, function(j){
       domin.by(sign * val[i,], sign * val[j,], k)
      }
  )
  cuantos <- sum(d)
  dominadores <- c(dominadores, cuantos)
  no.dom <- c(no.dom, cuantos == 0) # nadie le domina
  }
stopCluster(cluster)


# 
# no.dom <- logical()
# for (i in 1:n) {
#   d <- logical()
#   for (j in 1:n) {
#     d <- c(d, domin.by(sign * val[i,], sign * val[j,], k))
#   }
#   no.dom <- c(no.dom, sum(d) == 0) # nadie le domina
# }












frente<- subset(val, no.dom)

dat<-rbind(dat, cbind(k, dominadores/n, dominadores   ))
}

#library(scatterplot3d)
#scatterplot3d(frente[[3]][,1:3],color = 4, pch = 16,grid=TRUE, box=FALSE)


#q<-frente[[3]]
#qa<-median(q[,1])
#selc<-q==qa
#which(selc)

max1<-apply(frente, 2, max)
min2<-apply(frente,2,min)
ad<-frente[order(frente[,1],frente[,2]),]
#which(frente[[3]], frente[[3]][,1]== median1   )
png("p11_M_1.png",width=800, height=500)
xl <- paste("Primer objetivo (", cual[minim[1] + 1], ")", sep="")
yl <- paste("Segundo objetivo (", cual[minim[2] + 1], ")", sep="")
plot(val[,1], val[,2], xlab=paste(xl, "mejor con cuadro azul"),
     ylab=paste(yl,""),
     main="Medianas")
points(frente[,1], frente[,2], col="green", pch=16, cex=1.5)

as<-lm(frente[,2]~frente[,1])
abline(as, col="red")
points(max1[1], max1[2], col="blue",pch=16, cex=2)
points(min2[1], min2[2], col="blue",pch=16, cex=2)

tg<-apply(ad,2,median)
points(tg[1],tg[2], col=2,cex=2, pch=16)
df<-subset(frente, frente[,1]>tg[1] & frente[,2]>tg[2]  )
ws1<-apply(df,2,median)
t2<-rbind(tg,ws,ws1)
points(t2[,1], t2[,2],pch=16, col="blue", cex=2)
graphics.off()




wg<-c( 1/4,1/2, 3/4)
sis<-floor(dim(frente)[1]-0.4) *wg
sist<-ad[sis,]
png("p11_M_2.png",width=800, height=500)
xl <- paste("Primer objetivo (", cual[minim[1] + 1], ")", sep="")
yl <- paste("Segundo objetivo (", cual[minim[2] + 1], ")", sep="")
plot(val[,1], val[,2], xlab=paste(xl, ""),
     ylab=paste(yl,""),
     main="Sistematico")
points(frente[,1], frente[,2], col="green", pch=16, cex=1.5)

as<-lm(frente[,2]~frente[,1])
abline(as, col="red")
points(max1[1], max1[2], col="blue",pch=16, cex=2)
points(min2[1], min2[2], col="blue",pch=16, cex=2)
points(sist[,1], sist[,2], col="blue",pch=16, cex=2)
graphics.off()


 










 library(ggplot2) # recordar instalar si hace falta
#data <- data.frame(pos=rep(0, n), dom=dominadores)

png("p11_violin_2.png",width=800, height=500)
gr <- ggplot(dat, aes(x=factor(k), y=V2 ) ) + 
  geom_boxplot(width=0.5, aes(fill = factor(k)), show.legend =  FALSE,color="black", lwd=1)+
geom_violin(aes(fill = factor(k)),width=1,show.legend =  FALSE)
gr+geom_jitter(shape=16, aes(colour=factor(k) ), show.legend =  FALSE,position=position_jitter(0.2) )+
xlab("Cantidad de funciones objetivo") +
  ylab("Porcentaje") +
  ggtitle("Cantidad de soluciones dominantes")
graphics.off()

png("p11_violin_1.png",width=800, height=500)
gr <- ggplot(dat, aes(x=factor(k), y=V2 ) ) + 
  #geom_boxplot(width=0.5, aes(fill = factor(k)), show.legend =  FALSE,color="black", lwd=1)+
  geom_violin(aes(fill = "blue" ),width=1 ,show.legend =  FALSE)+
#gr+geom_jitter(shape=16, aes(colour=factor(k) ), show.legend =  FALSE,position=position_jitter(0.2) )+
  xlab("Cantidad de funciones objetivo") +
  ylab("Porcentaje") +
  ggtitle("Cantidad de soluciones dominantes (violines)")
print(gr)
graphics.off()



library(ggplot2)
library(grid)
library(gridExtra)

library(RColorBrewer) ## Libreria de colores
colores<-brewer.pal(n = length(k1), name = 'Paired')


a <- ggplot(dat, aes(x=factor(k), y=V2 ) ) + 
    geom_violin(width=0.8,show.legend =  FALSE,scale = "width")+
  geom_boxplot(width=0.4,  show.legend =  FALSE,color="black", lwd=1)+
  #geom_jitter(shape=16, aes(colour=factor(k) ), show.legend =  FALSE,position=position_jitter(0.2) )+
  xlab("Cantidad de funciones objetivo") +
  ylab("Porcentaje") +
  ggtitle("Cantidad de soluciones dominantes")



b <- ggplot(dat[0:600,], aes(x=factor(k), y=V2 ) ) + 
  geom_violin(width=0.8,show.legend =  FALSE,scale = "width")+
  geom_boxplot(width=0.4, show.legend =  FALSE,color="black", lwd=1)+
  
#geom_jitter(shape=16, aes(colour=factor(k) ), show.legend =  FALSE,position=position_jitter(0.2) )+
  xlab("") +
  ylab("") +
  
  ggtitle("")


c <- ggplot(dat[601:1800,], aes(x=factor(k), y=V2 ) ) + 
  geom_violin(width=0.8,show.legend =  FALSE,scale = "width")+
  geom_boxplot(width=0.4, show.legend =  FALSE,color="black", lwd=1)+
  
#geom_jitter(shape=16, aes(colour=factor(k) ), show.legend =  FALSE,position=position_jitter(0.2) )+
  xlab("") +
  ylab("") +ylim(0,max(dat[601:1800,2])    )+
  ggtitle("")



grid.newpage()
png("p11_violin_3.png",width=800, height=500)
pushViewport(viewport(layout = grid.layout(2, 2)))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
print(a, vp = vplayout(1, 1:2))  # key is to define vplayout
print(b, vp = vplayout(2, 1))
print(c, vp = vplayout(2, 2))
graphics.off()

