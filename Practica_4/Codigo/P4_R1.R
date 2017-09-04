##############################################################################
###                          Practica 4 Voronoi                             ##
###                                Reto 1                                   ##
###                     28/08/2017                                          ##
###                                                                         ##
##############################################################################


library(RColorBrewer) ## Libreria de colores 
library(ggplot2)
suppressMessages(library(doParallel))
library(stats)
library(moments)
library(nortest)
library(e1071)
library(pryr)
library(magick)


rm(list=ls())

reto<-"R1"
corri<-"mezcla"
## multiplos de 4
nt <- c(40,52,60,72,80,92)

kt <-c(12,12+12*0.25,12+12*0.5,12+12*0.75, 12*2,12*3,12*4)
exp<-c(1,2,3)


vp <- data.frame(numeric(), numeric()) # posiciones de posibles vecinos
for (dx in -1:1) {
  for (dy in -1:1) {
    if (dx != 0 | dy != 0) { # descartar la posicion misma
      vp <- rbind(vp, c(dx, dy))
    }
  }
}
names(vp) <- c("dx", "dy")
vc <- dim(vp)[1]



inicio <- function() {
  direccion <- sample(1:4, 1)
  xg <- NULL
  yg <- NULL
  if (direccion == 1) { # vertical
    xg <- 1
    yg <- sample(1:n, 1)
  } else if (direccion == 2) { # horiz izr -> der
    xg <- sample(1:n, 1)
    yg <- 1
  } else if (direccion == 3) { # horiz der -> izq
    xg <- n
    yg <- sample(1:n, 1)
  } else { # vertical al reves
    xg <- sample(1:n, 1)
    yg <- n
  }
  return(c(xg, yg))
}


celda <-  function(pos) {
  fila <- floor((pos - 1) / n) + 1
  columna <- ((pos - 1) %% n) + 1
  if (zona[fila, columna] > 0) { # es una semilla
    return(zona[fila, columna])
  } else {
    cercano <- NULL # sin valor por el momento
    menor <- n * sqrt(2) # mayor posible para comenzar la busqueda
    for (semilla in 1:k) {
      dx <- columna - x[semilla]
      dy <- fila - y[semilla]
      dist <- sqrt(dx^2 + dy^2)
      if (dist < menor) {
        cercano <- semilla
        menor <- dist
      }
    }
    return(cercano)
  }
}


propaga <- function(replica) {
  # probabilidad de propagacion interna
  prob <- 1
  dificil <- 0.99
  grieta <- voronoi # marcamos la grieta en una copia
  i <- inicio() # posicion inicial al azar
  xg <- i[1]
  yg <- i[2]
  largo <- 0
  while (TRUE) { # hasta que la propagacion termine
    grieta[yg, xg] <- 0 # usamos el cero para marcar la grieta
    largo <-  largo + 1
    frontera <- numeric()
    interior <- numeric()
    for (v in 1:vc) {
      vecino <- vp[v,]
      xs <- xg + vecino$dx # columna del vecino potencial
      ys <- yg + vecino$dy # fila del vecino potencial
      if (xs > 0 & xs <= n & ys > 0 & ys <= n) { # no sale de la zona
        if (grieta[ys, xs] > 0) { # aun no hay grieta ahi
          if (voronoi[yg, xg] == voronoi[ys, xs]) {
            interior <- c(interior, v)
          } else { # frontera
            frontera <- c(frontera, v)
          }
        }
      }
    }
    elegido <- 0
    if (length(frontera) > 0) { # siempre tomamos frontera cuando haya
      if (length(frontera) > 1) {
        elegido <- sample(frontera, 1)
      } else {
        elegido <- frontera # sample sirve con un solo elemento
      }
      prob <- 1 # estamos nuevamente en la frontera
    } else if (length(interior) > 0) { # no hubo frontera para propagar
      if (runif(1) < prob) { # intentamos en el interior
        if (length(interior) > 1) {
          elegido <- sample(interior, 1)
        } else {
          elegido <- interior
        }
        prob <- dificil * prob # mas dificil a la siguiente
      }
    }
    if (elegido > 0) { # si se va a propagar
      vecino <- vp[elegido,]
      xg <- xg + vecino$dx
      yg <- yg + vecino$dy
    } else {
      break # ya no se propaga
    }
  }
  if (largo >= limite) {
    png(paste("p4gr1_", replica, ".png", sep=""))
    par(mar = c(0,0,0,0))
    image(rotate(grieta), col=rainbow(k+1), xaxt='n', yaxt='n')
    graphics.off()
  }
  return(largo)
}


suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))



bucion<-function (ex1){ 
  if(ex1==1){
    s<-seq(from=0, to=10, by=(10/(n/2)))
    aux1<-vector()
    aux2<-vector()
    tr2<-pexp(s)
    
    for( i in 2: length(tr2))
    {
      aux1[i-1]<-tr2[i]-tr2[i-1]
    }
    
    s1<-seq(from=-3, to=3, by=(6/(n/2)))
    aux2<-vector()
    tr3<-pnorm(s1)
    
    for( i in 2:length(tr3) )
    {
      aux2[i-1]<-tr3[i]-tr3[i-1]
    }
    aux2
    aux3<-c(aux1,aux2)
    
    return(sample(1:n, prob =aux3, 1))  
  }else if (ex1==2){
     normales<-floor(rnorm(1, mean=n/2, sd=n/8))
    return(normales)
  }
  else{
    pois<- rpois(1, lambda = 1/(5.5)*n)
  return( pois )
}
}



datosR1<-data.frame()


for ( ex1 in exp){ 
for ( k in kt){
  for ( n in nt){ 
    
    zona <- matrix(rep(0, n * n), nrow = n, ncol = n)
    
    x <- rep(0, k) # ocupamos almacenar las coordenadas x de las semillas
    y <- rep(0, k) # igual como las coordenadas y de las semillas
    
    
    
    
for (semilla in 1:k) {
      while (TRUE) { # hasta que hallamos una posicion vacia para la semilla
        fila <- bucion( ex1)    ##### primer reto 
        columna <- bucion( ex1)  ###
        if (zona[fila, columna] == 0) {
          zona[fila, columna] = semilla
          x[semilla] <- columna
          y[semilla] <- fila
          break
        }
      }
    }
   # par(mar = c(0,0,0,0))
   rotate <- function(x) t(apply(x, 2, rev))
  #  image(rotate(zona), col=rainbow(k+1), xaxt='n', yaxt='n')
    
        
    
    
    celdas <- foreach(p = 1:(n * n), .combine=c) %dopar% celda(p)
    stopImplicitCluster()
    voronoi <- matrix(celdas, nrow = n, ncol = n, byrow=TRUE)
   # image(rotate(voronoi), col=rainbow(k+1), xaxt='n', yaxt='n')
 
    
   
    
    
    limite <- n # grietas de que largo minimo queremos graficar
    #for (r in 1:10) { # para pruebas sin paralelismo
    largos <- foreach(r = 1:200, .combine=c) %dopar% propaga(r)
    for ( q in 1:200){
      datosR1<-rbind(datosR1,c(n,k,ex1,largos[q]))
    }
    #datos2<-rbind(datos,c(n,k,t(largos)))
  }
}
  
}  

stopImplicitCluster()
names(datosR1)<-c("n","k" ,"Distri" ,"Largos")

setwd("C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_4/Codigo") 
#names(datos)<-c("n","k","Largos")
write.csv (datosR1, file="datosR1b.csv")


ac<-lm(Largos~k+n+Distri, data = datosR1)
png("P4_histogramaR1.png")
hist(resid(ac), freq = F, main="Histograma",xlab=" ", ylab=" ", ylim=c(0,0.05))
lines(density(resid(ac)), col=3  )
c<-ad.test(resid(ac))[2]
text( mean(density(resid(ac))$x) ,max(density(resid(ac))$y),"AD valor p:" )
text( mean(density(resid(ac))$x)+(mean(density(resid(ac))$x))/2.7 ,max(density(resid(ac))$y),c )
dev.off()


sink("lm.txt")
print(summary(ac))
sink() 


setwd("C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_4/Pruebas_Estadisticas") 


Valor_p<-apply(datosR1[,1:3],2, function(x) kruskal.test(datosR1[,4]~x)$p.value)
Estadistico<-apply(datosR1[,1:3],2, function(x) kruskal.test(datosR1[,4]~x)$statistic )
ar<-c(Estadistico,Valor_p)

az<-matrix(ar,ncol=2,nrow = 3)
colnames(az)<-c("Kruskal","Valor p")
row.names(az)<-c("n","k", "Distri")
write.csv (az, file="P4_KruskalR1.csv")



par(mfcol=c(1,2)) 
boxplot(Largos~n, data=datos,ylab="Longitud", xlab="Ta cuadridula",  main="n", col=topo.colors(length(nt)))
boxplot(Largos~k, data=datos, main="k",ylab="Longitud", xlab="Cantidad de semillas", col=cm.colors(length(kt))[length(kt):1 ]  )





### Defino mi paleta de colores

setwd("C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_4/Graficas")

tq<-as.numeric(names(table(datosR1[,1])))
colores<-brewer.pal(n = length(tq), name = 'Accent')

datosR1$n<-as.factor(datosR1$n)
datosR1$k<-as.factor(datosR1$k)
datosR1$Distri<-as.factor(datosR1$Distri)
datosR1$Largos<-as.numeric(datosR1$Largos)

###Preguntar a ELISA

for ( i  in 1:3) {
  i=3
w24<-paste("P4R1_", i, ".png", sep="")
print(i)
png(w24)
ggplot(subset(datosR1, datosR1$Distri==i) ,aes(x=n, y=Largos, fill=n) )+geom_violin(trim=FALSE)+
  scale_fill_brewer(palette = "RdBu")+ theme_minimal()+ stat_summary(fun.y=mean, geom="point",color="black",  size=2)+
ggtitle(paste("Distribución", i))+ylim(0,250)
dev.off()
}


frames=lapply(1:3,function(x) image_read(paste("P4R1_",x,".png",sep="")))
animation <- image_animate(image_join(frames), fps=0.8)
print(animation)
w3<-paste("P4R1_2distr", ".gif"  )
image_write(animation, w3)



for ( i in 1:3){
  w21<-paste("P4R1_Segmentado_", i, ".png", sep="")
png(w21)

ggplot(subset(datosR1, datosR1$Distri==i), aes(x=n, y=Largos)) +
  geom_boxplot(aes(fill=k) )+theme_bw()+ ylab("Largo")+ xlab("Tamaño de cuadricula")+
  facet_wrap( ~k , scales="free")+ ggtitle("Segmentados")+theme_bw()+
  theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) +
  scale_fill_brewer(palette = "Accent")+ylim(0,max(datosR1$Largos)) + 
ggtitle(paste("Distribución", i))
graphics.off()

}
frames=lapply(1:3,function(x) image_read(paste("P4R1_Segmentado_",x,".png",sep="")))
animation <- image_animate(image_join(frames), fps=0.8)
print(animation)
w3<-paste("P4R1_2t", ".gif"  )
image_write(animation, w3)








w25<-paste("P4_k_", reto, corri, ".pdf", sep="")
pdf(file = w25, bg = "transparent")
ggplot(datos,aes(x=k, y=Largos, fill=k) )+geom_violin()+geom_violin(trim=FALSE)+
  scale_fill_brewer(palette = "Dark2")+ theme_minimal()+ stat_summary(fun.y=mean, geom="point",color="black",  size=2)
dev.off()






w22<-paste("P4_Todos",  reto, corri, ".pdf", sep="")
pdf(file = w22, bg = "transparent")
ggplot(datos, aes(x=n, y=Largos, fill=k)) +geom_violin(trim=FALSE)+
  theme_bw()+ ylab("Largo")+ xlab("Tamaño cuadricula")+ ggtitle("Todos")+
  scale_fill_brewer(palette = "Accent")
dev.off()
graphics.off()
dev.off()




file.remove(list.files(pattern=".png")) 



#frames=lapply(0:200,function(x) image_read(paste("p4g_",replica,".png",sep="")))
#animation <- image_animate(image_join(frames))
#print(animation)
#w3<-paste("p4g_voronoi", dim, ".gif"  )
#image_write(animation, w3)
#sapply(0:200,function(x) file.remove(paste("p4g_",x,".png",sep="")))
