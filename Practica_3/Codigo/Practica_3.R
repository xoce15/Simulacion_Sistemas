##############################################################################
###                          Practica 3 Teoria de colas                     ##
###                                usando DoParallel                        ##
###                     21/08/2017                                          ##
###                                                                         ##
##############################################################################


library(RColorBrewer) ## Libreria de colores 
library(ggplot2)      ## Para las graficas de barras 
suppressMessages(library(doParallel))

rm(list=ls())

desde <- 1000
hasta <-  30000
replicas <- 30
original <- desde:hasta
invertido <- hasta:desde


primo <- function(n) {
  if (n == 1 || n == 2 || n==3) {
    return(TRUE)
  }
  if (n %% 2 == 0) {
    return(FALSE)
  }
  for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
    if ( n != i && ((n %% i) == 0)) {
      return(FALSE)
    }
  }
  return(TRUE)
}


primocer<-function(n){ 
  
  for ( i in n:1)
  { 
    if(primo(i)==TRUE){
      p4<-i
      break;}
  }
  return(p4)
}


ay<-vector()
t1<-1:(10*length(original))
for( i in 1: length(original)){ 
  ay[i]<-primo(t1[i])
  
}

q<-t1[ay==TRUE]
q<-q[1:length(original)]     ### Solo primos

q2<-t1[ay==FALSE]
q2<-q2[1:length(original)]   ### No primos

t2<-length(original)/2
q3<-c(sample(q2,size=(floor(t2)) ), sample(q,size=(ceiling(t2) ))  )  
q3[is.na(q3)]<-sample(q2, size=1)               ## Mezcla aleatoria secuencia entre primos y no primos 

## Este caso contempla el primer primo  encontrado desde el final hasta el principio repetido n veces
q4<-rep( primocer(hasta), length.out=length(original))  


cores<-seq(from=1,to=(detectCores() -1)   )


ot <-  numeric()
it <-  numeric()
at <-  numeric()
pri<- numeric()
Nop<- numeric()
mezcla<-numeric()
repeti<-numeric()


info<-data.frame(matrix(vector(), 0,7  ))
for ( t in  cores) { 
  registerDoParallel(makeCluster(cores))
  
  for (r in 1:replicas) {
    
    if ( t==1) { 
      
      ot<-c(ot, system.time(sapply(original,function(x) primo(x)  ))[3])
      it<-c(it, system.time(sapply(invertido,function(x) primo(x)  ))[3])
      at<-c(at, system.time(sapply(sample(original),function(x) primo(x) ))[3])
      pri<-c(pri, system.time(sapply(q, primo ))[3])
      Nop<-c(Nop, system.time(sapply(q2, primo ))[3])
      mezcla<-c(mezcla, system.time(sapply(q3, primo ))[3])
      repeti<-c(repeti, system.time(sapply(q4, primo ))[3])
      
      }
    ot <- c(ot, system.time(foreach(n = original, .combine=c) %dopar% primo(n))[3]) # de menor a mayor
    it <- c(it, system.time(foreach(n = invertido, .combine=c) %dopar% primo(n))[3]) # de mayor a menor
    at <- c(at, system.time(foreach(n = sample(original), .combine=c) %dopar% primo(n))[3]) # orden aleatorio
    pri<-c(pri, system.time(foreach(n = q, .combine=c) %dopar% primo(n))[3])
    Nop<-c(Nop, system.time(foreach(n = q2, .combine=c) %dopar% primo(n))[3])
    mezcla<-c(mezcla, system.time(foreach(n = q3, .combine=c) %dopar% primo(n))[3])
    repeti<-c(repeti, system.time(foreach(n = q4, .combine=c) %dopar% primo(n))[3])
  info<- rbind(info, cbind(t,1,as.numeric(ot)), cbind(t,2,as.numeric(it)),cbind(t,3,as.numeric(at)),
  cbind(t,4,as.numeric(pri)), cbind(t,5,as.numeric(Nop)),cbind(t,6,as.numeric(mezcla)),cbind(t,7,as.numeric(repeti)) )
    
  }
  
}  

stopImplicitCluster()
colnames(info)<-c("Procesa","Ordenamiento", "Tiempos")

write.csv(info, file="Doparallel.csv")


#### Pruebas Estadisticas 
setwd("C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_3/Pruebas") 

Parame<-aov(Tiempos~Ordenamiento+Procesa,data=info)  
summary(Parame)
b55<-resid(Parame)
c<-ad.test(b55)[2]

par(mfcol=c(1,2))
pdf(file = "Normalidad_C1.pdf", bg = "transparent")
hist(resid(Parame),freq=F, ylab="",xlab="", main="Histograma", ylim=c(0,0.3));lines(density(b55),col=2)
text(12,0.1,"AD Valor p:");text(26,0.1,c)
qqnorm(b55, main="");qqline(b55)
dev.off()

  sink("lm.txt")
  print(summary(Parame))
  sink() 

  Valor_p<-apply(info[,1:2],2, function(x) kruskal.test(info[,3]~x)$p.value)
  Estadistico<-apply(info[,1:2],2, function(x) kruskal.test(info[,3]~x)$statistic )
  ar<-c(Estadistico,Valor_p)
  
  az<-matrix(ar,ncol=2,nrow = 2)
  colnames(az)<-c("Kruskal","Valor P")
  row.names(az)<-c("Procesa","Ordenamiento")
  write.csv (az, file="P3_Kruskal.csv")

  
  
  setwd("C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_3/Graficas") 
  
  info$Procesa<-as.factor(info$Procesa)
  info$Tiempos<-as.numeric(info$Tiempos)
  info$Ordenamiento<-as.factor(info$Ordenamiento)
  
  ### Defino mi paleta de colores 
  tq<-as.numeric(names(table(info[,2])))
  colores<-brewer.pal(n = length(tq), name = 'Accent')
  
  
  w21<-paste("P3_R1Segmentado",  ".pdf", sep="")
  pdf(file = w21, bg = "transparent")
  
  ggplot(info, aes(x=Procesa, y=Tiempos)) +
    geom_boxplot(aes(fill=Ordenamiento) )+theme_bw()+ ylab("Tiempos (s)")+ xlab("Procesadores")+
    facet_wrap( ~Ordenamiento , scales="free")+ ggtitle("Segmentados")+theme_bw()+
    theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
          text = element_text(size = 12, family = "Tahoma"),
          axis.title = element_text(face="bold"),
          axis.text.x=element_text(size = 11)) +
    scale_fill_brewer(palette = "Accent")
  dev.off()
  
  
  w22<-paste("P3_R1to",  ".pdf", sep="")
  pdf(file = w22, bg = "transparent")
  ggplot(info, aes(x=Procesa, y=Tiempos, fill=Ordenamiento)) +
    geom_boxplot(  )+theme_bw()+ ylab("Tiempos (s)")+ xlab("Procesadores")+ ggtitle("Todos")+
    scale_fill_brewer(palette = "Accent")
  
  dev.off()
  
  w23<-paste("P3_R1fil",  ".pdf", sep="")
  pdf(file = w23, bg = "transparent")
  par(mfcol=c(1,2))
  
  boxplot(Tiempos~Ordenamiento, data=info, col=colores ,xlab="Ordenamiento", ylab="Tiempo (s)")
  boxplot(Tiempos~Procesa, data=info, col=topo.colors((detectCores() -1)  ), xlab="Procesadores",ylab="Tiempo (s)")
  dev.off()
  
  
  