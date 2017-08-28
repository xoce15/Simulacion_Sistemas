##############################################################################
###                          Practica 3 Teoria de colas                     ##
###                                usando Parallel                          ##
###                     21/08/2017                                          ##
###                                                                         ##
##############################################################################

library(RColorBrewer) ## Libreria de colores 
library(ggplot2)      ## Para las graficas de barras 
library(parallel)

rm(list=ls())

desde <- 1000
hasta <-   30000
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


cores<-seq(from=1,to=detectCores() - 1) 
ot <-  numeric()
it <-  numeric()
at <-  numeric()
pri<- numeric()
Nop<- numeric()
mezcla<-numeric()
repeti<-numeric()


info<-data.frame(matrix(vector(), 0,7  ))
                       
for ( t in  cores) { 
  cluster <-makeCluster(cores)
  
  clusterExport(cluster, "replicas"   )
  clusterExport(cluster, "primo"   )
  clusterExport(cluster, "original"   )
  clusterExport(cluster, "invertido"   )
  clusterExport(cluster, "q"   )
  clusterExport(cluster, "q2"   )
  clusterExport(cluster, "q3"   )
  clusterExport(cluster, "q4"   )

   for( a in replicas) {  
      if ( t==1) { 
          ot<-c(ot, system.time(sapply(original, primo  ))[3])
          it<-c(it, system.time(sapply(invertido, primo  ))[3])
          at<-c(at, system.time(sapply(sample(original), primo ))[3])
          pri<-c(pri, system.time(sapply(q, primo ))[3])
          Nop<-c(Nop, system.time(sapply(q2, primo ))[3])
          mezcla<-c(mezcla, system.time(sapply(q3, primo ))[3])
          repeti<-c(repeti, system.time(sapply(q4, primo ))[3])
   }
    ot<-c(ot,system.time(parSapply(cluster, original, primo))[3])
    it <- c(it, system.time(parSapply(cluster, invertido, primo))[3])
    at <- c(at, system.time(parSapply(cluster, sample(original), primo))[3])
    pri<-c(pri, system.time(parSapply(cluster, q, primo))[3])
    Nop<-c(Nop,    system.time(parSapply(cluster, q2, primo))[3])
    mezcla<-c(mezcla, system.time(parSapply(cluster, q3, primo))[3])
    repeti<-c(repeti, system.time(parSapply(cluster, q4, primo))[3])
           
 info<- rbind(info, cbind(t,1,as.numeric(ot)), cbind(t,2,as.numeric(it)),cbind(t,3,as.numeric(at)),
 cbind(t,4,as.numeric(pri)), cbind(t,5,as.numeric(Nop)),cbind(t,6,as.numeric(mezcla)),cbind(t,7,as.numeric(repeti)) )
   } 
} 
stopImplicitCluster()
colnames(info)<-c("Procesa","Ordenamiento", "Tiempos")


write.csv(info, file="parallel.csv")

#### Pruebas Estadisticas 

Parame<-aov(Tiempos~Ordenamiento+Procesa,data=info)  
summary(Parame)
b55<-resid(Parame)
c<-ad.test(b55)[2]



setwd("C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_3/Graficas") 


pdf(file = "Normalidad_Parallel.pdf", bg = "transparent")
par(mfcol=c(1,2)) 
 hist(resid(Parame),freq=F, ylab="",xlab="", main="Histograma");lines(density(b55),col=2)
 # text(12,0.1,"AD Valor p:");text(26,0.1,c)
  qqnorm(b55, main="");qqline(b55)
  dev.off()



  setwd("C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_3/Pruebas") 
  

sink("lm_Parallel.txt")
print(summary(Parame))
sink()




a56<-rbind(info[,2] %in% c(1,2,3))
info3<-matrix(info[a56==TRUE], ncol=3)
#colnames(info3)<-c("Procesa","Ordenamiento", "Tiempos")


Valor_p<-apply(info[,1:2],2, function(x) kruskal.test(info[,3]~x)$p.value)
Estadistico<-apply(info[,1:2],2, function(x) kruskal.test(info[,3]~x)$statistic )

Valor_p2<-apply(info2[,1:2],2, function(x) kruskal.test(info2[,3]~x)$p.value)
Estadistico2<-apply(info2[,1:2],2, function(x) kruskal.test(info2[,3]~x)$statistic )

Valor_p3<-apply(info3[,1:2],2, function(x) kruskal.test(info3[,3]~x)$p.value)
Estadistico3<-apply(info3[,1:2],2, function(x) kruskal.test(info3[,3]~x)$statistic )


ar<-c(Estadistico,Valor_p, Estadistico2,Valor_p2, Estadistico3,Valor_p3)

az<-matrix(ar,ncol=6,nrow = 2)
colnames(az)<-c("Kruskal","Valor p", "Kruskal(S7)","Valor p (S7)", "Kruskal(3)","Valor p (S3)")
row.names(az)<-c("Procesa","Ordenamiento")
write.csv (az, file="P3_Kruskal_Parallel.csv")




setwd("C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_3/Graficas") 

info$Procesa<-as.factor(info$Procesa)
info$Tiempos<-as.numeric(info$Tiempos)
info$Ordenamiento<-as.factor(info$Ordenamiento)

### Defino mi paleta de colores 
tq<-as.numeric(names(table(info[,2])))
colores<-brewer.pal(n = length(tq), name = 'Accent')


w21<-paste("P3_R1Segmentado_Parallel",  ".pdf", sep="")
pdf(file = w21, bg = "transparent")

ggplot(info, aes(x=Procesa, y=Tiempos)) +
  geom_boxplot(aes(fill=Ordenamiento) )+theme_bw()+ ylab("Tiempos (s)")+ xlab("Procesadores")+
        facet_wrap( ~Ordenamiento , scales="free")+ ggtitle("Segmentados")+theme_bw()+
        theme(plot.title = element_text(size = 14, family = "Tahoma", face = "bold"),
        text = element_text(size = 12, family = "Tahoma"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11)) +
        scale_fill_brewer(palette = "Accent")
graphics.off()
dev.off()



w22<-paste("P3_R1to_Parallel",  ".pdf", sep="")
pdf(file = w22, bg = "transparent")
  ggplot(info, aes(x=Procesa, y=Tiempos, fill=Ordenamiento)) +
        geom_boxplot(  )+theme_bw()+ ylab("Tiempos (s)")+ xlab("Procesadores")+ ggtitle("Todos")+
 scale_fill_brewer(palette = "Accent")
  graphics.off()
  dev.off()

  
  
  info2<-subset(info,  Ordenamiento!= 7)
  w24<-paste("P3_R1to_S7_Parallel",  ".pdf", sep="")
  pdf(file = w24, bg = "transparent")
  ggplot(info2, aes(x=Procesa, y=Tiempos, fill=Ordenamiento)) +
    geom_boxplot(  )+theme_bw()+ ylab("Tiempos (s)")+ xlab("Procesadores")+ ggtitle("Todos")+
    scale_fill_brewer(palette = "Accent")
  graphics.off()
  dev.off()  
  
  
  
  
w23<-paste("P3_R1fil_Parallel",  ".pdf", sep="")
pdf(file = w23, bg = "transparent")
par(mfcol=c(1,2))
 
  boxplot(Tiempos~Ordenamiento, data=info, col=colores ,xlab="Ordenamiento", ylab="Tiempo (s)")
  boxplot(Tiempos~Procesa, data=info, col=topo.colors((detectCores() -1)  ), xlab="Procesadores",ylab="Tiempo (s)")
  graphics.off()
  dev.off()





