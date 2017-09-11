##############################################################################
###                Practica 5. método  Monte Carlo                          ##
###                         Normal                                          ##
###                     06/09/2017                                          ##
###                                                                         ##
##############################################################################

suppressMessages(library(distr))
rm(list=ls())                  ### Borro la memoria de R 



inicio <- -6
final <- -inicio
paso <- 0.25
x <- seq(inicio, final, paso)
f <- function(x) { return(1 / (exp(x) + exp(-x))) }
png("p5f.png") # dibujamos f(x) para ver como es
plot(x,  (2/pi) * (1/(exp(x)+exp(-x))))
lines(x,  (2/pi) * (1/(exp(x)+exp(-x))), type="l")
graphics.off()

g <- function(x) { return((2 / pi) * f(x)) }
generador  <- r(AbscontDistribution(d = g)) # creamos un generador

parte <- function() {  valores <- generador(mues)
return(sum(valores >= desde & valores <= hasta))
}
x<-seq(from=-6, to=6, length.out = 300)
fx<-1/(exp(x)+exp(-x))
gx<-2*fx/pi
plot(gx~x)




datos<-data.frame(matrix(vector(), 0,4  ))
mu<-c(50000,50000*1.5,50000*2,50000*2.5,50000*3,50000*5,50000*50 )
#mu<-c(50,500, 1000)

library(parallel)

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "parte")
clusterExport(cluster,"mu")
clusterExport(cluster,"g")
clusterExport(cluster,"f")


for ( mues in mu){
  clusterExport(cluster,"mues")
    muestra <- generador(mues) # sacamos una muestra
   desde <- 3
  hasta <- 7
  pedazo <- 500000
  cuantos <- 500
  #repetir<-4
  a<-vector()
  b<-vector()
  res<-vector()
  for ( r in 1:30){
  clusterExport(cluster,"r")
  clusterExport(cluster,"desde")
  clusterExport(cluster,"hasta")
  clusterExport(cluster,"generador")
    a[r]<-Sys.time()
  montecarlo<-parSapply(cluster,1:cuantos,function(x) {valores <- generador(mues)
  return(sum(valores >= desde & valores <= hasta))
  }   )
  b[r]<-Sys.time()
  integral <- sum(montecarlo) / (cuantos * pedazo)
  res[r]<-((pi / 2) * integral)
  datos<-rbind(datos,c(mues, r, res[r],b[r]-a[r]))
  }
}

stopImplicitCluster()
colnames(datos)<-c("Muestra","Repe" , "Pi","Tiempo")
setwd("C:/Users/Z230/Dropbox/PISIS/PhD/Simulacion_Sistemas/Practica_5/Codigo")
write.csv(datos, file="P5_N_datos.csv")



boxplot(Pi~Muestra, data=datos)

#######################################

runs <- 100000
#runif samples from a uniform distribution

library(parallel)
runs<-c(10000,20000,50000,80000)
datos<-data.frame()
Genepi<-function(){  
  xs <- runif(runs,min=-0.5,max=0.5)
  ys <- runif(runs,min=-0.5,max=0.5)
  in.circle <- xs^2 + ys^2 <= 0.5^2
  mc.pi <- (sum(in.circle)/runs)*4
  return(mc.pi)
}

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "runs")
clusterExport(cluster, "Genepi")

for ( i in runs ){  
  a1<-Sys.time()
  P5R1<-parSapply(cluster, 1:100, function(x){  
    xs <- runif(runs,min=-0.5,max=0.5)
    ys <- runif(runs,min=-0.5,max=0.5)
    in.circle <- xs^2 + ys^2 <= 0.5^2
    mc.pi <- (sum(in.circle)/runs)*4
    return(mc.pi)
  })
  b1<-Sys.time()
  aw<-mean(P5R1)
  datos<-rbind(datos,c(aw, b1-a1))
}
colnames(datos)<-c("Promedio","Tiempo")

################################################

zika<-c(0,0,4,17,4,2,25,4,3,0,16,4,6,3,9,3,3,5,16,2,
        4,1,44,10,11,8,1,40,11,12,5,28,54,1)
hist(zika)

johnso<-(-1.86051+0.774719*sinh((zika+0.257332)/1.08323   ))

dias<-seq(1:length(zika))
plot(zika, type="l")
zika2<-(((zika^(1/3)) ))
#aw<-lm(zika2~ dias)
#ks.test(  zika2,dias)
shapiro.test(d)

#shapiro.test(diff(zika
quantile(mc.closing, 0.95)
quantile(mc.closing,0.05)
summary(mc.closing)
zika[21]
runs <- 10000

#simulates future movements and returns the closing price on day 200
generate.path <- function(vec){
  d <- diff(vec)
  n <- length(d)
  changes <- rnorm(n, mean=mean(d),sd=sd(d))
  sample.path <- cumsum(c(vec[1],changes))
  closing.price <- sample.path[n+1] #+1 because we add the opening price
  return(closing.price)
}
mc.closing <- replicate(runs,generate.path(zika[0:33]))

zika[34]
zika[33]+mean(mc.closing)

zika



plot(mc.closing)

#c<-matrix(rbind(a,b), ncol=3, nrow=dim(a)[2]*2 )

rweibull(length(zika), scale=9.52147,shape=0.82309  )
rexp(length(zika), rate=   )


zim<-mean(zika2)
zid<-sd(zika2)