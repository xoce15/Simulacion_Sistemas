##############################################################################
###                          Simulación de Producción de aguacate HASS      ##
###                                                                         ##
###                     28/10/2017                                          ##
###                                                      xoce15,Alex,Montes ##
##############################################################################

#file.remove(list.files(pattern=".png")) 
#rm(list=ls())
#gc(TRUE)


library("testit")
library(parallel)
library(sna)
#library(magick)


setwd("~/tablas")


datos<-read.csv("producciones.csv",header = T,sep = ",")

n_municipios<-dim(datos)[1]
dias_ciclo<-as.numeric(as.Date("2015-6-30")-as.Date("2014-06-30"))+1
inicio_ciclo<-as.Date("2014-06-30")
dias<-1:dias_ciclo
#datos$A_2014<-as.numeric(as.character(datos$A_2014))

media_diaria<-function(xi,c0,c1,c2,c3,pmax){
  
  assert(c3>c2 & c2>c1 & c1>c0)
  pi<-0
  
  if (xi>=c0 & xi<=c1)
  {
    pi<-pmax*((xi-c0)/(c1-c0))
  }
  if (xi>=(c1+1) & xi<=c2)
  {
    pi<-pmax
  }
  if (xi>=(c2+1) & xi<=c3)
  {
    pi<-pmax*((c3-xi)/(c3-c2))
  }
  return(pi)
}


###################### prod_mun genera dos tipos de producción por centro de producción              #############
###################### una versión deterministica y la otra estocastica por medio de Monte-Carlo     #############

prod_mun<-function(n0,n1,n2,n3,media,varianza){
  
  factor_esc<-(1/3)*(n2+n3-n1-1)
  ymax<-media/factor_esc
  
  dias<-1:dias_ciclo
  producciones<-numeric()
  producciones_al<-numeric()
  for (i in 1:dias_ciclo)               
  { 
    if (i<=n3 & i>=n0)
    {
      producciones[i]<-media_diaria(xi=dias[i],c0=n0,c1=n1,c2=n2,c3=n3,
                                    pmax=ymax)  
    }else
      producciones[i]<-0
  }
  desv_producciones<-sqrt(varianza*producciones*(1/media))
  for (i in 1:dias_ciclo)
  {
    producciones_al[i]<-abs(rnorm(1,producciones[i],desv_producciones[i]))
  }
  return(list(al=producciones_al,det=producciones))  
}
############################################################################################################################
### la función lo que recibe es la dirección en donde está ubicada la tabla distancia entre "" ##
### regresa que centro productivo va con que centro de acopio 
vecinos<-function(zona){ 
  url<-"~/tablas/distancias.csv"
  ww1<-url
  acopio<-read.csv(ww1, header = TRUE)
  names(acopio)<-c("Municipios", 1,2,3,4,5,6,7)
  ############################ Selecciona los centros productivos a 120 Kms al centro de acopio #######
  acop<-dim(read.csv("~/tablas/distancias.csv"))[2]-1
  
  order(acopio[1,2:8])
  #if( ano>1){
  # maxw<-apply(acopio[2:8],2,max)
  # minw<-apply(acopio[2:8],2, min)
  #  for (b in 1:length(maxw)){
  #  acopio[n_municipios+(ano-1),1+b]<-runif(1, min=minw[b], max=maxw[b] )
  #  } }
  
  aco1<-vector()
  aco<-data.frame()
  for ( i in 1: n_municipios) {
    aco1<-sort(acopio[i,2:8],decreasing = TRUE, index.return=TRUE)
    aux1<-sum(aco1<zona)
    aco<-rbind(aco,c(i,aux1, order(acopio[i,2:8])))
  }
  names(aco)<-c("CP",120,1,2,3,4,5,6,7)
  ################### De los centros de acopios que tienen el mismo centro productivo selecciono solo un centro de acopio
  asig<-vector()
  for ( j in 1:n_municipios){
    if( aco[j,2]>1){
      fg1<-aco[j,2]
      fg<-aco[j,3:(2+fg1)]
      asig[j]<-sample(fg,1)}
    if( aco[j,2]==1) {
      asig[j]<-aco[j,3]}
    if( aco[j,2]==0){ 
      fg<-aco[j,3:5]
      asig[j]<-sample(fg,1)}
  }
  asig<-unlist(asig)
  aco$asig<-asig
  return(aco)
}



######################################################################################################################
######################################################################################################################
############################# Acumulación de producción en los centros de acopio######################################
######################################################################################################################
######################################################################################################################


######################################################################################################################
############## se ha definido el tamano de contenedor que puede ser modificaco asi como el                          ##
############## factor de penetración de mercado (cuanto es capaz de conseguir de la producción total de su zona)    ##
############## el ciclo "ca" hace los procesos para cada centro de acopio, en vecw coloco los centro productivos    ##
############## correspondientes al centro de acopio asignado en la función vector (produc_acopio) y sobre estos     ##
############## elementos se hace el conteo de la cantidad de toneldas acumuladas, al completar un contenedor (20 t) ##
############## se realiza un envio, claro es posible tener dos o tres envios el mismo periodo de tiempo             ##
######################################################################################################################


####################################### Esta sección permite reactualizar la matriz de producción para cada C.Acopio #############
##################################################################################################################################
##################################################################################################################################


envi<-function(matrix, acopio){
  opcion<-matrix
  #envif<-data.frame()
  for ( ca in acopio){ 
    aux<-subset(opcion, opcion[,1]==ca)
    aux2<-sample(aux$Cliente, size=1, prob=aux$pro)
    #envif<-rbind(envif,subset(aux, aux$Cliente==aux2))
  } 
  return(subset(aux, aux$Cliente==aux2))
} 




zonas<-c(130, 160,200)
acumuw<-c(0.7,0.8,0.9,1)
contenedores<-c(25,50,75)
repe<-seq(1,10)

alexander<-data.frame()

print(zonas)
print(acumuw)
print(contenedores)
print(repe)


w<-1:3
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "datos")
clusterExport(cluster, "inicio_ciclo")
clusterExport(cluster, "n_municipios")
clusterExport(cluster, "prod_mun")
clusterExport(cluster, "media_diaria")
clusterExport(cluster, "dias_ciclo")
clusterExport(cluster, "w" )
clusterExport(cluster, "assert")
clusterExport(cluster, c("vecinos", "envi", "acumuw", "contenedores" )  )



for ( repeticion in repe){
  for ( zona in zonas  ){
    clusterExport(cluster, "zona")
    for( p in acumuw){
      for( contene in contenedores){ 
        time<-0
        qv<-Sys.time()
        
        print(c(repeticion,zona,p,contene))
        
        
        m_producciones<-parLapply(cluster, 1:n_municipios, function(j){
          n0<-as.numeric(as.Date(datos$Dia_inicial[j])-inicio_ciclo+1)
          n1<-as.numeric(as.Date(datos$Pico_1[j])-inicio_ciclo+1)
          n2<-as.numeric(as.Date(datos$Pico_2[j])-inicio_ciclo+1)
          
          n3<-as.numeric(as.Date(datos$Dia_final[j])-inicio_ciclo+1)
          med<-c(datos$A_2014[j],datos$A_2015[j],datos$A_2016[j])
          w<-1:3
          mod1<-lm(med~w)
          newdata=w=4
          media<-predict(mod1,newdata)
          prueba<-summary(mod1)$r.squared
          if(prueba<0.6){ 
            media<-mean(c(datos$A_2014[j],datos$A_2015[j],datos$A_2016[j]))
            
          }
          varianza<-sd(c(datos$A_2014[j],datos$A_2015[j],datos$A_2016[j]))
          return(prod_mun(n0=n0,n1=n1,n2=n2,n3=n3,media=media,varianza=varianza)$al)
        
        }
        )
        
        tamano_contedenor=20
        penetracion_local=0.38*p
        #### Capacidad de los centros de acopio en unidades de contenedores por los dias ciclo, asi será más facil manejar la  ###########
        #### capacidad usada, almacenada para cada centro de acopio                                                            ###########
        
        acop<-dim(read.csv("~/distancias.csv"))[2]-1
        produc_acopio<-vecinos(zona)
        ############### Del total de la produccción del producto solo un factor "penetración local", es acorde al producto que es de interes
        m_producciones1<-list()
        for ( i in 1:n_municipios){
          m_producciones1[[i]]<-m_producciones[[i]]*penetracion_local
        }
        salidas<-matrix(0,ncol=length(dias), nrow=acop)
        sobran<-matrix(0,ncol=(length(dias)+1), nrow=acop)
        recibio<-matrix(0,ncol=length(dias), nrow=acop)
        capa_acopio<-matrix(c(2,3,4,5,2,3,4)*tamano_contedenor, nrow=acop, ncol= (1+length(dias)) )
        capacidad<-capa_acopio[,1]
        
        #fidelprod<-list()
        #fidelacop<-list()
        #fidelacop[[acop]]=NA
        
        for ( t in dias){
          for ( pro in sample(1:n_municipios)) { 
            
            aux1<-produc_acopio[pro,10]                                     ### Este es C.Acopio asignado primeramente 
            falta_envi<-m_producciones1[[pro]][t]-capa_acopio[aux1,t]
            
            capa_acopio[aux1,t]=max(0,-falta_envi)
            # fidelprod[[pro]]<-aux1                                          ##### Con esto saturo la capacidad del C.Acopio al maximo
            
            # if(is.null(fidelacop[[aux1]]) || is.na(fidelacop[[aux1]])){
            #   fidelacop[[aux1]] <-pro  
            # }else{
            #    fidelacop[[aux1]] <-c(fidelacop[[aux1]],pro )
            # }
            ayu<- setdiff(produc_acopio[pro,3:9],aux1)                        ### Voy a verificar cuantos C.Acopio están cerca al radio 120kms
            f=1
            while(falta_envi>0 ){
              aux<-unlist(ayu[f])
              falta_envi<-falta_envi-capa_acopio[aux,t]
              capa_acopio[aux,t]<-max(0,-falta_envi)
              f=f+1
              if( f>6){
                break
              }
              #          fidelprod[[pro]]<-c(fidelprod[[pro]],aux   )
              
              #          if(is.null(fidelacop[[aux]]) | is.na(fidelacop[[aux]])){
              #             fidelacop[[aux]] <-pro  
              #          }else{
              #             fidelacop[[aux]] <-c(fidelacop[[aux]],pro )
              #           }
            }
          }                                              ###FOR 
        for ( aco in 1:acop) { 
            recibio[aco,t]<-capacidad[aco]-capa_acopio[aco,t]
            conte<-(recibio[aco,t]) %/%tamano_contedenor
            sobran[aco,t+1]<-(recibio[aco,t])-(conte*tamano_contedenor)
            salidas[aco,t]<-conte+salidas[aco,t]
            capa_acopio[aco,t+1]<-capa_acopio[aco,t+1]-sobran[aco,t+1]
           }
        }
        
        
        
        ######################################################################################################
        ####################### Esta Sección se hace las graficas como si fuera un              ##############
        ####################### diagrama de GANTS en el eje Y se tiene los centros de acopio    ##############
        ####################### sobre el eje X los dias de año simulados                        ##############
        ######################################################################################################
        # ws<-paste("grafi_", rep, ".png", sep="")
        # png(ws,width=800, height=800 )
        # sociomatrixplot(salidas, main=rep, xlab="Dias del año", cex.lab = 1.5,drawlines = FALSE,
        #             labels=list(c("1", "2", "3","4", "5", "6", "7"), c("")), ylab="Centros de acopio")
        # graphics.off()
        # }
        # frames=lapply(1:100,function(x) image_read(paste("grafi_",x,".png",sep="")))
        # animation <- image_animate(image_join(frames),fps=5 )
        # print(animation)
        # w3<-paste("Acumu_acopio", ".gif"  )
        # image_write(animation, w3)
        
        ##################################################################################################################################
        ##################################################################################################################################
        ################################## Envio internacional  ##########################################################################
        internacional<-read.csv("~/tablas/internacional_precios.csv",header = TRUE,sep = ",")
        costtranspor<-read.csv("~/tablas/precio_transporte.csv",header = TRUE,sep = ",")
        
        precios<-apply(internacional[,2:4], 1,mean)                   #### Precio de una tonelada pagada por el pais i
        paises<-seq(length(precios))
        datprecio<-data.frame(paises,precios)
        puertos<-1:4
        tamano_contedenor<-20
        
        utilidad<-data.frame()
        for ( ca in 1:acop){ 
          aux<-subset(costtranspor[,1:3], costtranspor[,1]==ca)
          for ( puert in puertos){ 
            aux1<-subset(aux, aux$Puerto==puert)
            prepuer<-aux1[2]
            for ( pai in paises) { 
              aux2<-subset(costtranspor[1:20,5:7], costtranspor[,5]==puert)
              aux2<-subset(aux2, aux2[,3]==pai)
              prepais<-aux2[2]
              
              venta<-subset(datprecio, paises==pai,select=precios)
              
              total<-as.numeric(venta)*tamano_contedenor-(as.numeric(prepuer)+as.numeric(prepais))
              
              utilidad<-rbind(utilidad, c(ca,puert,pai, total)   )
            }
          }  
      } 
       
        names(utilidad)<-c("Acopio", "Puerto", "Cliente", "Renta")
        opcion<-data.frame()
        for ( ca in 1:acop){ 
          for ( pai in paises){ 
            aux<-subset(utilidad, Acopio==ca & Cliente==pai)
            max<- max(aux$Renta)
            opcion<-rbind(opcion, c(subset(aux, Renta==max)))
            
          }
        }
       prob<-vector()
        for ( ca in 1:acop){
          aux<-subset(opcion,   opcion[,1]==ca)
          prob<-rbind(prob,cbind(aux[,4]/sum(aux[,4])))
        }
        opcion$pro<-prob
        
        ##################################################################################################################################
        ##### Selección de un cliente aleatoriamente dado la probabilidad que se calculo bajo su rentabilidad esperada ###################
        ##################################################################################################################################
        
        ##################################################################################################################################
        ############################## Está sección hace refencia a cuanta mercancia recibe el pais comprador, se le permite al centro ###
        ############################## de acopio hacer un cambio de pais cada 8 envios con una probabilidad determinada           ########
        ############################## no importa el tiempo entre cada envio sino más bien los contenedores enviados a cada pais. ########
        ############################## change hace referencia a la probabilidad de cambiar de cliente internacional               ########
        ############################## mientras que limi_envi es la cantidad de envios minimos requeridos para permitir el cambio ########
        ############################## si los envios son igual a limi_envi entonces actualizo el contador a CERO                  ########
        ##################################################################################################################################
        change<-0.35
        env_cliente<-data.frame()
        limi_envi<-contene   
        aux<-0
        
        for ( aco in 1:acop ){
          clien<-as.numeric(envi(opcion,aco)[3:4])
          for ( dia in dias){ 
            aux<- salidas[aco,dia]+aux
            auxw<-sum(aux)/limi_envi
            if( auxw==1 & runif(1)<change ){
              clien<-as.numeric(envi(opcion, aco)[3:4])
              aux<-0
            }
            env_cliente<-rbind(env_cliente, c(aco,clien[1], dia, salidas[aco,dia], clien[2]) ) 
            aux<-0
          }
        }
        names(env_cliente)<-c("Acopio","Cliente", "Dia", "Envio", "Renta")  
        
        dat<-data.frame()
        filtro<-list()
        for ( i in 1:7){ 
          filtro[[i]]<-subset(env_cliente, Acopio==i) 
          s<-sum(filtro[[i]]$Envio)
          dat<-rbind(dat, c(i,s, sum(filtro[[i]]$Envio*filtro[[i]]$Renta)))
        }
        names(dat)<-c("Acopio","Env_Total", "Rentabilidad")
        w<-Sys.time()
        time<-w-qv
        for ( wf in 1:7){
          alexander<-rbind(alexander, c(zona,p,contene, repeticion, dat[wf,1], dat[wf,2], dat[wf,3], time)    )  
        }
        
      }
    }
  }
}
names(alexander)<-c("Zona","Penetracion","Con_Change","replica",  "Acopio","To_Env","Renta" , "Tiempo")
print(c("repeticion","zona","p","contene"))
#alexander





366*(2/3)

write.csv(alexander,file="experi_otros.csv")



##################### Efectividad en todo el año ###############################################################################
alexander$efectividad<-(alexander$To_Env/366)/(capacidad/20)
alexander$ciclo_prod<-(alexander$To_Env/(366*(2/3))/(capacidad/20))




lm<-lm(alexander$efectividad~alexander$Zona+alexander$Penetracion+alexander$Con_Change+alexander$Acopio)

sink("lm.txt")
print(summary(lm))
sink() 

summary(lm)
ad.test(resid(lm))
hist(resid(lm), freq = F, main="Histograma",xlab=" ", ylab=" ", ylim=c(0,0.05))



fs<-c(1,2,3,5)
gg<-c(7,8,9,10)

Valor_p<-data.frame()
Estadistico<-data.frame()
 
Valor_p<-apply(alexander[,fs],2, function(x) kruskal.test(alexander[,7])~x$p.value)
Estadistico<-apply(alexander[,fs],2, function(x)  kruskal.test(alexander[,j]~x)$statistic)



acopi1<-alexander

print(wireframe(acopi1$ciclo_prod~acopi1$Acopio+acopi1$Penetracion,
scales = list(arrows=FALSE,cex=.5,tick.number = 10, z = list(arrows=T)),
           xlab="Centro Acopio",
           ylab="Penetración",zlab="Efectividad (%)",main=paste("Efectividad ciclo productivo",  sep="") ,
           light.source = c(10,10,10), drape=T,
           col.regions = rainbow(100, s = 1, v = 1, start = 0, end = max(1,100 - 1)/100, alpha = 1)) )




boxplot(alexander$Tiempo~alexander$Zona)
Secu<-read.csv("C:/Users/Z230/Dropbox/Alex_tesis/tablas/experi_otros2.csv",header = TRUE,sep = ",")

Secu$X<-NULL
Secu$efectividad<-(Secu$To_Env/366)/(capacidad/20)
Secu$ciclo_prod<-(Secu$To_Env/(366*(2/3))/(capacidad/20))
Secu$metodologia<-"Secu"
alexander$metodologia<-"Parale"



analisis<-rbind(Secu,alexander)


h<-lm(analisis$Tiempo~analisis$metodologia)
summary(h)
boxplot(analisis$Tiempo~analisis$metodologia, ylab="Tiempo (s)", col=c( 3,4))

