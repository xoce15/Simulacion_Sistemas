# Práctica 6


# Introducción


El presente documento contiene el reporte de la práctica número 6 [\[1\]](#bibliograf%C3%ADa), 
sobre  "sistemas multiagentes". En dicha práctica se simula un caso de epidemia en donde hay tres 
tipos de  estados de los diferentes agentes: Infectado, Recuperado y Suceptible. Donde los infectados son los agentes que 
van a propagar la infección, los susceptible son agentes que no han tenido la epidemia y son propensos a ser infectados 
con determinada probabilidad, mientras que los recuperados son agentes que portaron la enfermedad y aparte 
tuvieron una recuperación exitosa de ella. 
 <p align="justify"> </p>


### Especificaciones computacionales 
<p align="justify">
La presente práctica se realizó en una computadora 
con procesador Intel(R) Xeon(R) CPU E3-1245 v3 @3.4GHz 3.4GHz, con 16 GB de memoria RAM y 8 núcleos.
</p>

## Especificaciones Experimentales 
<p align="justify">
Para la comparación de la implementación secuencia contra la versión paralelizada se usaron cuatro diferentes 
tamaños de agentes (<img src="https://latex.codecogs.com/gif.latex?n"/>) siendo: 50,100,500,  
con una cantidad total de cinco replicas por cada tamaño de agentes. 
</p>

## Secciones posibles a ser paralelizados 
<p align="justify">
En esta práctica el trabajo principal se centra en determinar cuáles partes del código pueden ser paralelizadas y cuales 
no convendría paralelizarlo. En la <a href="#fig1"> Figura 1</a>, se presenta el diagrama de flujo de la simulación estudiada. 
Los movimientos y las actualizaciones tienen un limite maximo de repetición que es igual a
<img src="https://latex.codecogs.com/gif.latex?tmax"/>.
Como es percibible existe tres procesos principales, en donde los que se paralelizaron fueron: propagación de infección, movimientos y actualización.
La creación de agentes no se paralelizo dado que este proceso solo se realiza una vez en toda la simulación a comparación de propagación de infección
y movimientos que se repite <img src="https://latex.codecogs.com/gif.latex?tmax"/> veces.
</p>

<p align="center">
<div id="fig1" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_6/Graficas/flujo.jpg" height="70%" width="70%"/><br>
<b>Figura 1.</b> Diagrama de flujo del proceso de epidemia
</div>
</p>

La función paralelizada de contagio se muestra a continuación

```R
contagios<-parSapply(cluster, 1:n,function(i) {  
    
    if(agentes$estado[i]==3 || agentes$estado[i]==4 ){ 
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
```

<p align="justify">
Para la paralelización de la función movimientos y actualizaciones no se cambio mucho el codigo secuencial, a comparación 
del caso anterior en donde se recorre los agentes  susceptible y no los infectados como estaba implementado. La parte
diferente es la conversión de la información que arroja parSapply. La función utilizada se muestra a continuación:
</p>

 ```R
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

  ```
 
 
 <p align="justify">
  En donde el resultado de la función es <img src="https://latex.codecogs.com/gif.latex?a"/>, dicho elemento tiene propiedades de lista y tiene la información 
  con la que se requiere trabajar pero no organizada adecuadamente, para lo cual se transpone y se le quita
  la propiedad de lista. Posteriormente se alimenta a un dataframe que contiene una matriz con las dimensiones 
  requeridas. 
  </p>
  
  
  
   <p align="justify">
   En la <a href="#fig2"> Figura 2</a>, se presenta la variación de los tiempos para la ejecución en paralelo y la secuencial 
   para los diferentes tamaños de agentes, en donde en todos los casos la paralelización presenta mejores resultados que el proceso
   secuencial
  </p>
  
<p align="center">
<div id="fig2" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_6/Graficas/P6_NR%20.gif" height="70%" width="70%"/><br>
<b>Figura 2.</b> Comparación de tiempos secuencial (s) contra paralelo (p)
</div>
</p>
  
  <p align="justify">
   En la <a href="#fig3"> Figura 3</a>, se presenta los tiempos obtenidos tanto por secuencial y paralelizado pero sin filtrar por 
   la cantidad de agentes presentes. Dicha grafica se hace para una facil interpretación del fenomeno por el lector. 
  </p>
  <p align="center">
<div id="fig3" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_6/Graficas/P6_Todos.png" height="70%" width="70%"/><br>
<b>Figura 3.</b> Comparación de tiempos secuencial (s) contra paralelo (p) global
</div>
</p>

  
  
  
  
  <p align="justify">
   En la <a href="#fig4"> Figura 4</a>, se presenta la variación de la probabilidad de recuperados desde el inición de la simulación, 
   al disminuir el porcentaje de recuperados al inicio genera mayor numero de infectados a lo largo del tiempo, pero el considerar 
   proporciones de agentes diferentes a cero hace que la cantidad de infectados caiga considerablemente en el tiempo, dado que 
   si un agente desde el inicio está en el estado de recuperado no podra llegar al estado infectado, dado que el estado recuperado es 
   un estado absorvente. 
  </p>
  
  
  <p align="center">
<div id="fig4" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_6/Graficas/Rplot05.png" height="70%" width="70%"/><br>
<b>Figura 4.</b> Generación de recuperados con diferentes probabilidades
</div>
</p>
  
  
 <p align="justify">
 En la <a href="#fig5"> Figura 5</a>, finalmente se presenta la varación de proporción de infectados desde el inicio de la simulación,
en donde con el aumento de la probablidad la cantidad de infectados tiende a aumentar como era de esperarse para probabilidades menores
a 50%, despues de este umbral la cantidad de agentes infectados tiende a disminuir.  
  </p>
  
  
  
  
<p align="center">
<div id="fig5" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_6/Graficas/R2.png" height="70%" width="70%"/><br>
<b>Figura 5.</b> Generación de recuperados con diferentes probabilidades
</div>
</p>
  
  
  

#### Bibliografía
1. S.E. Schaeffer. Práctica 5: Diagramas de Voronoi, <i>R paralelo: simulación & análisis de datos</i>, http://elisa.dyndns-web.com/teaching/comp/par/p6.html.
