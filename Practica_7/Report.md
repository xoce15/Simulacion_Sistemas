# Práctica 7


# Introducción


El presente documento contiene el reporte de la práctica número 7 [\[1\]](#bibliograf%C3%ADa), 
sobre busquedas locales". El cual usa la función bidimensional mostrada a continuación:
<p align="center">
<img src="http://latex.codecogs.com/svg.latex?g(x,y)=\frac{(x+\frac{1}{2})^4-30x^2-20x+(y+\frac{1}{2})^4-30y^2-20y}{100}" border="0"/>

Se quiere maximizar dicha función restringiendo el dominio de las 
<img src="https://latex.codecogs.com/gif.latex?(x,y)"/>, las cuales deben ser mayores a -6 y menores a 5.
Si se evalua <img src="http://latex.codecogs.com/svg.latex?g(x,y)"/> en los intervalos mencionados, la representación 
grafica seria similar a la mostrada en la <a href="#fig1"> Figura 1</a>.


<p align="center">
<div id="fig1" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_7/Graficas/superficie.png" height="70%" width="70%"/><br>
<b>Figura 1.</b> Función evaluada en el dominio
</p>



### Especificaciones computacionales 
<p align="justify">
La presente práctica se realizó en una computadora 
con procesador Intel(R) Xeon(R) CPU E3-1245 v3 @3.4GHz 3.4GHz, con 16 GB de memoria RAM y 8 núcleos.
</p>

### Especificaciones experimentales 
<p align="justify">
Se tiene como paso un valor de 0.1, se usaron 20 replicas y se corrieron dos escenarios con 100 pasos y 300 pasos,
para realizar las diferentes graficas. 
Se tiene como temperatura inicial para la implementación del recocido simulado de 12 y el decrecimiento de la misma es 
0.005, cada iteración en donde se acepta un resultado que no fue mejor la función objetivo. 
</p>


# Resultados 

Como primer trabajo propuesto para está practica se tiene hacer la busqueda local para la función 
<img src="http://latex.codecogs.com/svg.latex?g(x,y)"/>

```R
  
replica <- function(j) {
  x <- runif(1, low, high)
  y <- runif(1, low, high)
  bestpos <- c(x, y)
  bestval <- g(x, y)
  trayectoria = c(bestval)
  camino = c(x, y)
  for (tiempo in 1:t) {
    d <- runif(1, 0, step)
    op = c(x - d, y, x + d, y, x, y - d, x, y + d)
    posibles = numeric() 
    for (i in 1:4) { 
      posibles <- c(posibles, g(op[2*i - 1], op[2*i]))
    }
    mejor <- which.max(posibles)
    nuevo = posibles[mejor] 
    x<-op[2*mejor - 1]
    y<-op[2*mejor]
    if((x<= high & y <= high) & ( x>= low & y>= low)){ 
      
      if (nuevo > bestval) { # minimizamos
        bestpos <- c(op[(2*mejor - 1) ],op[2*mejor])
        bestval <- nuevo
      }
      
      trayectoria <- c(trayectoria, bestval)
    }else { 
      x <- runif(1, low, high)
      y <- runif(1, low, high)
    }
    camino = c(camino, x, y)
  }
  return(camino) 
}

```
<p align="justify">
Claro como se nota está estructura mostrada no pareciera estar paralelizada, pero solo es necesario ponerla dentro de la 
función parSapply y quedaria paralelizada, a continuación se muestra como quedaria llamada desde parSapply
</p>


```R
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "replica")
dat<-data.frame()
resultados <-parSapply(cluster, 1:replicas, replica)
dat<-rbind(dat, resultados)
```
<p align="justify">
Se ha eliminado varias variables que deben ser cargadas para que funcione correctamente resultados, dado que 
se tiene este codigo como un ejemplo y no una fidedigna implementación. 

Posteriormente a dicha busqueda local se grafico para visualizar sus cambios a lo largo del tiempo
En la <a href="#fig2"> Figura 2</a>, se presenta la busqueda local graficada en donde los diferentes colores
de los puntos representan diferentes agentes, los cuales están presentes en el mismo momento que los demás. 
Poco a poco los agentes se van acercando más al maximo de la función, aunque unos pocos no se pueden reubicar, 
es decir quedan atrapados en maximos locales sin poder escapar de ellos (punto gris). 
</p>

<p align="center">
<div id="fig2" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_7/Graficas/P7_100.gif" height="70%" width="70%"/><br>
<b>Figura 2.</b> Busqueda local con diferentes agentes (t=100),<br> colores más fuertes representan mayores valores en la función. 
</div>
</p>

<p align="justify">
Mientras que la <a href="#fig3"> Figura 3</a> exhibe como van moviendose los diferentes agentes con un maximo de 300
pasos, en donde pareciera que solo seria necesario para está configuración tener 200 pasos dado que todos en dicho paso
se encuentran ubicados en el maximo de la función. 
</p>

<p align="center">
<div id="fig3" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_7/Graficas/P7_300.gif" height="70%" width="70%"/><br>
<b>Figura 3.</b> Busqueda local con diferentes agentes (t=300), <br> colores más fuertes representan mayores valores en la función. 
</div>
</p>

<p align="justify">
Como tercera sección se realizó una implementación de un recocido simulado, la cual es una tecnica de optimizacón
inspirada en procesos de función de metales, la cual acepta con determinada probabilidad un valor de la función evaluada 
que no sea mejor que la anterior guardada. A continuación se presenta la implentación:
</p>


```R
replica <- function(j) {
  x <- runif(1, low, high)
  y <- runif(1, low, high)
  bestpos <- c(x, y)
  bestval <- g(x, y)
  trayectoria = c(bestval)
  camino = c(x, y)
  actual<-c(x,y)
  mejor<-bestval
  for (tiempo in 1:t) {
    d <- runif(1, 0, step)
    op = c( max(x - d,low), y, min(x + d, high), y, x,max(y - d,low) , x,min( y + d, high))
    posibles = numeric() 
    for (i in 1:4) { 
      posibles <- c(posibles, g(op[2*i - 1], op[2*i]))
    }
    aux<-floor(runif(1, min=1,max=length(posibles)))
    aux1<-posibles[aux]
    delta<-aux1-g(actual[1],actual[2] ) 
    if(delta>0){
      mejor<-posibles[posibles==aux1]
      actual<- c(op[2*mejor - 1],op[2*mejor])
      
    } else if ( runif(1)<exp(delta/tempe) ) { 
      actual<-c(op[2*aux - 1],op[2*aux])
      tempe<-tempe*0.995
    }
    nuevo = posibles[mejor]
    x<-actual[1]
    y<-actual[2]
      trayectoria <- c(trayectoria, bestval)
      camino = c(camino, x, y, tempe )
  }
  return(camino) 
}
```

<p align="justify">
La <a href="#fig4"> Figura 4</a> presenta como va decreciendo la temperatura a lo largo de las iteraciones, para cada
uno de los agentes. En donde el que tiene el mayor decrimiento pierde el más o menos el 25% del valor inicial de la temperatura
</p>


<p align="center">
<div id="fig4" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_7/Graficas/tempe.png" height="70%" width="70%"/><br>
<b>Figura 4.</b>, Variación de la temperatura a lo largo del tiempo por replica. 
</div>
</p>

#### Bibliografía
1. S.E. Schaeffer. Práctica 7:Busqueda Local, <i>R paralelo: simulación & análisis de datos</i>, http://elisa.dyndns-web.com/teaching/comp/par/p7.html.


