# Práctica 5


# Introducción


El presente documento contiene el reporte de la práctica número 5 [\[1\]](#bibliograf%C3%ADa), 
sobre El metodo "Monte Carlo". El cual usa la generación de numeros aleatorios, para obtener un parametro que se quiere estimar. 
 <p align="justify"> </p>


### Especificaciones computacionales 
<p align="justify">
La presente práctica se realizó en una computadora 
con procesador Intel(R) Xeon(R) CPU E3-1245 v3 @3.4GHz 3.4GHz, con 16 GB de memoria RAM y 8 núcleos.
</p>


## Especificaciones Experimentales 
<p align="justify">
Para la variación del tamaño de la muestra en la aproximación de la integral se usó crecimiento exponenciales en los tamaños de la muestra,
siendo la base 50000, y los exponentes usados: 1,1.5,2,2.5,3,5,50.  Para los tamaños de muestra para el calculo de Pi, se usaron las mismas 
muestras que anteriormente. 
En las graficas el eje horizontal se a cambiado por una secuencia de números que va desde uno hasta la cantidad de muestras usadas para el 
estudio, mencionado anteriormente.  
Para estimar la error de estimación se considera la siguiente formula: <br>
</p>
<p align="center">
<img src="https://latex.codecogs.com/gif.latex?(\frac{real-pronosticado}{real})\times100"/>
</p>
Donde real es el valor patrón, <br>
pronosticado es el valor obtenido por los calculos realizados. 

# Resultados 
<p align="justify">
Como primer objetivo de la practica se tiene  el efecto del tamaño de muestra en 
la precisión del estimado, comparando con Wolfram Alpha, por un lado y por otro lado en el tiempo de ejecución. 
Se usó Wolfram Alpha para obtener la solución de la integral dado y a está formula se la evaluo en R, en el rango estipulado. Dicho 
valor de referencia es: 0.04883411. 
</p>
<p align="justify">
En la <a href="#fig1"> Figura 1</a>, muestra como la media de las estimaciones se va acercando al valor esperado (linea roja), al aumentar 
la muestra la estimación de la integral mejora aunado a la  disminución de la desviación de los experimentos. Mientras que el lado derecho 
de la figura presenta el error relativo de los experimentos, en donde la linea roja punteada hace referencia a la media
de todo el sistema. Como se percibe en las graficas el error de la estimación disminuye al aumentar el tamaño de la muestra. 
</p>

<p align="center">
<div id="fig1" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_5/Graficas/P5_Valor_aproximado.png" height="70%" width="70%"/><br>
<b>Figura 1.</b> Valor aproximado de la integra 
</div>
</p>


<p align="justify">
En la <a href="#fig2"> Figura 2</a>, presenta los tiempos computaciones al calcular el valor aproximado de la integral, en donde la linea
azul representa la media del sistema. La ultima muestra lleva más de siete veces en ejecución en promedio  a comparación de las demás muestras. 
Aunque es evidente su mayor tiempo de ejecución, este mismo no crecio al nivel que crecio la muestra, trayendo ahorro de tiempo significativos 
en este casos, pues se esperaba que el tiempo de procesamiento creciera al exponente 50 como lo hizo la muestra. 

<p align="center">
<div id="fig2" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_5/Graficas/P5_Tiempo.png" height="70%" width="70%"/><br>
<b>Figura 2.</b> Tiempo de ejecución  (s)
</div>
</p>

<p align="justify">
Se genero como segundo objetivo de la practica la aproximación al numero de Pi, el valor de referencia del valor Pi fue: 3.141593. 
En la <a href="#fig3"> Figura 3</a>, muestra como la aproximación va mejorando cuando se va aumenta el tamaño de la muestra. 
En color rojo se tiene el valor de referecia de Pi. 
</p>

A continuación se muestra la implementación en paralelo de la aproximación del numero de Pi. 
~~~R
cluster <- makeCluster(detectCores() - 1)
runs1<-c(50000,50000*1.5,50000*2,50000*2.5,50000*3,50000*5,50000*50)

for ( runs in runs1 ){
  clusterExport(cluster, "runs")  
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
  datos<-rbind(datos,c(runs, aw, b1-a1))
}
colnames(datos)<-c("muestra", "Promedio","Tiempo")

~~~
<p align="center">
<div id="fig3" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_5/Graficas/P5_R1Aprox.png" height="70%" width="70%"/><br>
<b>Figura 3.</b> Aproximación al numero Pi 
</div>
</p>


<p align="justify">
La <a href="#fig4"> Figura 4</a>, muestra el consumo promedio  de tiempo para cada tamaño de muestra, como sucedio en el caso anterior
el tiempo aumento, pero el mismo no crecio de con la misma tasa de aumento que crecio la muestra.

</p>


<p align="center">
<div id="fig4" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_5/Graficas/P5_R2tiempo.png" height="70%" width="70%"/><br>
<b>Figura 4.</b> Tiempo de ejecución (s)
</div>
</p>



Finalmente se paraleliza la implementación de pronosticos que usa el metodo  Monte Carlo  obtenida en [\[2\]](#bibliograf%C3%ADa),
el cual es presentado a continuación:


~~~R
generate.path <- function(vec){
  d<-diff(vec)
  n <- length(d)
  changes <- rnorm(n,mean=mean(d),sd=sd(d))
  sample.path <- cumsum(c(vec[9],changes))
  closing.price <- sample.path[n+1] 
   return(closing.price)
}
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "generate.path")  
clusterExport(cluster, "runs")  
clusterExport(cluster, "zika") 
ppro<-vector()
for ( i in 1:(length(zika)-18 ) )
  {
  clusterExport(cluster, "i")
  as<-parSapply(cluster, 1:30, function(x){
  c.closing <- replicate(runs,generate.path(zika[10:(18+i)])  )
  return( c.closing)
  })
ppro[i]<-abs(zika[11+i]-(zika[10+i]+mean(as)) )/zika[11+i]
}
~~~


<p align="justify">
En donde zika[10:(18+i)], es el vector que se usa con la longitud determinada para realizar la predección de cuantas personas
tendran zika en el siguiente periodo. El autor usa "cumprod", pero para este caso presenta muy malos ajustes (anchos de limites de confianza
en millares), mientras que "cumsum", genera mejores estimaciones y limites de confianza más cerrados. 
Se usarón minimo ocho datos para estimar el siguiente, como el tamaño de submuestra más pequeño. 
Usando un intervalo entre 10 a 22, se obtiene un error de estimación del 2.9%, mientras que la media del sistema es de 664 %, lo
cual es poco alentador para estos pronosticos. 
</p>

Si se usa la propuesta del autor paralelizada la media del error de estimación es de 1.198e+29 y aunque la estrategia implementada 
generá errores fuertes, dichos errores son de menor magnitud a comparación del presentado en [\[2\]](#bibliograf%C3%ADa).




#### Bibliografía
1. S.E. Schaeffer. Práctica 5: Diagramas de Voronoi, <i>R paralelo: simulación & análisis de datos</i>, http://elisa.dyndns-web.com/teaching/comp/par/p5.html.
2. Will Kurt, <i>Countbayesie.</i>. https://www.countbayesie.com/blog/2015/3/3/6-amazing-trick-with-monte-carlo-simulations.
4. R. Calaway, S. Weston, D. Tenenbaum. Foreach Parallel Adaptor for the 'parallel' Package. <i>R Package</i>, https://cran.r-project.org/web/packages/doParallel/doParallel.pdf.

