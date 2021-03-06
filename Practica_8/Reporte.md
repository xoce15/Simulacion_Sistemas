# Práctica 8


# Introducción


El presente documento contiene el reporte de la práctica número 8 [\[1\]](#bibliograf%C3%ADa), 
sobre "modelo de urnas". Una de las principales caracteristicas de está modelación es que no importa
que sucede sobre un elemento determinado sino más bien lo que sucede en el estrato al cual pertenece dicho elemento. 
Lo que se está simulando es un proceso quimico de filtrado de particulas, en donde a las particulas contenidas 
en un recipiente se les permite unirse  bajo una distribución  exponencial y se les permite romperse siguiendo una distribucción signoide. 



<p align="justify">
La <a href="#fig1"> Figura 1</a>, presenta el diagra de flujo para el proceso de modelación, en donde al inicio se generan 
<img src="https://latex.codecogs.com/gif.latex?k"/> elementos, que se distribuyen bajo una distribucción normal. Despues
estos elementos se agrupan en <img src="https://latex.codecogs.com/gif.latex?n"/> cúmulos. Posteriormente sigue un proceso 
que se realiza en el mismo <img src="https://latex.codecogs.com/gif.latex?t"/>, tanto el rompimiento como la agregación de cúmulos. Finalmente se reporta la distrubuccón final de los cumulos en el  de <img src="https://latex.codecogs.com/gif.latex?p"/>
paso. Como criterio de selección de los elementos a paralelizar, se escogieron las fases tanto de rompimiento como unificación de cúmulos.
Dado que estos pasos se tendran que repetir al menos <img src="https://latex.codecogs.com/gif.latex?p"/> pasos 
por <img src="https://latex.codecogs.com/gif.latex?k"/> 

</p>

<p align="center">
<div id="fig1" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_8/Graficas/flujo_001.png" height="70%" width="70%"/><br>
<b>Figura 1.</b> Diagrama de flujo del proceso de separación y/o eliminación de cúmulos 
</p>



### Especificaciones computacionales 
<p align="justify">
La presente práctica se realizó en una computadora 
con procesador Intel(R) Xeon(R) CPU E3-1245 v3 @3.4GHz 3.4GHz, con 16 GB de memoria RAM y 8 núcleos.
</p>

### Especificaciones experimentales 
<p align="justify">
Se tiene como valor de duración 30, mientras que para  <img src="https://latex.codecogs.com/gif.latex?k "/> se usaron diferente valores siendo:
<ol>
<li>  10000
<li>   15000
<li>   30000 
<li>   90000
</ol>
Para <img src="https://latex.codecogs.com/gif.latex?n"/> aumenta dependiendo de 
<img src="https://latex.codecogs.com/gif.latex?k"/> de la siguiente forma <img src="https://latex.codecogs.com/gif.latex?n:30k"/>
</p>


# Resultados 


La paralelización de roptura se presenta a continuación 
```R
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
   ```
  Mientras que la paralelización de la unificación de cúmulos es:
  
  ```R
  clusterExport(cluster, "freq")
  faunion<-parSapply(cluster, 1:dim(freq)[1], function(i) { # fase de union
  cumulos <- unirse(i)
   return(cumulos)
           }) 
   cumulos<-unlist(faunion)
   ```
  <p align="justify">
  Dado que la fase de roptura hace cambios en frecuencia de los cúmulos, estos cambios son cargados 
  antes de ejecutar la fase de unión y posteriormente se emboca a la función. Claro tambien es necesario hacerlo con la  función 
  <em>farotura</em> dado que <em>funion</em> hace cambios en la frecuencia de cúmulos y para conectarlos a lo largo del tiempo se debe darle de nuevo
  está información a los diferentes procesadores usados para la experimentación. 
  </p>
  
 <p align="justify">
La <a href="#fig2"> Figura 2</a>, presenta la evolución del tiempo  con diferentes  <img src="https://latex.codecogs.com/gif.latex?n:k"/>,
tanto para el proceso secuencial como para el proceso paralelizado, en donde pareciera no haber grandes diferencias entre uno con respecto
al otro, pero despues del 90000, se comienza a generar separaciones que probablemente con mayor experimentación se podria 
 notar con mayor sensibilidad el efecto de la paralelizació o no. 
</p>
  
  
  <p align="center">
<div id="fig2" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_8/Graficas/comparacion.png" height="70%" width="70%"/><br>
<b>Figura 2.</b> Comparación de tiempos entre secuencial contra paralelizado
</div>
</p>
  
 <p align="justify">  
En la  <a href="#tabl1"> Cuadro 1</a>,  contiene la prueba estadistica  de Kruskal-Wallis, para sustentar los supuestos mencionados anteriormente,Con un 95% o 99% de confianza se puede afirmar que no existe evidencia suficiente para rechazar Ho por lo cual se afirmar que estadisticamente no hay diferencia en que estrategia administrativa (secuencial/paralelizado)  que se use para la ejecución de los experimentos, los tiempos son estadisticamente similares. 
 Puede ser que el uso de la libreria para la paralelización en el tipo de proceso simulado no es lo suficientemente dificil para notar una diferencia significativa en los tiempos de ejecución.
</p>

| Factores  | Estadistico KS | Valor <img src="https://latex.codecogs.com/gif.latex?p"/>|
| :-------: | ------:        | -----: |
| secu/para        | 7         | 0.4289   |
<caption>Cuadro 1. Pruebas estadísticas </caption><br>
<br>
  
 <p align="justify">
La <a href="#fig3"> Figura 3</a>, muestra los datos recolectados en el experimento por medio de una 
 grafica de cajas, en donde se puede percibir que el proceso paralelizado tiene mayor variación a comparación del secuencial
 dado que su caja es mayor ademas del vigote del cuarto cuartil. 
</p>
  


  <p align="center">
<div id="fig3" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_8/Graficas/boxplot.png" height="70%" width="70%"/><br>
<b>Figura 3.</b> Comparación de tiempos entre secuencial contra paralelizado por grafica de cajas
</div>
</p>
  
  
 <p align="justify">
La <a href="#fig4"> Figura 4</a>, muestra la variación de la distribución a lo largo del tiempo, en donde la linea
 roja representa la media de los cúmulos+la desviación estandar  de los cúmulos dividida por dos. 
</p>
  
  
  
 <p align="center">
<div id="fig4" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_8/Graficas/P8.gif" height="70%" width="70%"/><br>
<b>Figura 4.</b> Variación de las distribuciones con k=10000
</div>
</p>
  
  
  
  
## Bibliografía
1. S.E. Schaeffer. Práctica 8:Busqueda Local, <i>R paralelo: simulación & análisis de datos</i>, http://elisa.dyndns-web.com/teaching/comp/par/p8.html.


  
