# Práctica 10


# Introducción


El presente documento contiene el reporte de la práctica número 10 [\[1\]](#bibliograf%C3%ADa), 
sobre "algoritmo genético". Aplicado al problema de la mochila, en donde se quiere empacar diferentes
objetos en una mochila, los cuales tienen diferente peso y valor. 


### Especificaciones computacionales 
<p align="justify">
La presente práctica se realizó en una computadora 
con procesador Intel(R) Xeon(R) CPU E3-1245 v3 @3.4GHz 3.4GHz, con 16 GB de memoria RAM y 8 núcleos.
</p>

### Especificaciones experimentales 
<p align="justify">
Se tiene como la cantidad de cromosomas en el algoritmo genético de 50, mientras que las poblaciones iniciales
son variadas iniciando en 200, luego 400, posteriormente 800 y finalmente 1600. Toda la experimentación se corrio con 30 repeticiones.
La cantidad de generaciones evaluadas fueron: 50,100,200,400,800,1600.
</p>

>


# Resultados


El codigo paralelizado para el algoritmo genético es presentado acontinuación, en donde solo se muestrán lo que se paralelizo 
y asi como esta en este documento no funcionara dado que es un extracto del codigo. 


~~~R
  mut<-parSapply(cluster, 1:tam, mut1)  
  p<-data.frame(matrix(unlist(mut), ncol=n))
  
  tam <- dim(p)[1]
  
  clusterExport(cluster, c("p","tam"))
    
  p1<-parSapply(cluster, 1:rep, function(i) { # una cantidad fija de reproducciones
    padres <- sample(1:tam, 2, replace=FALSE, prob=prob1)
    hijos <- reproduccion(p[padres[1],], p[padres[2],], n)
    
    return(hijos)
  })
  
  aq<-data.frame(matrix(unlist(p1), ncol=n    ))
  p<-rbind(p,aq)
     
  clusterExport(cluster, c("p", "tam"))
  
  objaux<-parSapply(cluster, 1:tam, function(i) {   
    obj <- objetivo(unlist(p[i,]), valores)
    return(obj)
  })
  
  factaux<-parSapply(cluster, 1:tam, function(i) {   
    fact <- factible(unlist(p[i,]), pesos, capacidad)
    return(fact)
  })
  
  ~~~
 







<p align="justify">
La <a href="#fig1"> Figura 1</a>, presenta los tiempos de ejecución de la experimentación de acuerdo a la estrategia de implementación 
donde s es secuencial y p es paralelo. En todos los casos de la experimentación la estrategia secuencial consume al menos 10 veces
más tiempo que la implementación paralela. Para este caso no es necesario usar una prueba estadistica para demostrar dicha diferencia dado 
que en ningun caso las cajas de vigotes se interceptan.
</p>


<p align="center">
<div id="fig1" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_10/Graficas/P10.gif" height="70%" width="70%"/><br>
<b>Figura 1.</b> Variación de los tiempos de ejecución dependiendo de la implementación. 
</div>
</p>

<p align="justify">
La <a href="#fig2"> Figura 2</a>, muestra como va variando el GAP en diferentes casos, para los escenarios evaluados se 
presenta una tendencia lineal en el aumento de la función objetivo, al aumentar tanto el numero de generaciones como la población inicial. 
Se esperaba comportamientos no lineal en la grafica pero por los rangos de variación de la experimentación no fueron detectados. 
</p

<p align="center">
<div id="fig2" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_10/Graficas/Superficie.gif" height="70%" width="70%"/><br>
<b>Figura 2.</b> Superficie de respuesta . 
</div>
</p>


<p align="justify">
La <a href="#fig3"> Figura 3</a>, se cambio la probabilidad de que los padres pudieran reproducirse de acuerdo a su desempeño 
en de la función objetivo, en donde padres con mejor función objetivo tenian mayor probabilidad de transferir sus genes a nuevas 
generaciones.
Claro solo se considero la función objetivo y no se vigilo si dichos padres eran factibles o no para el problema de optimización 
analizado. Es decir se puede seleccionar hasta el momento padres que sobrepasen la capacidad de la mochila simplemente porque tienen
mejor desempeño de la función objetivo. 
Para la <a href="#fig3"> Figura 3</a>, cambio hace referencia al cambio de probabilidades segun la función objetivo (FO), mientras que uniforme
considera el caso anterior en donde cualquier padre de la población tiene la misma probabilidad de reproducirse. 
El hecho de cerrar la brecha a la diversificación al seleccionar elementos con FO mayor, trajo como repercusión una disminución en general
del desempeño promedio en la función objetivo. 

</p>



<p align="center">
<div id="fig3" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_10/Graficas/P10R1.gif" height="70%" width="70%"/><br>
<b>Figura 3.</b> Cambio de probabilidades en la selección de padres. 
</div>
</p>


 <p align="justify">  
En el  <a href="#tabl1"> Cuadro 1</a>,  contiene la prueba estadistica  de Wilcox, para sustentar los supuestos mencionados anteriormente,
Con un 95% o 99% de confianza se puede afirmar que no existe evidencia suficiente para rechazar Ha por lo cual se amenciona que estadisticamente  
hay diferencia en la estrategia de selección de padres utilizada. 
En donde mantener la probabilidad uniforme en la selección de los padres presenta mejor desempeño que variar la probabilidad de selección
de acuerdo a la FO.
</p>




| Factores  | Estadistico W | Valor <img src="https://latex.codecogs.com/gif.latex?p"/>|
| :-------: | ------:        | -----: |
| Cambio/Uniforme        | 6415         | 0.000   |
<caption>Cuadro 1. Pruebas estadísticas </caption><br>
<br>




<p align="justify">
La <a href="#fig4"> Figura 4</a>, se cambio la estrategia de supervivencia de los individuos seleccionando en primera instancia
a individuos factibles y posteriormente individuos no factibles. Para la probabilidad de los individuos no factibles
se genero similar que para los individuos factibles pero se multiplico por 0.55, asi la probabilidad de tener este tipo 
de individuos en la siguiente generación es poco probable. Donde anterior hace referencia a la estrategia 
usada anteriormente y supervivencia al cambio en dicha ideologia de prevalencia de los individuos. 

</p>





<p align="center">
<div id="fig4" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_10/Graficas/P10R2.gif" height="70%" width="70%"/><br>
<b>Figura 4.</b> Cambio de probabilidades en la supervivencia de los individuos. 
</div>
</p>


 <p align="justify">  
En el  <a href="#tabl1"> Cuadro 2</a>,  contiene la prueba estadistica  de Wilcox, para sustentar los supuestos mencionados anteriormente,
Con un 95% o 99% de confianza se puede afirmar que no existe evidencia suficiente para rechazar Ha por lo cual se amenciona que estadisticamente  
hay diferencia en la estrategia de selección de padres utilizada. 
En donde mantener la probabilidad con la estrategia anterior en la supervivencia de los individuos presenta mejor desempeño que variar la probabilidad de selección
de acuerdo a la FO.
</p>




| Factores  | Estadistico W | Valor <img src="https://latex.codecogs.com/gif.latex?p"/>|
| :-------: | ------:        | -----: |
| Cambio/Uniforme        | 74899         | 0.000   |
<caption>Cuadro 2. Pruebas estadísticas </caption><br>
<br>





## Bibliografía
1. S.E. Schaeffer. Práctica 10:Algoritmo Genético, <i>R paralelo: simulación & análisis de datos</i>, http://elisa.dyndns-web.com/teaching/comp/par/p10.html.













