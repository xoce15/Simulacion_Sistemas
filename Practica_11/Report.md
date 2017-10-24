# Práctica 11


# Introducción


El presente documento contiene el reporte de la práctica número 11 [\[1\]](#bibliograf%C3%ADa), 
sobre "frente de pareto". En dicha práctica se generán funciones polinomicas, las cuales serán consideradas 
como funciones objetivo, con la finalidad de determinar los puntos con mejor compromiso en cada función. 
Las funciones objetivo se contrapomen una con respecto a la otra, es decir no están correcionadas ni positivamente, 
ni negativamente pues si asi lo fuera seria un problema mono-objetivo.


### Especificaciones computacionales 
<p align="justify">
La presente práctica se realizó en una computadora 
con procesador Intel(R) Xeon(R) CPU E3-1245 v3 @3.4GHz 3.4GHz, con 16 GB de memoria RAM y 8 núcleos.
</p>

### Especificaciones experimentales 
<p align="justify">
Se consideran 200 soluciones iniciales, se experimenta con diferente cantidad de funciones objetivo empezando con dos y 
finalizando con 10. 
</p>




# Resultados


El codigo paralelizado para la frontera de pareto es presentado acontinuación:

~~~R
obj<-parLapply(cluster, 1:k, function(i){ 
    poli(vc, md, tc)
    }
  )
val1<-parSapply(cluster, 1:n, function(i){  
  af<-rep(0,k) 
  for (j in 1:k) { # para todos los objetivos
        af[j]<-eval(obj[[j]], sol[i,], tc)
         }
  return(af) 
  }
   )
stopCluster(cluster)
val<-t(val1)

d <- logical()
clusterExport(cluster, c("d"))
  for (i in 1:n) {
       clusterExport(cluster, c("i"))
  d<- parSapply(cluster, 1:n, function(j){
       domin.by(sign * val[i,], sign * val[j,], k)
      }
  )
  cuantos <- sum(d)
  dominadores <- c(dominadores, cuantos)
  no.dom <- c(no.dom, cuantos == 0) # nadie le domina
  }
~~~




<p align="justify">
La <a href="#fig1"> Figura 1</a>, presenta el porcentaje de dominados que hay con respecto a la cantidad de objetivos.
Mientras que <a href="#fig2"> Figura 2</a>, segrega  la cantidad de objetivos para poder visualizar mejor la variación 
interna de cada subgrupo. El tener diferentes limites en los ejes Y permite ver como se comporta cada subgrupo y no  
se pierde la apreciación como sucede en la <a href="#fig1"> Figura 1</a>, en donde la variación de las primeros subgrupos, niebla
a los restantes. 
Al aumentar las funciones objetivo el porcentaje de soluciones dominadas disminuye notablemente, en donde despues del septimo
las soluciones dominadas son 0. Lo que representa que al tener siete funciones objetivo todas las soluciones son no-dominadas. 
</p>


<p align="center">
<div id="fig1" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_11/Graficas/p11_violin_2.png" height="70%" width="70%"/><br>
<b>Figura 1.</b> Variación del porcentaje de dominados con respecto a la cantidad de objetivos. 
</div>
</p>


<p align="center">
<div id="fig2" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_11/Graficas/p11_violin_3.png" height="70%" width="70%"/><br>
<b>Figura 2.</b> Variación del porcentaje de dominados con respecto a la cantidad de objetivos subdivididos. 
</div>
</p>



<p align="justify">
La <a href="#fig3"> Figura 3</a>, para seleccionar los elementos dispersos de la frontera de pareto se ordeno dicha 
frontera de menor a mayor. Posteriormente se selecciono la mediana de dicho conjunto. Teniendo claro el elemento de la mitad del
conjunto inicial, se usa para seleccionar los primeros elementos a la mediana y al mencionado conjunto se le calcula nuevamente 
la mediana. Este proceso se repitio  pero ahora desde la mediana al ultimo elemento, para obtener otro punto. 
Claro tambien es posible un proceso más rapido y menos laborioso seleccionar del frente ordenado los elementos 25%,50%,75%,
una idea similar a los cuartiles(medianas) o al muestreo sistematico.
Tanto la primera como la segunda metodologia descritas se pueden extender a  <img src="https://latex.codecogs.com/gif.latex?k"/>
objetivos, por simplicidad grafica solo se presenta para dos funciones objetivo. 
La linea roja representa la regresión entre el eje Y y X usado con la finalidad de ayudar en la verificación de 
los metodos de selección implementados.
Tanto la idea de medianas como selección sistematico pueden ser el mismo si los porcentajes de selección en la estrategia
sistematica son iguales a (25%,50%,75%). 
La <a href="#fig4"> Figura 4</a>, fue diseñada usando la selección de puntos por medio del muestreo sistematico. 


</p>




<p align="center">
<div id="fig3" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_11/Graficas/p11_M_1.png" height="70%" width="70%"/><br>
<b>Figura 3.</b> Selección de elementos de la frontera de pareto por medianas
</div>
</p>


<p align="center">
<div id="fig4" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_11/Graficas/p11_M_2.png" height="70%" width="70%"/><br>
<b>Figura 4.</b> Selección de elementos de la frontera de pareto por muestreo sistematico
</div>
</p>




## Bibliografía
1. S.E. Schaeffer. Práctica 11:Frontera de pareto, <i>R paralelo: simulación & análisis de datos</i>, http://elisa.dyndns-web.com/teaching/comp/par/p11.html.







