# Práctica 12


# Introducción


El presente documento contiene el reporte de la práctica número 12 [\[1\]](#bibliograf%C3%ADa), 
sobre "Redes Neuronales". En donde se requiere reconocer los numeros del cero al nueve con la red. 


### Especificaciones computacionales 
<p align="justify">
La presente práctica se realizó en una computadora 
con procesador Intel(R) Xeon(R) CPU E3-1245 v3 @3.4GHz 3.4GHz, con 16 GB de memoria RAM y 8 núcleos.
</p>

### Especificaciones experimentales 
<p align="justify">
Toda la experimentación se realiza con 30 replicas, se probaron diferentes conjuntos de prueba entre ellos fueron 300,600,1200 y 2400.
</p>

# Resultados
<p align="justify">
Como parte de los resultados de está práctica solo se paralelizo la sección de prueba de la red neuronal, dado
que la sección de prueba va cambiando recurrentemente los pesos de <img src="https://latex.codecogs.com/gif.latex?W"/>.
Para el funcionamiento de la red neuronal seria altamente provechoso poder paralelizar está sección, dado que se debe 
dedicar mayor esfuerzo a entrenar la red y no el caso contrario. Lastimosamente este proceso no se pudo realizar en esta práctica.
</p>



~~~R
 for ( kt in prueba){ 
   clusterExport(cluster,"kt")
 ti<- parSapply(cluster, 1:kt, function(t1) {
    d <- sample(0:tope, 1)
    pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
    correcto <- decimal(binario(d, n),n)
    salida <- rep(FALSE, n)
   for ( i in 1:n){  
  w <- neuronas[i,]
    deseada <- correcto[i]
    resultado <- sum(w * pixeles) >= 0
    salida[i] <- resultado
     }
  r <- min(decimal(salida, n), k) # todos los no-existentes van al final
   return(r==correcto)
})
}
~~~

<p align="justify">
La <a href="#fig1"> Figura 1</a>, presenta la variación del tiempo de ejecución para los diferentes conjuntos de prueba, en donde en los 
dos primeros subconjuntos la diferencia es minima pero despues para 1200 se presenta diferencias estadisticas significativas en los 
tiempos de ejecución. 
</p>



<p align="center">
<div id="fig1" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_12/Graficas/P12Tiempos.gif" height="70%" width="70%"/><br>
<b>Figura 1.</b> Comparación de tiempos paralelo contra secuencial. 
</div>
</p>


<p align="justify">
La <a href="#fig2"> Figura 2</a>, se presenta la variación de los tiempos de ejecución sin descriminar el conjunto de prueba que se 
uso en la red neuronal. En donde a primera vista pareciera que no hay un efecto significativo en toda la estrategia de ejecución
paralelo vs secuencial. Para aclarar está duda se realizó una prueba estadistica de Kruskal-Wallis, en donde se obtuvo un valor 
<img src="https://latex.codecogs.com/gif.latex?p"/> igual a 0.469, el cual comprueba que estadisticamente para está experimentación
no se encuentra diferencia estadistica.
</p>



<p align="center">
<div id="fig2" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_12/Graficas/p12_R1B.png" height="70%" width="70%"/><br>
<b>Figura 2.</b> Comparación de paralelo contra secuencial. 
</div>
</p>




<p align="justify">
La <a href="#fig3"> Figura 3</a>, muestra la variación de los porcentajes de aciertos para tres diferentes combinaciones de probabilidades,
siendo el primer caso el base, el segundo se le suma 0.025 unidades a "n" y "g" mientras que a "b" se le resta está misma cantidad. 
finalmente el caso tres se le suma 0.05 unidades a "n" y "g" mientras que a "b" se le resta está cantidad.
Para los dos casos adicionales de prueba se evidencio una reducción significativa en el porcentaje reconocimiento acertado de los digitos. 
Este efecto debe esta fuertemente relacionado con la variación presentada en el porcentaje del patron "b". 
</p>




<p align="center">
<div id="fig3" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_12/Graficas/p12_R1.png" height="70%" width="70%"/><br>
<b>Figura 3.</b> Variación de las probabilidades para los diferentes colores. 
</div>
</p>

<p align="justify">
La <a href="#fig4"> Figura 4</a>, ilustra como va variando el porcentaje de instancias correctamente clasificas para cada número en particular,
en donde se nota que el dato más complicado de intentificar para la red neuronal es el ocho. 
</p>


<p align="center">
<div id="fig4" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_12/Graficas/P12R.gif" height="70%" width="70%"/><br>
<b>Figura 4.</b> Variación porcentajes de acertados para cada número en particular. 
</div>
</p>


## Bibliografía
1. S.E. Schaeffer. Práctica 12: Redees Reuronales, <i>R paralelo: simulación & análisis de datos</i>, http://elisa.dyndns-web.com/teaching/comp/par/p12.html.

