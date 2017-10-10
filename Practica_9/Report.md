# Práctica 9


# Introducción


El presente documento contiene el reporte de la práctica número 9 [\[1\]](#bibliograf%C3%ADa), 
sobre "interacción entre particulas". En donde se tiene diferentes particulas distribuidas normalmente
en un contenedor unidimensional, los cuales se van movimiento a lo largo 
<img src="https://latex.codecogs.com/gif.latex?t"/> bajo una carga <img src="https://latex.codecogs.com/gif.latex?c"/>
y una fuerza de atracción  <img src="https://latex.codecogs.com/gif.latex?f"/>. 



### Especificaciones computacionales 
<p align="justify">
La presente práctica se realizó en una computadora 
con procesador Intel(R) Xeon(R) CPU E3-1245 v3 @3.4GHz 3.4GHz, con 16 GB de memoria RAM y 8 núcleos.
</p>

### Especificaciones experimentales 
<p align="justify">
Se tiene como valor  <img src="https://latex.codecogs.com/gif.latex?tmax"/>. igual a 100, las masas de los objetos 
siguen una distribucción lognormal, dado que se quiere muchas particulas con grandes masas y pocas particulas
con poco masa, dado que si tienen poca masa se moverán más rapido. Por lo cual se eligio tener pocos elementos pequeños. 
Dichas masas fueron acomodadas de menor a mayor para probar visualmente las hipotesis que se estarán probando 
a lo largo de está práctica. Cantidad de particulas 50.
Los radios siguen la siguiente relación para ser generados 0.02masa/max(masa).



# Resultados


<p align="justify">
La <a href="#fig1"> Figura 1</a>, muestra como se van moviendo las particulas en la cuadricula atraidas por las de mayor carga 
en donde las de mayor tamaño tienen movimientos menores que las de menor tamaño. 
</p>


  <p align="center">
<div id="fig1" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_9/Graficas/P9R2.gif" height="70%" width="70%"/><br>
<b>Figura 1.</b> Movimiento de las particulas dependiendo de la masa 
</div>
</p>

<p align="justify">
Mientras que la  <a href="#fig2"> Figura 2</a>, muestra como se va reducción los desplazamientos promedio con respecto al tamaño 
de la particula, en donde la media de los desplazamientos es 0.002.  
</p>

 <p align="center">
<div id="fig2" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_9/P9_grafico100.png" height="70%" width="70%"/><br>
<b>Figura 2.</b> Caja de bigotes para los desplazamientos 
</div>
</p>


<p align="justify">
La  <a href="#fig3"> Figura 3</a>, presenta una regresión lineal de la media de desplazamiento por uno sobre la masa, 
se usa está relación para mejorar el ajuste del modelo lineal. 
Se presentá tanto el modelo de regresión como su respectivo coeficiente de correlación, los coeficientes 
del modelo se redondearon a tres decimales. El coefciente de correlación aumenta con respecto aumenta 
 <img src="https://latex.codecogs.com/gif.latex?t"/>, pero en el maximo de <img src="https://latex.codecogs.com/gif.latex?t"/>
 el coeficiente de correlación es muy pobre. Lo que hace incapie que hace falta considerar otras variables en el modelo
 o permitir que tendan menor fluctuación (controlarlas), para aumentar este coeficiente. 
</p>



<p align="center">
<div id="fig3" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_9/Graficas/P9_Regresion.gif" height="70%" width="70%"/><br>
<b>Figura 3.</b> Regresión lineal para los desplazamientos
</div>
</p>

<p align="justify">
En la  <a href="#fig4"> Figura 4</a>, tiene los desplazamientos de cada una de las particulas a lo largo de toda la 
experimentación. Con la cual se puede percibir que las primeras particulas poseen movimientos mayores que las particulas 
mayores a 20. 

</p>
 <p align="center">
<div id="fig4" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_9/Graficas/P9_R.gif" height="70%" width="70%"/><br>
<b>Figura 4.</b> Desplazamiento por cada particula
</div>
</p>


<p align="justify">
Se habia mencionado con la <a href="#fig3"> Figura 3</a>, que "podia" existir algunas estrategias para aumentar 
el coeficiente de correlación de la regresión lineal. Para este caso se controlo la carga
<img src="https://latex.codecogs.com/gif.latex?c"/>, bajo una distribucción binomial (-1,1). 
En la  <a href="#fig5"> Figura 5</a>, se ha graficado este cambio, al controlar otra variable
la ganancia en el coeficiente no es muy alto alrededor de 10%, se puede explicar un poco más de variación del proceso
de desplazamiento pero aun hace falta modelar otra covariable para el modelo o simplemente 
utlizar estrategias de modelado estadistico no lineal para tener modelos más fiables. 
</p>

<p align="center">
<div id="fig5" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_9/Graficas/P9_Regresion_controC.gif" height="70%" width="70%"/><br>
<b>Figura 5.</b> Regresión controlando <img src="https://latex.codecogs.com/gif.latex?c"/>
</div>
</p>

<p align="justify">
La <a href="#fig6"> Figura 6</a>, ilustra los resultados al momento de cambiar el tipo de distribucción en la masa 
promedio de las particulas. Donde tanto en el eje X y Y, las medias están aproximadamente en el centro de la cuadricula. 
</p>

<p align="center">
<div id="fig6" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_9/P9_graficonor100.png" height="70%" width="70%"/><br>
<b>Figura 6.</b> Caja de vigotes para desplazamiento bajo la distribucción normal media 4, desviación estandar 0.5
</div>
</p>

<p align="justify">
La <a href="#fig7"> Figura 7</a>, muestra como se van desplazando las particulas usando una distribucción normal,
como se  ha diseñada los tamaños de las particulas tienen poca variación una con respecto a la otra.  Además se van aglomerando hacia el centro de la cuadricula, hasta converger en el  centro, dado la distribucción que se empleo
para designar las masas. 
</p>


<p align="center">
<div id="fig7" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_9/Graficas/P9R2_normal.gif" height="70%" width="70%"/><br>
<b>Figura 7.</b> Movimiento de las particulas con distribucción normal
</div>
</p>

# Conclusiones

<p align="justify">
El desplazamiento es afectado significativamente con respecto a la masa que tiene la particula, y dicha 
masa en la estrategia de creación. Al aumentar  <img src="https://latex.codecogs.com/gif.latex?t"/>
en promedio se mejora el modelo de regresión pero se han contemplado pocas variables por lo cual 
para explicar un alto porcentage de la varicación seria necesario una  <img src="https://latex.codecogs.com/gif.latex?t"/>:500, al menos o simplemente considerar otras covariables. El hecho de controlar  <img src="https://latex.codecogs.com/gif.latex?c"/> se pudo incrementar el coeficiente de correlación de la regresión. 
 </p> 
 
 
## Bibliografía
1. S.E. Schaeffer. Práctica 9:interacciones entre partículas, <i>R paralelo: simulación & análisis de datos</i>, http://elisa.dyndns-web.com/teaching/comp/par/p9.html.


