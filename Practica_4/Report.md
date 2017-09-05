# Introducción

<p align="justify">
El presente documento contiene el reporte de la práctica número 4, sobre "Diagramas de Voronoi". A grandes razgos y sin entrar 
a profundidad metodologica: Se tiene un area rectangular de <img src="https://latex.codecogs.com/gif.latex?n\times&space;n"/>,  
discretizado en una rejilla de <img src="https://latex.codecogs.com/gif.latex?n"/> cuadrículas por cada eje. 
En este contenedor se le lanza uniformemente 
<img src="https://latex.codecogs.com/gif.latex?k"/> semillas, las cuales generarán igual 
número de zonas de voronoi, a dichas semillas se les permite crecer con respecto a que tan cerca 
se encuentren a una casilla vacia. Teniendo las 
<img src="https://latex.codecogs.com/gif.latex?k"/> zonas creadas en el contenedor, se escoge una zona al azar, 
donde se comenzará a propagar una fractura en el material. Si la grieta está ubicada en el centro tiene 
una probabilidad de desplazamiento pero si está cerca a la frontera, su probabilidad de penetración será mayor a la anterior.     
</p>


### Especificaciones computacionales 
La presente práctica se realizó en una computadora 
con procesador Intel(R) Xeon(R) CPU E3-1245 v3 @3.4GHz 3.4GHz, con 16 GB de memoria RAM y 8 núcleos.

### Especificaciones experimentales 

Se considerán diferentes cuadrículas siendo estás: 40,50,60,70,80,90, mientras que la cantidad de semillas usadas en cada una de las cuadrícula son 12,15,18,21,24,36,48.

Se usaron tres tipos diferentes de generación de semillas.
<ol>
<li>  Bajo una distribución normal con media <img src="https://latex.codecogs.com/gif.latex?n/2"/> y varianza <img src="https://latex.codecogs.com/gif.latex?n/8"/>
<li>  Bajo una distribucion Poisson con  <img src="https://latex.codecogs.com/gif.latex?\lambda=1/5.5n"/>
<li> Una mezcla entre exponencial y normal creadas usando las distribuciones acumuladas, mostrada a continuación para mayor claridad.  
</ol>

```
    s<-seq(from=0, to=10, by=(10/(n/2)))
    aux1<-vector()
    aux2<-vector()
    tr2<-pexp(s)
    for( i in 2: length(tr2))
    {
      aux1[i-1]<-tr2[i]-tr2[i-1]
    }
    s1<-seq(from=-3, to=3, by=(6/(n/2)))
    aux2<-vector()
    tr3<-pnorm(s1)
    for( i in 2:length(tr3) )
    {
      aux2[i-1]<-tr3[i]-tr3[i-1]
    }
    aux2
    aux3<-c(aux1,aux2)
```
 
# Resultados 

<p align="justify">
Como primer objetivo  se estudio  de manera sistemática el efecto que el número de semillas y 
el tamaño de la zona tienen en la distribución de los largos de las grietas que se forman. Las semillas se considerán 
una microestructura particular del material. 
En la <a href="#fig1"> Figura 1</a>, presenta diferentes crecimientos del material, 
en donde la zona en color azul celeste asemeja una semilla y dicho material tiene  <img src="https://latex.codecogs.com/gif.latex?k"/>  semillas. 
</p>

<p align="center">
<div id="fig1" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_4/Graficas/q.png" height="70%" width="70%"/><br>
<b>Figura 1.</b> Representación de semillas
</div>
</p>

<p align="justify">
En la <a href="#fig2"> Figura 2</a>, se muestra como va variando el largo de la grieta con respecto 
al número de semillas puestas en la rejilla, así como el tamaño de la misma. Para 
todos los casos se presentarón datos atípicos siendo grietas mayores a 200 unidades metrícas. 
Con el aumento paulatino de las cuadrículas las grietas tienden a disminuir su tamaño. Este resultado puede estar 
dado principalmente por el efecto significativo que tiene la cantidad de semillas sobre la cuadrícula, 
puesto que en la <a href="#fig3"> Figura 3</a>, el tamaño de las cajas de vigotes tiende a aumentar si se 
incrementa <img src="https://latex.codecogs.com/gif.latex?k"/>. Algo de esperarse por la forma de simular en fenómeno.  
</p>


<p align="center">
<div id="fig2" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_4/Graficas/P4_Todos.png" height="70%" width="70%"/><br>
<b>Figura 2.</b> Variaciones sin filtro
</div>
</p>


<p align="center">
<div id="fig3" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_4/Graficas/P4_Segmentado.png" height="70%" width="70%"/><br>
<b>Figura 3.</b> Variaciones del largo de la fractura separado por semillas
</div>
</p>




<p align="justify">
En la  <a href="#tabl1"> Tabla 1</a>, se presentá la prueba estadística Kruskal-Wallis, la 
cual apoya las posturas menciondas anteriormente. Siendo en el caso estudiado tanto la 
variable <img src="https://latex.codecogs.com/gif.latex?n"/> y <img src="https://latex.codecogs.com/gif.latex?k"/>, 
altamente significativa (valor <img src="https://latex.codecogs.com/gif.latex?p"/> <0.05). 
Dado el caso, si se quiere disminuir  el largo de la grieta se recomendaría usar tamaños de cuadrículas grandes 
y una cantidad de semillas bajas. 
</p>



| Factores  | Estadistico KS | Valor <img src="https://latex.codecogs.com/gif.latex?p"/>|
| :-------: | ------:        | -----: |
| n         | 358.37         | 0.000   |
| k         | 281.26         | 0.000 |
<caption>Tabla 1. Pruebas estadísticas </caption><br>

* * *

## Distribución de las semillas de otras formas

<p align="justify">
 En está sección se realiza modificaciones en el código, el cual anteriormente 
 generaba <img src="https://latex.codecogs.com/gif.latex?k"/> semillas distribuidas uniformemente en un contenedor <img src="https://latex.codecogs.com/gif.latex?n"/> especifico, 
 pero ahora se considerará otros formas de generación de semillas. 
 En la <a href="#fig4"> Figura 4</a>, Se presenta la variación del largo de la grieta con respecto al tamaño de la cuadrícula y la cantidad de semillas variando las distribuciones para cada caso. 
 Se ha probado que la griega es altamente sensible al tamaño del contenedor, la cantidad de semillas y pareciera ser también afectada por el tipo de distribución usada en la 
 generación de las semillas. Es de esperarse que si las semillas fueron  generadas con una distribución uniforme, el variar las estrategias de generación deberá ser significativo por consiguiente.  
</p>

<p align="center">
<div id="fig4" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_4/Graficas/P4R1_2t%20.gif" height="70%" width="70%"/><br>
<b>Figura 4.</b> Variaciones del largo de la fractura separado por semillas variando la distribución.
</div>
</p>



<p align="justify">
En la  <a href="#tabl2"> Tabla 2</a>, se presentá la prueba estadística Kruskal-Wallis, 
para tener un sustento verás y no especulación gráfica. Dicha prueba  apoya las posturas menciondas anteriormente, 
tanto las variables <img src="https://latex.codecogs.com/gif.latex?n"/>, <img src="https://latex.codecogs.com/gif.latex?k"/> 
y distribución altamente significativa (valor <img src="https://latex.codecogs.com/gif.latex?p"/> <0.05). 
</p>




| Factores  | Estadistico KS | Valor <img src="https://latex.codecogs.com/gif.latex?p"/> |
| :-------: | ------:        | -----: |
| n         | 103.949         | 0.000   |
| k         | 61.9242         | 0.000 |
| Distri    | 221.939         | 0.000 |
<caption>Tabla 2. Pruebas estadísticas </caption><br>








<p align="justify">
En la <a href="#fig5"> Figura 5</a>, se presentá como varia el largo de la grieta considerando 
solo el tamaño de la cuadrícula por cada distribución. En donde el peor caso se vislumbra en la 
primera distribución, donde las gráficas de violin tiene mayor aglomeración de datos a comparación de las otras distribuciones. 
</p>

<p align="center">
<div id="fig5" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_4/Graficas/P4R1_2distr%20.gif" height="70%" width="70%"/><br>
<b>Figura 5.</b> Variaciones del largo de la fractura por tamaño de cuadrícula
</div>
</p>



<p align="justify">
En la <a href="#fig6"> Figura 6</a>, se presenta como van generandose los diferentes diagramas de Voronoi con una 
rejilla de 1000<img src="https://latex.codecogs.com/gif.latex?\times&space;"/>1000,En la <a href="#fig7"> Figura 7</a>, 
la rejilla es de 600, finalmente la <a href="#fig8"> Figura 8</a>, es de 200. 
Se van generando exporadicamente los diagramas, como es de esperarse en los dos casos anteriores 
la longitud de la fractura es afectada significativamente por el tamaño de la rejilla y la cantidad de semillas 
usadas en la experimentación. 
</p>

<p align="center">
<div id="fig6" style="width:300px; height=200px">
<img src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_4/Graficas/P4R2_Voronoi%20.gif" height="70%" width="70%"/><br>
<b>Figura 6.</b> Aparición a lo largo del tiempo de semillas
</div>
</p>



<center>
<table>
<tbody><tr>
<td>
<a onblur="try{parent.deselectBloggerImageGracefully();} catch(e) {}" href="Fig7"><img style="display: block; margin: 0px auto 10px; text-align: center; cursor: pointer; width: 320px; height: 240px;"
src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_4/Graficas/P4R2_Voronoi%20.gif" alt="" border="0" /></a>
<div style="text-align: center;">
<span style="font-size:small;"><i style="color: black;">Figura 7. Tamaño del contenedor de 600</i></span></div></td>
<td>
<a onblur="try{parent.deselectBloggerImageGracefully();} catch(e) {}" href="Fig8"><img style="display: block; margin: 0px auto 10px; text-align: 
center; cursor: pointer; width: 320px; height: 240px;"
src="https://github.com/xoce15/Simulacion_Sistemas/blob/master/Practica_4/Graficas/P4R2_Voronoi%20.gif" alt="" border="0" /></a>
<div style="text-align: center;">
<span style="font-size:small;"><i style="color: black;">Figura 8. Tamaño del contenedor de 200</i></span></div></td>
</tr>
</tbody></table>
</center> 





# Conclusiones

<p align="justify">
Para poder disminuir el tamaño de las fracturas del material se recomienda tener menor 
número de semillas para que la propagación de la misma sea dificil. Es claro que para algunos materiales 
lo que se desea es lo contrario, por seguridad de las personas, generando la necesidad de usar 
mayor número de semillas para que el material se fracture completamente. 
</p>
