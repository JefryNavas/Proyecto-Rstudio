#Jefry Navas
#Examen Primer Parcial

#Precio
x1 = c(2900,3500,2900,3500,2300,2000,3000,1300,3200,1500,2600,1600,1800,1700,1200,1600,1000,1400,1000,600)
#Calidad

#Codificaci?n
#n numero de estados de la variable cualitativa = 3
#var. ficticias= n-1= 3-1 = 2

#           x2 x3
#exelente	  0	 0
#muy bueno	0	 1
#bueno		  1	 0

x2 = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,1,1)
x3 = c(0,1,0,0,0,0,0,1,1,0,1,1,1,1,1,1,0,1,0,0)
#Calificaci?n
y = c(86,85,82,81,81,81,79,78,72,83,83,82,80,75,73,73,70,70,67,66)

#Literal 1 Obtenga la ecuaci?n de regresi?n estimada en la que se emplee precio y calidad para predecir la calificaci?n final.
reg1=lm(y~x1+x2+x3)
y_est=reg1$coefficients[1]+reg1$coefficients[2]*x1+reg1$coefficients[3]*x2+reg1$coefficients[4]*x3

#y_est = 77.367+0.001737*x1-11.205317*x2-3.722674*x3

#Literal 2 Grafique residuales estandarizados. Existe alg?n outlier? 
rs=rstandard(reg1)
plot(y_est,rs)
#intervalo [-2,2]
# Si existe un outlier en la posicion 9 

#Literal 3 Indique los residuales eliminados estudentizados de estos datos. Empleando como nivel de significancia 0.05, ?puede clasificarse cualquiera de estas observaciones como observaci?n at?pica? Explique
rst=rstudent(reg1)
g=(length(x1)-1)-3-1
n_significancia = 0.05
valor_tabla = qt(n_significancia/2, g, lower.tail = F) #Valor de la tabla de Distribucion t 
valores_atipicos =rst[abs(rst)>valor_tabla]
#  Si existe un valor at?pico en la posicion 9


#Literal 4 Cu?les son los valores de influencia de estos datos. ?Parece haber alguna observaci?n influyente en estos datos? Explique.
hi=influence(reg1)$hat
treshold=3*(3+1)/length(x1)
# No existe ningun valor influyente ya que no sobrepasa el treshold 

#Literal 5 Calcule la distancia de Cook de estos datos. ?Es alguna de las observaciones una observaci?n influyente? Explique.
cookreg1=cooks.distance(reg1)
#No se observa ningun valor influyente ya que ningun valor supera el umbral de 1

#Literal 6 Qu? variable aporta mayor informaci?n al modelo y por qu??

summary(reg1)

#La variable que aporta mas al modelo es la x2 porque es menor a las p-value de la prueba t de las demas variables

#Literal 7 Determine el precio estimado de una caminadora Vision Fitness T9200 que ha sido catalogada como muy buena y tiene una calificaci?n de 78. Difiere en algo con el precio real? Si es as?, explique por qu??

reg2 =lm(x1~x2+x3+y)
y_est2=reg2$coefficients[1]+reg2$coefficients[2]*0+reg2$coefficients[3]*1+reg2$coefficients[4]*78

#El precio seria 2039.274, el precio si difiere

#Literal 8 Si se desea predecir el precio mediante la calificaci?n general, indique los coeficientes de regresi?n del modelo

reg3 = lm(x1~y)
summary(reg3)
#Coeficientes de regresi?n
#b0 = -5292.82 b1= 94.67

#Literal 9 Para el literal anterior determine la bondad de ajuste del modelo, junto con el coeficiente de correlaci?n y el p-valor de la regresi?n?. El modelo es significante?

#Bondad de ajuste es de 0.4182
corr = cor(y,x1)
#Coeficiente de correlacion 0.6466
# p-valor = 0.00206 el modelo si es significante

#Literal 10 Cu?l es el precio de una caminadora catalogada como muy buena con calificaci?n de 90?

y_est3=reg2$coefficients[1]+reg2$coefficients[2]*0+reg2$coefficients[3]*1+reg2$coefficients[4]*90

#El precio es 2696.26
