#Jefry Navas
#Examen Primer Parcial

#Manga es el ancho de la lancha en pulgadas
y=c(98,98,98,93.5,93.5,96,90,94,96,92,91)
#HP son los caballos de fuerza
x=c(400,340,400,340,320,350,310,310,350,330,330)
#Velocidad maxima es la velocidad maxima de la lancha en millas por hora
z=c(47.5,44.9,47.3,44.5,44.5,42.5,45.8,42.8,43.2,45.3,47.7)


#LITERAL 1 Calcule el ancho de una lancha cuando la velocidad es de 30 millas por hora
reg1=lm(y~z)
y_est1=reg1$coefficients[1]+reg1$coefficients[2]*30
#El valor es de 95.3

#LITERAL 2 La bondad de ajuste del modelo generado en el literal 1. Haga un comentario sobre la bondad del ajuste.
summary(reg1)
#El valor es de 0.00108 La bondad de ajuste del modelo es muy malo porque no supera el umbral del 70%

#LITERAL 3 Calcule un estimador insesgado de la varianza para el literal 1.
anova(reg1)
#Valor del ECM es 9.1265


#LITERAL 4  ?Indica la prueba t que haya una relaci?n significante entre potencia y velocidad?. Use un nivel de significancia del 95% y el m?todo del valor cr?tico.
reg2=lm(x~z)

n_significancia = 0.05
valor_tabla = qt(n_significancia/2, 9, lower.tail = F) #Valor de la tabla de Distribucion t 
t = summary(reg2)[["coefficients"]][, "t value"] # Obtener los valores de t
tAbs = abs(t[2]) # el valor absoluto de t

relaciont = valor_tabla < tAbs #relacion segun valor de t

print(relaciont)

#Se acepta la hipostesis nula y se rechaza la hipotesis alternativa ya que el valor de t calculado es menor al visto en la tabla




#LITERAL 5 Para el literal anterior, determine el intervalo de confianza del coeficiente de regresi?n que representa a la pendiente, del modelo de regresi?n lineal simple.
b1=reg2$coefficients[2]
summary(reg2)
sb1 = 4.972
l1=b1+(2.262)*sb1
l2=b1-(2.262)*sb1
#El intervalo de confianza es de [-3.57,18.9]


#LITERAL 6 Pruebe si la relaci?n entre ancho de la lancha y velocidad es significante usando la prueba F, con un nivel de significancia del 99%. Use el m?todo del p-value (indique el valor). ?Cu?l es la conclusi?n?
summary(reg1)
#p-value: 0.9236<0.01 es falso por lo tanto Se acepta la hipotesis nula y se rechaza la hipotesis alternativa


#LITERAL 7 Para el literal anterior, indique el error est?ndar de estimaci?n del coeficiente de regresi?n que representa al corte con el eje y, del modelo de regresi?n lineal simple.
summary(reg1)
#El valor del error estandar es 3.021


#LITERAL 8 La revista Water Ski, estim? que el costo (en USD) de cada modelo se puede determinar por la funci?n: LaTeX: c\left(x\right)=3x^3+1c ( x ) = 3 x 3 + 1,   donde la variable independiente representa la velocidad. Determine cu?l es el costo de todos los sky para todos los fabricantes.
c=3*z^3+1
#El costo se guardo en la variable c

#LITERAL 9 Calcule, el ancho de una lancha cuando la potencia es de 400 HP y el costo es de 200 000 USD.
reg3=lm(y~x+c)
y_est3=reg3$coefficients[1]+reg3$coefficients[2]*400+reg3$coefficients[3]*200000
# El ancho de la lancha es de 102.8387

#LITERAL 10 Cu?l es la bondad de ajuste del modelo generado en el literal 9. y haga un comentario sobre la bondad del ajuste.
summary(reg3)
#La bondad de ajuste es de 0.7072 Es un modelo aceptable porque supera el umbral del 70%

#LITERAL 11 Para el literal anterior, se cumple que las perturbaciones siguen una distribuci?n normal. Justifique su respuesta, gr?ficamente. 
plot(reg3)
#Si cumple una distribucion normal porque en la grafica Normal Q-Q se observa que los datos estan alrededor de la linea de 45 grados


#LITERAL 12  Indique la ecuaci?n de regresi?n que permite predecir la potencia en funci?n del ancho y velocidad.
reg4=lm(x~y+z)
y_est4=reg4$coefficients[1]+reg4$coefficients[2]*y+reg4$coefficients[3]*z


#LITERAL 13 Para el literal anterior, basados en el criterio del residual estandarizado? Existe alg?n outlier?
rs=rstandard(reg4)
#Si existe un valor outlier en la posicion 2 ya que pasa del rango entre -2 y 2


#LITERAL 14 Indique los residuales eliminados estudentizados, del literal 12. Empleando como nivel de significancia 0.05, ?puede clasificarse cualquiera de estas observaciones como observaci?n at?pica? Explique.
rst=rstudent(reg4)
g=(length(x)-1)-2-1
n_significancia2 = 0.05
valor_tabla = qt(n_significancia2/2, g, lower.tail = F) #Valor de la tabla de Distribucion t 
# valor_tabla = 2.365 Todos los valores fuera del rango entre [-2.365,2.365]
valores_atipicos =rst[abs(rst)>valor_tabla]
# Si encontramos que la observacion 2 esta fuera del rango -4.072759


#LITERAL 15 Cu?les son los valores de influencia, del literal 12. ?Parece haber alguna observaci?n influyente en estos datos? Explique.
hi=influence(reg4)$hat
treshold=3*(2+1)/length(x)
#No existe ningun valor influyente ya que no supera el treshold


#LITERAL 16 Calcule la distancia de Cook para la regresi?n del literal 12. Qu? conclusi?n se obtiene?
cookreg4=cooks.distance(reg4)
#No se observa ningun valor influyente ya que ningun valor supera el umbral de 1
