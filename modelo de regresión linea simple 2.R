# Jefry Navas
#Prueba 2


# ancho de la lancha 
y=c(98,98,98,93.5,93.5,96,90,94,96,92,91)

# caballos de fuerza
x=c(400,340,400,340,320,350,310,310,350,330,330)

#Velocidad maxima 
z=c(47.5,44.9,47.3,44.5,44.5,42.5,45.8,42.8,43.2,45.3,47.7)


#LITERAL 1: Si se desea predecir el ancho de una lancha en funci?n de la potencia del motor. Determine si existe alg?n valor influyente. Justifique su respuesta indicando el treshold.

reg = lm(y~x)

hi=influence(reg)$hat

treshold=6/length(x)

# El valor 0.14877 en la posicion 5 es un valor influyente porque es mayor al treshold
#Conclusi?n el treshold es igual a 0.54 quiere decir que cualquier valor que sea mayor al treshold es un valor influyente



#LITERAL 2: En el diagrama de dispersi?n del literal anterior, se observa alg?n outlier?. Justifique su respuesta, num?ricamente. 

plot(x,y)
rest=rstandard(reg)
plot(x,rest)


#No existe ningun valor at?pico porque en el residual estandarizado ningun valor pasa del rango entre -2,2


#LITERAL 3: Si se desea predecir la velocidad de la lancha tomando en cuenta el ancho m?ximo de la lancha. Compruebe gr?ficamente si se cumple con la propiedad de homocedasticidad. 

reg2=lm(z~y)
rest2=rstandard(reg2)
plot(y,rest2)

#No representa adecuadamente la relaci?n entre las variables por lo tanto no cumple la propiedad de homocedasticidad



#LITERAL 4: Para el literal anterior, se cumple afirma que las perturbaciones siguen una distribuci?n normal. Justifique su respuesta, gr?ficamente. 

plot(reg2)

#En la grafica Normal Q-Q se observa que los valores estan alrededor de la linea de 45 grados por lo tanto se cumple con la condici?n de normalidad

#LITERAL 5: Indique el cu?l es el valor del residual estandarizado de la Mastercraft X-1, para el literal 3.

#El residual estandarizado rest2 del literal 3 en la posicion 7 su valor es de 0.3870

#LITERAL 6: Si se desea conocer cu?l es la velocidad de una lancha tomando en cuenta su potencia. Indique el vector de los valores influyentes y en qu? gr?fica podr?a identificar la existencia de un valor influyente.

reg3=lm(z~x)

#Valores Influyentes
hi2=influence(reg3)$hat

#La grafica para observar un valor influyente es diagrama de dispersion
plot(x,z)







