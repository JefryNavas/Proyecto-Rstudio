#Jefry Navas

#Ingreso de los Datos
x= c(98,98,98,93.5,93.5,96,90,94,96,92,91)
y= c(400,340,400,340,320,350,310,310,350,330,330)
z= c(47.5,44.9,47.3,44.5,44.5,42.5,45.8,42.8,43.2,45.3,47.7)

#Literal 1 La potencia del motor para una lancha que tiene 60 pulgadas de ancho.
reg = lm(y~x)
summary(reg)

y_60 = reg$coefficients[1]+reg$coefficients[2]*60


#Respuesta es 58.7



#Literal 2 Bondad de ajuste del modelo generado en el literal 1. y haga un comentario sobre la bondad del ajuste.


b0=reg$coefficients[1]
b1=reg$coefficients[2]

y_estimada=b0+b1*x
SCR=sum((y_estimada-mean(y))^2)

SCE=sum((y-y_estimada)^2)

STC=SCR+SCE

r2=SCR/STC



#Comentario
#Bondad de ajuste = 0.5794 El modelo no se ajusta bien a los datos

#Literal 3 De acuerdo con los datos, ?cree usted que las lanchas m?s potentes, tienen mayor velocidad?. Justifique su respuesta a trav?s de un gr?fico y mediante una ecuaci?n de regresi?n.


#Diagrama de dispersion
plot(y,z)

reg2 = lm(z~y)
summary(reg2)

#Sacamos la ecuacion
b02=reg2$coefficients[1]
b12=reg2$coefficients[2]

yest2=b02+b12*y
abline(reg2)

# 



#Literal 4 Calcule la suma total de cuadrados para el literal 3.

SCR2=sum((yest2-mean(z))^2)

SCE2=sum((z-yest2)^2)

STC2=SCR2+SCE2

#Literal 5 Calcule el coeficiente de correlaci?n muestral para el literal 1.

corr = cor(x,y)

#Las varialbes si tienen correlaci?n

#Literal 6 ?Indica la prueba t que haya una relaci?n significante entre potencia y velocidad?. Use un nivel de significancia del 95% y el m?todo del valor cr?tico.

n_significancia = 0.5
valor_tabla = qt(n_significancia/2, 9, lower.tail = F) #Valor de la tabla de Distribucion t 
t = summary(reg2)[["coefficients"]][, "t value"] # Obtener los valores de t
tAbs = abs(t[2]) # el valor absoluto de t

relaciont = valor_tabla < tAbs #relacion segun valor de t

print(relaciont)

# Se demuestra que si hay una relacion significante entre la potencia y la velocidad


#Literal 7 Pruebe si la relaci?n entre ancho de la lancha y velocidad es significante usando la prueba F, con un nivel de significancia del 99%. Use el m?todo del p-value. ?Cu?l es la conclusi?n?

reg3 = lm(z~x)
summary(reg3)



n_significancia2 = 0.1
dist_f = qf(n_significancia2, 1, 9, lower.tail=F)#Valor de la tabla de Distribucion F 

relacionf = dist_f < 0.9236  #relacion segun valor de F

print(relacionf)

# p-value = 0.006508 es menor que 0.1 por lo cual podemos decir que la hip?tesis nula ser? rechazada y la hip?tesis alternativa aceptada. Demostrando adem?s que el ancho de la lancha si tiene relacion con la velocidad