library(readr)
HistoricalQuotes_Limpios <- read_csv("6 SEMESTRE/ESTADISTICA PARA LA CIENCIA DE LA COMPUTACION/R studio/HistoricalQuotes_Limpios.csv")
View(HistoricalQuotes_Limpios)


#REGRESION CON TODAS LAS VARIABLES

y=HistoricalQuotes_Limpios$Volume #variable dependiente
z=HistoricalQuotes_Limpios$Date
x1=HistoricalQuotes_Limpios$`Close/Last`
x2=HistoricalQuotes_Limpios$Open
x3=HistoricalQuotes_Limpios$High
x4=HistoricalQuotes_Limpios$Low


#Representaci?n gr?fica de los datos.




#Representaci?n de correlaciones entre los datos.
matriz = cbind(y,x1,x2,x3,x4)
mm = cor(matriz) #crea la matriz de correlacionS

#Ejecuci?n de la tarea de regresi?n.
reg = lm(y~x1+x2+x3+x4)
summary(reg)
yest=reg$coefficients[1]+reg$coefficients[2]*x1+reg$coefficients[3]*x2+
  reg$coefficients[4]*x3+reg$coefficients[5]*x4
  
#Ecuacion de Regresi?n
#yest=136505984+1615128*x1+1987932*x2+10560498*x3-14952132*x4 


#Pruebas de significancia y bondad de ajuste del modelo.

#Bondad de ajuste del modelo es:  0.462, se encuentra bajo del umbral (0.70). 


############################# PRUEBA T ###################################

#b1
#Por p-value:
sig =0.05
0.1722 <sig   #False
#Por valor cr?tico de t:

g_libertad= length(x1)-4-1 #grados de libertad: n-p-1
tabla= qt(sig/2,g_libertad,lower.tail = F) #valor de la tabla T
t=summary(reg) [["coefficients"]][,"t value"]#OBTENER LOR VALORES DE T
tAbs = abs(t[2])

tabla < tAbs  #False

#No hay relacion 

#b2
#Por p-value:
0.0824<sig  #FALSE

#Por valor cr?tico de t:
t1Abs = abs(t[3])
tabla < t1Abs   #False

#No hay relacion 

#b3
#Por p-value:
2e-16 <sig  #TRUE
#Por valor cr?tico de t:
t2Abs = abs(t[4])
tabla < t2Abs   #TRUE

#Existe una relacion 

#b4
#Por p-value:
2e-16 <sig #TRUE
#Por valor cr?tico de t:
t3Abs = abs(t[5])
tabla < t3Abs  #TRUE

#Existe una relacion 

#########################PRUEBA F #############################
#--Por P-Value:
sig=0.05
2.2e-16 <sig    #TRUE

#Por valor cr?tico F:

dist_f =qf(sig,4,g_libertad,lower.tail =F )
dist_f < 541.4     #TRUE

#Mediante ambos m?todos de la prueba F se cumplenlas condiciones, por lo tanto,
#se rechaza la Ho y se acepta l a Ha, donde existe una relaci?n significante entre x1,x2,X3,X4 y y .


#Comprobaci?n de las suposiciones del modelo.

#Con la ayuda de la prueba t podemos obtener: 

#variables que no aportan al modelo
regno = lm (y~x1+x2)
summary(regno)

#variables que aportan al modelo
regsi = lm (y~x3+x4)
summary(regsi)



#Identificaci?n de outliers y valores influyentes.

###################Datos outliers###################
rs= rstandard(reg) #umbral -2 - 2
ol = rs[abs(rs)>2]    #127 datos atipicos
prueba_ol = rs[abs(rs)>2]
#outliers = which(abs(rs)>2)
rst=rstudent(reg) #prueba de comprobaci?n 
sig=0.05
grad_l= (length(x1)-1)-4-1 #(n-1)-p-1
valor_t= qt(sig/2,grad_l,lower.tail = F) #valor de la tabla T
#el nuevo rango del umbral va de -1.961 - 1.961
olrst =rst[abs(rst)>valor_t]  #134 datos atipicos

#################Datos Influyentes#################
hi=influence(reg)$hat #influyentes 
treshold= (3*(4+1))/ length(x1)
influyentes = hi[hi > treshold] #Existen 127 valores influyentes
cook= cooks.distance(reg) #distancia de cook
incook = cook[cook > 1] #Mediante la distancia de cook no se pudieron indentificar nuevos valores influyentes. 
