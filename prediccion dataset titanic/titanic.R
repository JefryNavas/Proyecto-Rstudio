library(readr)
train_titanic <- read_csv("6 SEMESTRE/ESTADISTICA PARA LA CIENCIA DE LA COMPUTACION/R studio/parcial 2/train_titanic.csv")
View(train_titanic)

y = train_titanic$Survived
x1= train_titanic$Pclass
x2= train_titanic$Sex
x3= train_titanic$Age
x4= train_titanic$SibSp
x5 = train_titanic$Parch
x6 = train_titanic$Fare
#x7= train_titanic$Embarked
#x8= train_titanic$Embarked
#0 para mujeres y 1 para hombres.

x2 = (x2=='male')*1

#Limpieza de datos
#Se crea vector eliminando los NA que aparecen en la variable fecha, dejando 714 observaciones. 
train_titanic2 =  train_titanic[! is.na(train_titanic$Age),]
y2 = train_titanic2$Survived
x12= train_titanic2$Pclass
x22= train_titanic2$Sex
x32= train_titanic2$Age
x42= train_titanic2$SibSp
x52 = train_titanic2$Parch
x62 = train_titanic2$Fare

#0 para mujeres y 1 para hombres.

x22 = (x22=='male')*1

#Embarked
#C x7 = 0 x8 = 0
#S x7 = 1 x8 = 0
#Q x7 = 0 x8 = 1

#x72 = (x72=='S')*1
#x82 = (x82=='Q')*1

#mm = cbind(y,x1,x2,x3,x4,x5,x6,x7,x8)
mm = cbind(y2,x12,x22,x32,x42,x52,x62)
#Se busca si hay una correlación alta entre x32 (edad) con otra variable
correlac = cor(mm) #no se encuentra alta correlación con ninguna otra variable. 
#Se toma la correlación mas alta, en este caso con la x12 con una correlacion de -0.3692 
# se realiza la ecuación de regresión con las variables de x32 en base a x12
reg = lm(x32~x12)
yest = reg$coefficients[1]+reg$coefficients[2]*x1


#Se crea el bucle que buscará y reemplazará los valores de la x3 en base a la regresión obtenida
for(i in 1:nrow(train_titanic))
  if(is.na(train_titanic$Age[i])){
    train_titanic$Age[i] = reg$coefficients[1]+reg$coefficients[2]*x1[i]
    train_titanic$Age[i] = round(train_titanic$Age[i],0)
    x3[i] = train_titanic$Age[i]
  }

mm2 = cbind(y,x1,x2,x3,x4,x5,x6)
correlac2 = cor(mm2)#Correlación del dataframe sin Missing Values. 

set.seed(1)# semilla establecida para que los datos aleatorios no cambien.
aleatorio = sample(891,replace = F) #datos aleatorios de las observaciones
#Se obtiene la matriz aleatoria de los datos. 
trainrandom = train_titanic[c(aleatorio),] #Se coloca la matriz con números de filas aleatorizadas.

#training 70% de 891 = 624 datos
training =trainrandom[1:624,]

#TEST (30% de los datos) -> 267 datos
test =trainrandom[625:length(y),]

#Creamos las variables de entrenamiento para el modelo de regresion logistica
yE = training$Survived
x1E = training$Pclass
x2E = training$Sex
x3E = training$Age
x4E = training$SibSp
x5E = training$Parch
x6E = training$Fare

#Creamos las variables de prueba
yP = test$Survived
x1P = test$Pclass
x2P = test$Sex
x3P = test$Age
x4P = test$SibSp
x5P = test$Parch
x6P = test$Fare

x2P = (x2P=='male')*1

#Creamos el modelo de regresion logistica
reg = glm(yE~x1E+x2E+x3E+x4E+x5E+x6E,family=binomial())
# Formamos la ecuacion de regresión y le probamos con las variables de test
yest = exp(reg$coefficients[1]+reg$coefficients[2]*x1P+reg$coefficients[3]*x2P+reg$coefficients[4]*x3P+reg$coefficients[5]*x4P+reg$coefficients[6]*x5P+reg$coefficients[7]*x6P)/(1+exp(reg$coefficients[1]+reg$coefficients[2]*x1P+reg$coefficients[3]*x2P+reg$coefficients[4]*x3P+reg$coefficients[5]*x4P+reg$coefficients[6]*x5P+reg$coefficients[7]*x6P))

yest1 = round(yest)

error = (yP==yest1)*1
# Que tan bueno es mi modelo de regresión
accuracy = sum(error)/length(y2)

