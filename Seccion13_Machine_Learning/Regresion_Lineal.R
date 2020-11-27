# Title     : TODO
# Objective : TODO
# Created by: Adrian Hoyos
# Created on: 4/09/2020

#install.packages('corrgram')
#install.packages('corrplot')
#install.packages('caTools') # paquete para el modelo de prubas

# Regresion lineal
# sep es el separadr
datos <- read.csv('Curso_R/Seccion13_Machine_Learning/student-mat.csv', sep = ';')

head(datos)

# La intencion de este script es predecir que nota sacara en G3 teniendo en cuenta las demas variables

# Verificar si tiene datos nulos
any(is.na(datos))

library(ggplot2)
library(ggthemes)
library(dplyr)
library(corrplot)
library(corrgram)
library(caTools)

# Ahora procederemos a hacerlas predicciones, para ello, sacaremos las columnas numericas
columnas.numericas <- sapply(datos, is.numeric)

head(columnas.numericas)

# Correlacionar los datos con las columnas numericas, es decir una vs todas
datos.correlacion <- cor(datos[,columnas.numericas])
print(datos.correlacion)

# Grafico para interpretas mejor los datos
grafico <- corrplot(datos.correlacion, method = 'color')

# En caso de querer relacionar TODOS los datos, independientemente si son numericas o no
corrgram(datos)

# para que los valores aleatorios al a horade descomponer entre entrenamiento y prueba sean los mismosque voy a hacer yo
set.seed(80)

# 70% para entrenamiento y el restante osea el 30% para testear
ejemplo <- sample.split(datos$G3, SplitRatio = 0.7)

#Entrenamiento
entrenamiento <- subset(datos, ejemplo=TRUE)

#Tests
pruebas <- subset(datos, ejemplo==FALSE)

#Aqui creamos el modelo de regresion lineal para hacer las predicciones
modelo <- lm(G3 ~. , entrenamiento)

#Summary permite revisar el modelo que se ha creado
print(summary(modelo))

#Errores de las estimaciones, para eso son los residuos
#Los residuos son la diferencia entre el valor real y el estimado
residuos <- residuals(modelo)
class(residuos)
residuos <- as.data.frame(residuos)
head(residuos)
ggplot(residuos, aes(residuos)) + geom_histogram(fill='blue', alpha=0.5)

# Graficos a nivel estadistico
plot(modelo)

# Hacer predicciones con el modelo entrenado
predicciones <- predict(modelo, pruebas)
resultados <- cbind(predicciones, pruebas$G3)
head(resultados)
colnames(resultados) <- c('prediccion', 'real')

#Conversion a dataframe
resultados <- as.data.frame(resultados)
head(resultados)

#Ver que hay valores negativos
min(resultados)

class(resultados)

#Con esta funcion hacemos que los valores negativos sean convertidos a 0
cero <- function (x) {
   if(x < 0){
     return(0)
   } else {
     return(x)
   }
}

#Aplicar la funcion cero
resultados$prediccion <- sapply(resultados$prediccion, cero)

#Observar que ya el minimo no son numeros negativos si no 0
min(resultados$prediccion)

#Error cuadrático medio
error <- mean((resultados$real-resultados$prediccion)^2)
error

#Otra forma de verificar que el modelo se ajusta en porcentaje
sse <- sum((resultados$prediccion - resultados$real)^2)
sst <- sum( (mean(datos$G3)-resultados$real)^2 )
resultado <- 1 - sse/sst
resultado
