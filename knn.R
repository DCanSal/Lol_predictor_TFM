#LIBRERIAS 
library(caret)


#FUNCIONES
#Dado que las predicciones de un modelo knn se devuelven como dos columnas con la probabilidad
#calculadada de pertenecer a cada categoría, se escribe una función para igualar el formato de
#las predicciones con el de las etiquetas del data_test.
#La función aciertos_a_confusión tiene un argumento:

#  -   df: el dataframe de predicciones.

#El ouput de la función es el mismo dataframe con una columna con las predicciones del modelo
#igualadas al formato de las etiquetas.
aciertos_a_confusion <- function(df){
  df <- as.data.frame(df)
  df$aciertos <- 0
  for (i in 1:length(df$aciertos)){
    if ((df[i,3] > df[i,2])){
      df[i,4] <- 1
    }
  }
  return(df)
}


#Importamos los datos del primer dataset
data_train         <- readRDS("./datosPreGame/data_train")
data_test          <- readRDS("./datosPreGame/data_test")
labels_data_train  <- readRDS("./datosPreGame/labels_train")
labels_data_test   <- readRDS("./datosPreGame/labels_test")

data_train <- cbind(labels_data_train, data_train)

#nuestro dataset al tener varias columnas dummy tiene variables con varianza próxima a cero.
#el paquete caret tiene una funcion "nearZeroVar" para generar un indice de variables que se
#eliminan del dataset
indiceCerosTrain <- nearZeroVar(data_train)

data_train       <- data_train[,-indiceCerosTrain]

#KNN
#Tenemos que preparar los parametros de caret para entrenar el modelo knn
control <- trainControl(method = "repeatedcv",
                        repeats = 3,
                        verboseIter = T)

knn_1 <- train(labels_data_train~.,
               data_train,
               'knn',
               trControl = control,
               tuneLength = 20)

#igualamos el dataset de testeo a las condiciones del dataset de entrenamiento aplicando el mismo
#indice.
data_test <- cbind(labels_data_test, data_test)
data_test <- data_test[,-indiceCerosTrain]
data_test <- data_test[,-1]

labels_data_test <- as.numeric(labels_data_test) - 1

#Preparamos las predicciones
predictions  <- predict(knn_1, 
                        newdata = data_test,
                        k = 9,
                        type = "prob", 
                        prob=T)

predicciones <- cbind(labels_data_test, predictions)

predicciones <- as.data.frame(cbind(labels_data_test, predictions))

probs1 <- aciertos_a_confusion(predicciones)

#Generamos la matriz de confusion y evaluamos el RMSE del modelo. 
matriz_confusion <- confusionMatrix(data      = as.factor(probs1[,4]),
                                    reference = as.factor(probs1[,1]))

residuals_completo <- as.numeric(labels_data_test) - as.numeric(probs1[,4])
RMSE_completo      <- sqrt(mean(residuals_completo^2))

#Eliminamos el data_train y data_test del primer dataset
rm(data_train, data_test)


#Importamos datos del segundo dataset para realizar las predicciones 
data_train         <- readRDS("./datosCompleto/data_train")
data_test          <- readRDS("./datosCompleto/data_test")
labels_data_train  <- readRDS("./datosCompleto/labels_train")
labels_data_test   <- readRDS("./datosCompleto/labels_test")

#Aplicamos el mismo metodo de eliminacion de variables
indiceCerosTrain <- nearZeroVar(data_train)
data_train       <- data_train[-indiceCerosTrain]

#Procedemos a entrenar el segundo modelo con el dataset completo
control <- trainControl(method = "repeatedcv",  
                         repeats = 3,  
                         verboseIter = T)  

knn_2   <- train(labels_data_train~.,  
                data_train,  
                'knn',  
                trControl = control,  
                tuneLength = 20)


#Con el segundo modelo entrenado, seguimos el mismo procedimiento para evaluar el modelo y las predicciones
predictions  <- predict(knn_2,   
                         newdata = data_test,  
                         k = 9,  
                         type = "prob",   
                         prob=T)  

predicciones <- cbind(labels_data_test, predictions)  

predicciones <- as.data.frame(cbind(labels_data_test, predictions))  

probs1 <- aciertos_a_confusion(predicciones)  

matriz_confusion <- confusionMatrix(data      = as.factor(probs1[,4]),  
                                     reference = as.factor(probs1[,1]))  

residuals_completo <- as.numeric(labels_data_test) - as.numeric(probs1[,4])  
RMSE_completo      <- sqrt(mean(residuals_completo^2))

                
