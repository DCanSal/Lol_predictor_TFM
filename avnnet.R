#LIBRERIAS
library(caret)
library(dplyr)

#FUNCIONES
#Una funcion rescatada de los apuntes de clase para evaluar la tasa de fallos a partir de la matriz
#de confusion.
#La funcion tiene un argumento, el dataframe resultante de las predicciones del modelo avvnet.
#El output de la funcion es el mismo dataframe con una columna añadida "Aciertos" con valores
#True o False 
aciertos_avnnet <- function(df){
  df<- as.data.frame(df)
  df$Aciertos <- F
  for (i in 1:length(df$Aciertos)){
    if ((df[i,1] == df[i,2])){
      df[i,3] <- T
    }
  }
  return(df)
}


#Import data
data_train         <- readRDS("./datosPreGame/data_train")
data_test          <- readRDS("./datosPreGame/data_test")
labels_data_train  <- readRDS("./datosPreGame/labels_train")
labels_data_test   <- readRDS("./datosPreGame/labels_test")

#Unimos labels con los datos de entrenamiento
data_train <- cbind(labels_data_train, data_train)

#avvnet expresa variables cuya varianza es casi 0. Se precisa controlarlas y usar un nuevo
#dataframe. Para ello contamos con la funcion nearZeroVar que genera un indice de variables
#que se eliminan del modelo para facilitar la computacion
indiceCerosTrain <- nearZeroVar(data_train)
data_train       <- data_train[,-indiceCerosTrain]

#Estos son los parametros definidos para el entrenamiento de ambas redes
grupos=4
sinicio=1234
repe=3
size=c(5)
decay=c(0.01)
repeticiones=3
itera=100



#Preparando Caret
control<-trainControl(method = "repeatedcv",
                      number=grupos,
                      repeats=repe,
                      savePredictions = "all",
                      classProbs=TRUE) 

# Aplico caret y construyo modelo
avnnetgrid <- expand.grid(size=size,
              decay=decay,
              bag=FALSE)
avnnet_1   <- train(make.names(labels_data_train)~.,
              data=data_train,
              method="avNNet",
              linout = FALSE,
              maxit=itera,
              repeats=repeticiones,
              trControl=control,
              tuneGrid=avnnetgrid,
              trace=T)


#Preparamos los datos test para igualarlos a los data_train y asi poder realizar las predicciones
data_test <- cbind(labels_data_test, data_test)
data_test <- data_test[,-indiceCerosTrain]
data_test <- data_test[,-1]

#Realizamos las predicciones
predictions  <- predict(avnnet_1, data_test)
predicciones <- as.data.frame(cbind(labels_data_test, predictions))
levels(predictions) <- c("0","1")

#Generamos la matriz de confusion y calculamos el RMSE para evaluar el modelo. 
matriz_confusion<- confusionMatrix(data=predictions, reference = labels_data_test)
residuals_preGame <- as.numeric(labels_data_test) - as.numeric(predictions)
RMSE_preGame      <- sqrt(mean(residuals_preGame^2))

#Una vez tenemos el primer modelo entrenado con el primer dataset, importamos el segundo
#dataset eliminando el espacio de los datos previos
rm(data_train, data_test)

#Importamos las versiones de los datasets completas
data_train       <- readRDS("./datosCompleto/data_train")  
data_test        <- readRDS("./datosCompleto/data_test")  

data_train       <- cbind(labels_data_train, data_train)  
indiceCerosTrain <- nearZeroVar(data_train)  
data_train       <- data_train[,-indiceCerosTrain]

#Entrenamos el segundo modelo con los mismos parametros del anterior
control    <- trainControl(method = "repeatedcv",
                      number=grupos,
                      repeats=repe,  
                      savePredictions = "all",
                      classProbs=TRUE)   

avnnetgrid <- expand.grid(size=size,
                           decay=decay,
                           bag=FALSE)  

avnnet_2   <- train(make.names(labels_data_train)~.,  
                  data=data_train,  
                  method="avNNet",  
                  linout = FALSE,  
                  maxit=itera,  
                  repeats=repeticiones,  
                  trControl=control,  
                  tuneGrid=avnnetgrid,  
                  trace=T)

#Utilizamos el mismo procedimiento para evaluar las predicciones y el RMSE
data_test <- cbind(labels_data_test, data_test)  
data_test <- data_test[,-indiceCerosTrain]  
data_test <- data_test[,-1]  

predictions  <- predict(avnnet_2, data_test)  
predicciones <- as.data.frame(cbind(labels_data_test, predictions))  

levels(predictions) <- c("0","1")  

matriz_confusion<- confusionMatrix(data=predictions, reference = labels_data_test)

residuals_completo <- as.numeric(labels_data_test) - as.numeric(predictions)  
RMSE_completo      <- sqrt(mean(residuals_completo^2))



