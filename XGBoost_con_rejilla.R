#El paquete XGBoost ha cogido mucha popularidad como un algoritmo agresivo capaz de encontrar interacciones en grandes cantidades de datos
#muy heterogéneos. No usaremos el paquete de caret para el entrenamiento de los modelos por lo que la parametrización corre a cargo del uso
#de una rejilla.

#LIBRERIAS
library(caret) #para generar la matriz de confusion
library(xgboost)

#FUNCIONES
#para el output de la rejilla se han escrito dos funciones muy simples que etiquetaran el csv resultante de las predicciones
#con la tasa de aciertos para escoger un modelo ganador.
#La función "Aciertos" tiene un argumento:
  
#  -   df: el dataframe de predicciones.

#Dado que el output de la función "predict" del paquete base de R devuelve para un objeto XGBoost la probabilidad respectiva
#de que la clasificación sea 0 y 1, la función "Aciertos" compara las probabilidades y la iguala al formato
#de las etiquetas del data_test.

#El output es el mismo dataframe con una columna codificada en 0 y 1 igual al formato de las etiquetas del data_test.
  Aciertos <- function(df){
    df$Aciertos <- F
    for (i in 1:length(df$Aciertos)){
      if ((df[i,2] > df[i,3] && df[i,1] == 1) || (df[i,2] < df[i,3] && df[i,1] == 0)){
        df[i,4] <- T
      }
    }
    return(df)
  }

#La función "tasa" calcula la tasa de aciertos a partir del dataframe resultante de la función "Aciertos". Tiene un solo argumento:
    
#-   dfaciertos: el dataframe resultante de la función "Aciertos"
#3La función calcula la tasa de predicciones acertadas por el modelo de XGBoost. El output es un número flotante.
    tasa <- function(dfaciertos){
      Trues  <- sum(dfaciertos$Aciertos == T)
      Falses <- sum(dfaciertos$Aciertos == F)
      result <- (Trues/(Trues+Falses))*100
      return(result)
    }

#Se escribe una última función para igualar el formato de las predicciones al que demanda la función "confusionMatrix"
#del paquete caret. La función "Aciertos_a_confusion" tiene un argumento:
    
#-   df: el dataframe de predicciones.
  
#Dado que el output de la función "predict" del paquete base de R devuelve para un objeto XGBoost la probabilidad respectiva de que la clasificación sea 0 y 1, la función "Aciertos_a_confusion" iguala el formato al que exige la función "confusionMatrix" para generar posteriormente una matriz de confusión.
  
    Aciertos_a_confusion <- function(df){
      df$Aciertos <- 0
      for (i in 1:length(df$Aciertos)){
        if ((df[i,2] > df[i,3] )){
          df[i,4] <- 1
        }
      }
      return(df)
    }

### REJILLA 
    
#Para encontrar los parámetros óptimos para entrenar un modelo XGBoost, se ha escrito un bucle for anidado de 4 niveles.
#Se importan los datos de entrenamiento y testeo y se inicia el bucle volcando los resultados de las predicciones en csv
#que se almacenan en un fichero

depth <- c(15, 13, 11, 9, 7, 5, 3)
eta <- c(20:2) / 100
subsample <- c(5:10) / 10
colsample <- c(5:10) / 10
  
  for (d in depth){
    for (e in eta){
      for (s in subsample){
        for (c in colsample){
          
          bst_model <- xgboost(data = predictors,
                               nfold = 10,
                               early_stopping_rounds =  30,
                               label = as.matrix(target),
                               num_class = 2,
                               max_depth = d,
                               eta = e,
                               nthread = 12,
                               subsample = s,
                               colsample_bytree = c,
                               min_child_weight = 1,
                               nrounds = 600, # PARA EL TESTEO LO HAS BAJADO A 20. ESTABA A 600
                               objective = "multi:softprob",
                               maximize = FALSE)
          
          predictions  <- predict(bst_model, as.matrix(data_test))
          results <- data.frame(predictions[(1:length(predictions)) %% 2 == 0],
                                predictions[(1:length(predictions)) %% 2 == 1])
          
          resultscheck <- cbind(labels, results)
          aciertos     <- Aciertos(resultscheck)
          tasacheck    <- tasa(aciertos)
          
          #Aqui va la linea de write.csv con los parametros y con el df aciertos. 
          write.csv(aciertos, file = paste0("./cross_validationXGBoost/results_depth_", d, "_eta_", e, "_subsample_", s, "_colsample_", c, "_tasa_", tasacheck, ".csv"), row.names = FALSE)
        }
      }
    }
  }

### Modelos XGBoost
    
#Una vez se conocen los parámetros de XGBoost para entrenar los modelos, se importan los correspondientes datos
#con el mismo método que hemos empleado para el resto de algoritmos y se entrenan los respectivos modelos.
#XGBoost no acepta dataframes de R, por lo que se han de convertir los dataframe a matrices.

  train             <- readRDS("./datosPreGame/data_train")
  test_preGame      <- readRDS("./datosPreGame/data_test")
  target            <- readRDS("./datosPreGame/labels_train")
  test_completo     <- readRDS("./datosPreGame/data_test")
  
  predictors <- data.matrix(train)
  rm(train)

#Los parámetros escogidos son los siguientes
  
  depth <- 13
  eta <- 0.2
  subsample <- 0.5
  colsample <- 0.9

#y procedemos a entrenar el modelo
    xgboost_1 <- xgboost(data = predictors,
                         nfold = 10,
                         early_stopping_rounds =  30,
                         label = as.matrix(target),
                         num_class = 2,
                         max_depth = depth,
                         eta = eta,
                         nthread = 12,
                         subsample = subsample,
                         colsample_bytree = colsample,
                         min_child_weight = 1,
                         nrounds = 600, # PARA EL TESTEO LO HAS BAJADO A 20. ESTABA A 600
                         objective = "multi:softprob",
                         maximize = FALSE)

#eliminamos del entorno de trabajo los predictores y el dataset de testeo para importar el dataset completo

rm(predictors, data_test)

train              <- readRDS("./datosCompleto/data_train")
test_completo      <- readRDS("./datosCompleto/data_test")


predictors <- data.matrix(train)
rm(train)

#Entrenamos el segundo modelo manteniendo los mismos parámetros

xgboost_2 <- xgboost(data = predictors,
                         nfold = 10,
                         early_stopping_rounds =  30,
                         label = as.matrix(target),
                         num_class = 2,
                         max_depth = depth,
                         eta = eta,
                         nthread = 12,
                         subsample = subsample,
                         colsample_bytree = colsample,
                         min_child_weight = 1,
                         nrounds = 600, # PARA EL TESTEO LO HAS BAJADO A 20. ESTABA A 600
                         objective = "multi:softprob",
                         maximize = FALSE)

#Una vez tenemos ambos modelos entrenados procedemos a realizar las predicciones respectivas sobre el data_test y a generar las matrices de confusión correspondientes pasando las predicciones previamente por la función "Aciertos_a_confusión"
 
predicciones_completo <- predict(xgboost_1,newdata = data.matrix(test_completo))
  
  results_1 <- data.frame(predicciones_completo[(1:length(predicciones_completo)) %% 2 == 0],
                          predicciones_completo[(1:length(predicciones_completo)) %% 2 == 1])
  results_1 <- cbind(labels, results_1)
  probs1    <- Aciertos_a_confusion(results_1)
  
  matriz_confusion_1    <- confusionMatrix(data = as.factor(probs1[,4]), reference = probs1[,1])
  predicciones_preGame  <- predict(xgboost_2,newdata =  as.matrix(test_preGame))
  
  results_2 <- data.frame(predicciones_preGame[(1:length(predicciones_preGame)) %% 2 == 0],
                          predicciones_preGame[(1:length(predicciones_preGame)) %% 2 == 1])
  results_2 <- cbind(labels, results_2)      
  probs2    <- Aciertos_a_confusion(results_2)
  
  matriz_confusion_2 <- confusionMatrix(data = as.factor(probs2[,4]), reference = probs2[,1])
 
  #Por último, en el caso de XGBoost se ha de realizar el cálculo del RMSE manualmente
  
  residuals_preGame  <- (as.numeric(labels) - 1) - as.numeric(probs2[,4])
  RMSE_preGame       <- sqrt(mean(residuals^2)) 
  
  residuals_completo <-(as.numeric(labels) - 1) - as.numeric(probs1[,4])
  RMSE_completo      <- sqrt(mean(residuals_completo^2)) 
