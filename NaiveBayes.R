setwd("D:\\Universidad\\master\\documentacion\\Master BDBA\\Machine Learning\\TFM")


#LIBRERIAS
library(caret)
library(klaR)

#FUNCIONES

#Esta es una funcion que se ha reciclado de los scripts anteriores para evaluar el modelo previo a realizar la matriz de confusion
#Se explica en el readme. 
aciertos_nb <- function(df){
  df<- as.data.frame(df)
  df$Aciertos <- F
  for (i in 1:length(df$Aciertos)){
    if ((df[i,1] == df[i,2])){
      df[i,3] <- T
    }
  }
  return(df)
}

#Esta funcion es cortesia del usuario de Stack Overflow: "Cybernetic". Permite representar de una manera limpia y elegante
#los resultados de una matriz de confusion
draw_confusion_matrix <- function(cmtrx) {
  total <- sum(cmtrx$table)
  res <- as.numeric(cmtrx$table)
  # Generate color gradients. Palettes come from RColorBrewer.
  greenPalette <- c("#F7FCF5","#E5F5E0","#C7E9C0","#A1D99B","#74C476","#41AB5D","#238B45","#006D2C","#00441B")
  redPalette <- c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D")
  getColor <- function (greenOrRed = "green", amount = 0) {
    if (amount == 0)
      return("#FFFFFF")
    palette <- greenPalette
    if (greenOrRed == "red")
      palette <- redPalette
    colorRampPalette(palette)(100)[10 + ceiling(90 * amount / total)]
  }
# set the basic layout
layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
# create the matrix
  classes = colnames(cmtrx$table)
  rect(150, 430, 240, 370, col=getColor("green", res[1]))
  text(195, 435, classes[1], cex=1.2)
  rect(250, 430, 340, 370, col=getColor("red", res[3]))
  text(295, 435, classes[2], cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col=getColor("red", res[2]))
  rect(250, 305, 340, 365, col=getColor("green", res[4]))
  text(140, 400, classes[1], cex=1.2, srt=90)
  text(140, 335, classes[2], cex=1.2, srt=90)
# add in the cmtrx results
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
# add in the specifics
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cmtrx$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cmtrx$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cmtrx$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cmtrx$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cmtrx$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cmtrx$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cmtrx$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cmtrx$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cmtrx$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cmtrx$byClass[7]), 3), cex=1.2)
# add in the accuracy information
  text(30, 35, names(cmtrx$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cmtrx$overall[1]), 3), cex=1.4)
  text(70, 35, names(cmtrx$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cmtrx$overall[2]), 3), cex=1.4)
}



#DATOS
data_train         <- readRDS("./datosCompleto/data_train")
data_test          <- readRDS("./datosCompleto/data_test")
labels_data_train  <- readRDS("./datosCompleto/labels_train")
labels_data_test   <- readRDS("./datosCompleto/labels_test")

data_train <- cbind(labels_data_train, data_train)

#NaiveBayes no puede trabajar con tantas variables con varianza proxima a cero, por lo que se eliminan del dataset con la funcion
#nearZeroVar
indiceCerosTrain <- nearZeroVar(data_train)

data_train       <- data_train[,-indiceCerosTrain]


#NAIVE BAYES
model_1 <- train(labels_data_train~.,
                 data_train,
                 'nb',
                 trControl = trainControl(method = 'cv', number=10))


#Preparamos los datos de test para realizar las predicciones
data_test <- cbind(labels_data_test, data_test)
data_test <- data_test[,-indiceCerosTrain]
data_test <- data_test[,-1]

#Realizamos las predicciones
predictions  <- predict(model_1, data_test)
predicciones <- as.data.frame(cbind(labels_data_test, predictions))

#Realizamos la matriz de confusion con las predicciones
confusion_matriz<- confusionMatrix(data=predictions, reference = labels_data_test)

#Extraemos los residuales para evaluar el modelo
residuals_completo <- as.numeric(labels_data_test) - as.numeric(predictions)
RMSE_completo      <- sqrt(mean(residuals_completo^2))

#Eliminamos el dataset completo para importar el segundo dataset y realizar el mismo proceso
rm(data_train, data_test)

#importamos el dataset completo
data_train <- readRDS("./datosCompleto/data_train")
data_test  <- readRDS("./datosCompleto/data_test")

data_train <- cbind(labels_data_train, data_train)

#Realizamos el mismo proceso que anteriormente
indiceCerosTrain <- nearZeroVar(data_train)  
data_train       <- data_train[,-indiceCerosTrain]

#Procedemos a entrenar el segundo modelo con los mismos parametros
model_2 <- train(labels_data_train~.,  
                  data_train,  
                  'nb',  
                  trControl = trainControl(method = 'cv', number=10))

#Realizamos las predicciones correspondientes y evaluamos el modelo
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
