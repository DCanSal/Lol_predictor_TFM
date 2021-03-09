#CD
setwd("D:\\Universidad\\master\\documentacion\\Master BDBA\\Machine Learning\\TFM")

#LIBRERIAS
library(h2o)
library(randomForest)
library(caret)
library(bit64)

#FUNCIONES
aciertos_rf <- function(df){
  df$Aciertos <- F
  for (i in 1:length(df[,1])){
    if ((df[i,2] > df[i,3] && df[i,1] == 0) || (df[i,2] < df[i,3] && df[i,1] == 1)){
      df[i,4] <- T
    }
  }
  return(df)
}

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


#Importamos el dataset de la primera condicion del estudio
data_train         <- readRDS("./datosPreGame/data_train")
data_test          <- readRDS("./datosPreGame/data_test")
labels_data_train  <- readRDS("./datosPreGame/labels_train")
labels_data_test   <- readRDS("./datosPreGame/labels_test")

data_train <- cbind(labels_data_train, data_train)



#Para esta parte del proyecto, necesito emplear un servidor como h2o para aliviar la carga de los calculos. Iniciamos h2o
h2o.init()

#Iniciamos un random forest con los parametros base
rf_1 <-  h2o.randomForest(
  x = colnames(data_train)[2:ncol(data_train)],
  y = colnames(data_train)[1],
  training_frame = as.h2o(data_train, use_datatable = T),
  nfolds = 2,
  mtries = 4,
  ntrees = 500,
  verbose= T
)


#Eliminamos los datos preGame e importamos los datos completos para el segundo modelo
rm(data_train,data_test)

data_train         <- readRDS("./datosCompleto/data_train")
data_test          <- readRDS("./datosCompleto/data_test")

data_train <- cbind(labels_data_train, data_train)

#Volvemos a abrir la sesion h2o para el segundo analisis
h2o.init()

#Entrenamos el siguiente modelo. 
rf_2 <-  h2o.randomForest(
  x = colnames(data_train)[2:ncol(data_train)],
  y = colnames(data_train)[1],
  training_frame = as.h2o(data_train, use_datatable = T),
  nfolds = 2,
  mtries = 4,
  ntrees = 500,
  verbose= T
)

#para guardar el modelo que salga de ahí hay que hacer h2o.saveModel y para cargarlo h2o.loadModel.
### ESTAS SON LAS PREDICCIONES DEL MODELO COMPLETO
modelo_cargado_1 <- h2o.loadModel(path = "./rf_1_Completo/DRF_model_R_1599602428198_249")

predictions_2 <- h2o.predict(object = modelo_cargado_1,
                             newdata = as.h2o(data_test, use_datatable = T))

predicciones_bien_1 <- as.data.frame(predictions_2)

rm(data_test)



##  CARGAMOS EL SEGUNDO MODELO PARA PREDECIR. 
data_test          <- readRDS("./datosPreGame/data_test")

modelo_cargado_2 <- h2o.loadModel(path = "./rf_1_preGame/DRF_model_R_1599602428198_1")

predictions_1 <- h2o.predict(object = modelo_cargado_2,
                             newdata = as.h2o(data_test, use_datatable = T))
predicciones_bien_2 <- as.data.frame(predictions_1)

probs1 <- as.data.frame(cbind(as.factor(labels_data_test), as.factor(predicciones_bien_1[,1])))
probs2 <- as.data.frame(cbind(as.factor(labels_data_test), as.factor(predicciones_bien_2[,1])))

probs1[,2] <- as.factor(probs1[,2])
levels(probs1[,2]) <- c("0","1")

probs2[,2] <- as.factor(probs2[,2])
levels(probs2[,2]) <- c("0","1")

#Realizamos las matrices de confusion para evaluar los modelos. 
matriz_confusion_1 <- confusionMatrix(data      = as.factor(probs1[,2]),
                                      reference = labels_data_test)
matriz_confusion_2 <- confusionMatrix(data      = as.factor(probs2[,2]),
                                      reference = labels_data_test)
