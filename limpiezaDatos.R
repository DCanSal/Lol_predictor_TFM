#LIBRERIAS
library(dplyr)
library(rpart)
library(tidyverse)
library(rpart.plot)
library(caret)


#FUNCIONES
#Esta es una funciion encapsulada dentro de una funcion mas grande: tramificador
#El objetivo de esta funcion es etiquetar las categorias resultantes del tramificador.
#El output de la funcion tramificador es una transformacion de una variable continua a una cantidad
#variable de categorias usando arboles de decision. Los atributos de esta funcion son:
# - arrayIndices: como int. Es el resultado de los tramos que devuelve la funcion tramificador.
# - arrayEntrada: el nombre de la variable original a partir de la que se generan etiquetas.
# - arraySalida: un array vacio que se rellena con las etiquetas de esta misma funcion.
#
#
#El output de la funcion es el propio array de salida que termina de ser procesado en la funcion
#en la que esta anidada. El array consiste en una serie de etiquetas que sustituyen a la variable
#continua que entra en la funcion.

PonerEtiquetas <- function(arrayIndices,arrayEntrada,arraySalida){
  for (numeroTramo in (1:(length(arrayIndices) + 1))){
    
    i <- 1
    if(numeroTramo == 1)
    {
      nombreEtiqueta <- paste0("[0,", arrayIndices[1], sep="")
      nombreEtiqueta <- paste(nombreEtiqueta, ")", sep="")
      for(numero in arrayEntrada)
      {
        if(numero < arrayIndices[1])
          arraySalida[i] <- nombreEtiqueta
        i <- i + 1
      }
    }
    else if(numeroTramo == length(arrayIndices) + 1)
    {
      nombreEtiqueta <- paste0("[>=", arrayIndices[numeroTramo-1], sep="")
      nombreEtiqueta <- paste(nombreEtiqueta, ")", sep="")
      for(numero in arrayEntrada)
      {
        if(numero >= arrayIndices[numeroTramo - 1]){
          arraySalida[i] <- nombreEtiqueta
        }
        i <- i + 1
      }
      
    }
    else if(numeroTramo > 1 && numeroTramo <= length(arrayIndices))
    {
      nombreEtiqueta <- paste0("[", arrayIndices[numeroTramo-1], sep="")
      nombreEtiqueta <- paste(nombreEtiqueta, ",", sep="")
      nombreEtiqueta <- paste0(nombreEtiqueta, arrayIndices[numeroTramo], sep="")
      nombreEtiqueta <- paste(nombreEtiqueta, ")", sep="")
      for(numero in arrayEntrada)
      {
        if(numero >= arrayIndices[numeroTramo - 1] && numero < arrayIndices[numeroTramo]){
          arraySalida[i] <- nombreEtiqueta
        }
        i <- i + 1
      }
      
    }
  }  
  return(arraySalida)
}

#La funcion tramificador tiene el objetivo de convertir una variable continua en una variable
#factor a partir del paquete rpart. La funcion rpart utiliza arboles de decision para agrupar
#los valores de una variable continua paraa generar una clasificacion basada en el indice de
#Gini. Los atributos de la funcion son: 
#
# - varObj: la variable objetivo del estudio sobre la que ponderar la variable que ha de ser
#           tramificada.
# - varTramificada: la variable sobre la que vamos a realizar la agrupacion.
# - df: el dataframe del que provienen las variables.
#
#El output de la funcion es una variable factor construida a partir de una variable continua.

tramificador <- function(varObj, varTramificada, df){
  tree <- rpart(as.character(varObj)~varTramificada, data = df, control = rpart.control(minsplit=2, minbucket=1, cp=0.001))
  
  splitis       <- tree$splits #Esta es una matriz que contiene los splits del árbol
  indice        <- splitis[,4] #Aquí tenemos la columna de los indices del arbol. 
  indiceOrdenado<- sort(indice, decreasing = F)
  
  
  arraySalida <- c()
  arraySalidaBien <- PonerEtiquetas(as.integer(indiceOrdenado),varTramificada,arraySalida)
  
  return(as.factor(arraySalidaBien))
}


##El mayor problema que tiene utilizar esta clase de algoritmos de clasificacion es respetar
##las dimensiones de los objetos test y train. Vamos a generar dos funciones para asegurarnos
##de que al final del proceso obtendremos dos objetos con las mismas dimensiones para hacer 
##xgboost

#Escribimos un dumificador flexible para generar columnas dummy.

dummyGen <- function(nueva_tabla, tabla_original, columna_dumificada, valores_columna){
  
  nombre_nuevo <- paste(columna_dumificada, "_", sep = "")
  
  i <- 1
  for (val in valores_columna){
    nueva_variable <- data.frame(matrix(0, nrow(nueva_tabla), 1))
    nueva_variable[tabla_original[,columna_dumificada] == val, 1] <- 1
    colnames(nueva_variable) <- paste0(nombre_nuevo, i)
    nueva_tabla <- cbind(nueva_tabla,nueva_variable)
    i <- i + 1
  }
  
  return(nueva_tabla)
}


limpiaDatos <- function(tabla_raw) {
  
  #lo primero es crear una tabla donde ir guardando los resultados
  nueva_tabla <- data.frame(matrix(0, nrow(tabla_raw), 1))
  
  #Las primeras variables del datset se quedan tal y como estan puesto que ya 
  #estan en formato binario. 
  
  nueva_tabla$blue_wins       <- tabla_raw$blue_wins
  nueva_tabla$first_dragon    <- tabla_raw$first_dragon
  nueva_tabla$first_baron     <- tabla_raw$first_baron
  nueva_tabla$first_blood     <- tabla_raw$first_blood
  nueva_tabla$first_inhibitor <- tabla_raw$first_inhibitor
  nueva_tabla$first_tower     <- tabla_raw$first_tower
  nueva_tabla$first_rifth     <- tabla_raw$first_rifth
  
  #tower_kills: el rango de la variable tower_kills se encuentra entre 0 y 11. La funcion dummygen
  #generara las columnas correspondientes
  
  tower_kills <- c('0','1','2','3','4','5' ,'6','7','8','9','10','11')
  nueva_tabla <- dummyGen(nueva_tabla, tabla_raw, "tower_kills", tower_kills)
  
  #dragon_kills: el rango de la variable dragon_kills se encuentra entre 0 y 6. 
  #Los datos significativos, hacen que reduzcamos a 0 y 4. 
  
  dragon_kills <- c('0','1','2','3','4')
  nueva_tabla  <- dummyGen(nueva_tabla, tabla_raw, "dragon_kills", dragon_kills)
  
  #baron_kills: el rango de la variable baron_kills se encuentra entre 0 y 2. 
  baron_kills  <- c('0','1','2')
  nueva_tabla  <- dummyGen(nueva_tabla, tabla_raw, "baron_kills", baron_kills)
  
  #inhibitor_kills: el rango de la variable inhibitor_kills va de 0 a 7.
  #los valores significativos, en cambio, van de 0 a 3. 
  inhibitor_kills <- c('0','1','2','3')
  nueva_tabla     <- dummyGen(nueva_tabla, tabla_raw, "inhibitor_kills", inhibitor_kills)
  
  #rifth_kills: el rango de esta variable va de 0 a 2.
  rifth_kills <- c('0','1','2')
  nueva_tabla <- dummyGen(nueva_tabla, tabla_raw, "rifth_kills", rifth_kills)
  
  #champ_pick: las variables champ_pick y ban son las más complejas. Para evitar el sobreajuste
  #mantendremos todas las observaciones que aparezcan a pesar de que no todas sean significativas.
  
  champ_pick_1  <- dput(levels(tabla_raw$champ_pick_1))
  nueva_tabla    <- dummyGen(nueva_tabla, tabla_raw, "champ_pick_1", champ_pick_1)
  
  champ_pick_2  <- dput(levels(tabla_raw$champ_pick_2))
  nueva_tabla    <- dummyGen(nueva_tabla, tabla_raw, "champ_pick_2", champ_pick_2)
  
  champ_pick_3  <- dput(levels(tabla_raw$champ_pick_3))
  nueva_tabla    <- dummyGen(nueva_tabla, tabla_raw, "champ_pick_3", champ_pick_3)
  
  champ_pick_4  <- dput(levels(tabla_raw$champ_pick_4))
  nueva_tabla    <- dummyGen(nueva_tabla, tabla_raw, "champ_pick_4", champ_pick_4)
  
  champ_pick_5  <- dput(levels(tabla_raw$champ_pick_5))
  nueva_tabla    <- dummyGen(nueva_tabla, tabla_raw, "champ_pick_5", champ_pick_5)
  
  champ_pick_6  <- dput(levels(tabla_raw$champ_pick_6))
  nueva_tabla    <- dummyGen(nueva_tabla, tabla_raw, "champ_pick_6", champ_pick_6)
  
  champ_pick_7  <- dput(levels(tabla_raw$champ_pick_7))
  nueva_tabla    <- dummyGen(nueva_tabla, tabla_raw, "champ_pick_7", champ_pick_7)
  
  champ_pick_8  <- dput(levels(tabla_raw$champ_pick_8))
  nueva_tabla    <- dummyGen(nueva_tabla, tabla_raw, "champ_pick_8", champ_pick_8)
  
  champ_pick_9  <- dput(levels(tabla_raw$champ_pick_9))
  nueva_tabla    <- dummyGen(nueva_tabla, tabla_raw, "champ_pick_9", champ_pick_9)
  
  champ_pick_10 <- dput(levels(tabla_raw$champ_pick_10))
  nueva_tabla    <- dummyGen(nueva_tabla, tabla_raw, "champ_pick_10", champ_pick_10)
  
  #Como hemos indicado en la variable anterior, aplicaremos el mismo proceso para la variable
  #ban
  
  ban_1        <- dput(levels(tabla_raw$ban_1))
  nueva_tabla  <- dummyGen(nueva_tabla, tabla_raw, "ban_1", ban_1)
  
  ban_2        <- dput(levels(tabla_raw$ban_2))
  nueva_tabla  <- dummyGen(nueva_tabla, tabla_raw, "ban_2", ban_2)
  
  ban_3        <- dput(levels(tabla_raw$ban_3))
  nueva_tabla  <- dummyGen(nueva_tabla, tabla_raw, "ban_3", ban_3)
  
  ban_4        <- dput(levels(tabla_raw$ban_4))
  nueva_tabla  <- dummyGen(nueva_tabla, tabla_raw, "ban_4", ban_4)
  
  ban_5        <- dput(levels(tabla_raw$ban_5))
  nueva_tabla  <- dummyGen(nueva_tabla, tabla_raw, "ban_5", ban_5)
  
  ban_6        <- dput(levels(tabla_raw$ban_6))
  nueva_tabla  <- dummyGen(nueva_tabla, tabla_raw, "ban_6", ban_6)
  
  ban_7        <- dput(levels(tabla_raw$ban_7))
  nueva_tabla  <- dummyGen(nueva_tabla, tabla_raw, "ban_7", ban_7)
  
  ban_8        <- dput(levels(tabla_raw$ban_8))
  nueva_tabla  <- dummyGen(nueva_tabla, tabla_raw, "ban_8", ban_8)
  
  ban_9        <- dput(levels(tabla_raw$ban_9))
  nueva_tabla  <- dummyGen(nueva_tabla, tabla_raw, "ban_9", ban_9)
  
  ban_10       <- dput(levels(tabla_raw$ban_10))
  nueva_tabla  <- dummyGen(nueva_tabla, tabla_raw, "ban_10", ban_10)
  
  ##Maestrias
  valores_unicos_1  <- sort(table(tabla_raw$maestria_1), decreasing = TRUE)[1:length(levels(tabla_raw$maestria_1))]
  maestria_1        <- dput(names(valores_unicos_1))
  nueva_tabla       <- dummyGen(nueva_tabla, tabla_raw, "maestria_1", maestria_1)
  
  valores_unicos_2  <- sort(table(tabla_raw$maestria_2), decreasing = TRUE)[1:length(levels(tabla_raw$maestria_2))]
  maestria_2        <- dput(names(valores_unicos_2))
  nueva_tabla       <- dummyGen(nueva_tabla, tabla_raw, "maestria_2", maestria_2)
  
  valores_unicos_3  <- sort(table(tabla_raw$maestria_3), decreasing = TRUE)[1:length(levels(tabla_raw$maestria_3))]
  maestria_3        <- dput(names(valores_unicos_3))
  nueva_tabla       <- dummyGen(nueva_tabla, tabla_raw, "maestria_3", maestria_3)
  
  valores_unicos_4  <- sort(table(tabla_raw$maestria_4), decreasing = TRUE)[1:length(levels(tabla_raw$maestria_4))]
  maestria_4        <- dput(names(valores_unicos_4))
  nueva_tabla       <- dummyGen(nueva_tabla, tabla_raw, "maestria_4", maestria_4)
  
  valores_unicos_5  <- sort(table(tabla_raw$maestria_5), decreasing = TRUE)[1:length(levels(tabla_raw$maestria_5))]
  maestria_5        <- dput(names(valores_unicos_5))
  nueva_tabla       <- dummyGen(nueva_tabla, tabla_raw, "maestria_5", maestria_5)
  
  valores_unicos_6  <- sort(table(tabla_raw$maestria_6), decreasing = TRUE)[1:length(levels(tabla_raw$maestria_6))]
  maestria_6        <- dput(names(valores_unicos_6))
  nueva_tabla       <- dummyGen(nueva_tabla, tabla_raw, "maestria_6", maestria_6)
  
  valores_unicos_7  <- sort(table(tabla_raw$maestria_7), decreasing = TRUE)[1:length(levels(tabla_raw$maestria_7))]
  maestria_7        <- dput(names(valores_unicos_7))
  nueva_tabla       <- dummyGen(nueva_tabla, tabla_raw, "maestria_7", maestria_7)
  
  valores_unicos_8  <- sort(table(tabla_raw$maestria_8), decreasing = TRUE)[1:length(levels(tabla_raw$maestria_8))]
  maestria_8        <- dput(names(valores_unicos_8))
  nueva_tabla       <- dummyGen(nueva_tabla, tabla_raw, "maestria_8", maestria_8)
  
  valores_unicos_9  <- sort(table(tabla_raw$maestria_9), decreasing = TRUE)[1:length(levels(tabla_raw$maestria_9))]
  maestria_9        <- dput(names(valores_unicos_9))
  nueva_tabla       <- dummyGen(nueva_tabla, tabla_raw, "maestria_9", maestria_9)
  
  valores_unicos_10 <- sort(table(tabla_raw$maestria_10), decreasing = TRUE)[1:length(levels(tabla_raw$maestria_10))]
  maestria_10       <- dput(names(valores_unicos_10))
  nueva_tabla       <- dummyGen(nueva_tabla, tabla_raw, "maestria_10", maestria_10)
  
  return(nueva_tabla)
}


#DATOS
datos <- readRDS("datos40K")

colnames(datos) <- c("blue_wins","first_dragon","first_baron","first_blood","first_inhibitor",
                     "first_tower","first_rifth","tower_kills","dragon_kills","baron_kills",
                     "inhibitor_kills","rifth_kills","champ_pick_1","champ_pick_2","champ_pick_3",
                     "champ_pick_4","champ_pick_5","champ_pick_6","champ_pick_7","champ_pick_8",
                     "champ_pick_9","champ_pick_10","ban_1","ban_2","ban_3","ban_4","ban_5","ban_6",
                     "ban_7","ban_8","ban_9","ban_10","maestria_1","maestria_2","maestria_3",
                     "maestria_4","maestria_5","maestria_6","maestria_7","maestria_8",
                     "maestria_9","maestria_10")
varObjBin <- datos$blue_wins

datos[,c(1:7,9:32)] <- lapply(datos[,c(1:7,9:32)], factor)


datos$maestria_1  <- tramificador(varObjBin, datos$maestria_1,  datos)
datos$maestria_2  <- tramificador(varObjBin, datos$maestria_2,  datos)
datos$maestria_3  <- tramificador(varObjBin, datos$maestria_3,  datos)
datos$maestria_4  <- tramificador(varObjBin, datos$maestria_4,  datos)
datos$maestria_5  <- tramificador(varObjBin, datos$maestria_5,  datos)
datos$maestria_6  <- tramificador(varObjBin, datos$maestria_6,  datos)
datos$maestria_7  <- tramificador(varObjBin, datos$maestria_7,  datos)
datos$maestria_8  <- tramificador(varObjBin, datos$maestria_8,  datos)
datos$maestria_9  <- tramificador(varObjBin, datos$maestria_9,  datos)
datos$maestria_10 <- tramificador(varObjBin, datos$maestria_10, datos)

saveRDS(datos, "datos_nodummy40K")




datosLimpios <- limpiaDatos(datos)
#Ahora tenemos que eliminar la columna extra que se genera en la funcion de limpieza.
datosLimpios <- datosLimpios[,-1]

saveRDS(datosLimpios, "datos")

trainIndex <- createDataPartition(datosLimpios$blue_wins, p=0.8, list=FALSE)
data_train <- datosLimpios[trainIndex,]
data_test  <- datosLimpios[-trainIndex,]

#Guardamos los labels por separado
labels_data_test  <- data_test[,1]
labels_data_train <- data_train[,1]

#Y los eliminamos de ambos conjuntos
data_train <- data_train[,-1]
data_test  <- data_test[,-1]


#Guardamos la version con los datos completos en una carpeta
saveRDS(data_train, "./datosCompleto/data_train")
saveRDS(data_test, "./datosCompleto/data_test")
saveRDS(labels_data_train, "./datosCompleto/labels_train")
saveRDS(labels_data_test, "./datosCompleto/labels_test")

#El objetivo de este proyecto es construir modelos de prediccion empleando dos versiones del 
#dataset. Una vez hemos guardado el dataset completo, procedemos a extraer las columnas de la
#segunda condicion del proyecto; esto es, quedarnos solo con las variables que podemos obtener
#antes de que la partida de comienzo siquiera dando lugar al dataset pre-game.
data_train <- data_train[,-c(1:34)]
data_test  <- data_test[,-c(1:34)]

#Y hacemos lo mismo para los datos del pre-game
saveRDS(data_train, "./datosPreGame/data_train")
saveRDS(data_test, "./datosPreGame/data_test")
saveRDS(labels_data_train, "./datosPreGame/labels_train")
saveRDS(labels_data_test, "./datosPreGame/labels_test")




