## Introduccion

En este proyecto generamos un dataset a partir de la API de Riot Games mediante Cassiopeia (link_cassiopeia). Al dataset se le hacen algunas transformaciones
preparándolo para entrenar varios modelos de machine learning. Las técnicas empleadas en este proyecto son: avnnet, knn, Naïve-Bayes, Random Forest y XGBoost.
La finalidad es ser capaz de generar 


El código de la construcción de la base de datos está escrito en Python. La limpieza de datos y entrenamiento de los modelos se escribió en R.

### Requisitos
#### Python: 
- Cassiopeia
- Pandas


#### R: 
Paquetes:
- dplyr
- rpart
- tidyverse
- caret


### Construyendo el dataset

Para la construccion del dataset se ha empleado el framework Cassiopeia (https://cassiopeia.readthedocs.io/en/latest/#). Esta interfaz está diseñada
para Python y nos permite interactuar con la API de Riot Games empleando un sistema de tipos particular para facilitar las consultas. También ofrece
utilidades para almacenar esos datos.

El código está disponible en el markdown del archivo cassiopeia_lol_api.py. Igualmente se puede leer desde su respectivo archivo de Markdown.


### Limpieza del dataset

La ventaja de trabajar con esta clase de datasets es que por lo general estamos trabajando con datos limpios que no requieren gran cantidad de 
transformaciones. Uno de los objetivos del proyecto era demostrar que se podía utilizar una medida interpretada para sustituir un dato que hubiera
resultado costoso obtener a través de la API dado el acceso limitado que tenemos. 

Por tanto, se trató de deducir una medida para representar en el dataset la habilidad de un jugador con un campeón respectivo. Se explica

También se realizó una transformación en variables dummy de la mayoría de variables para facilitar el input de los datos para los modelos. En el 
código se detallan las funciones creadas para llevar este proceso a cabo.


### Entrenamiento de los modelos. 

En el github se encuentran los códigos para el entrenamiento de cada uno de los modelos. 