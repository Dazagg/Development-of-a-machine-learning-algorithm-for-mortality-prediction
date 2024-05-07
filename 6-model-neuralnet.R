library(pROC)
library(corrplot)
library(caret)
library(randomForest)
library(e1071)
library(xgboost)


tu_dataframe = Dummy_encoding_5644_67



# Remove highly correlated variables
matriz_correlacion <- cor(tu_dataframe[, -which(names(tu_dataframe) == "muerte")])
corrplot(matriz_correlacion, method = "circle")
pares_correlacion_alta <- which(abs(matriz_correlacion) > 0.7 & upper.tri(matriz_correlacion), arr.ind = TRUE)
columnas_a_eliminar <- unique(c(pares_correlacion_alta[, 1], pares_correlacion_alta[, 2]))
tu_dataframe_filtrado <- tu_dataframe[, -columnas_a_eliminar]

# Create train and test sets
datos=tu_dataframe_filtrado
set.seed(124)
indice_entrenamiento <- createDataPartition(datos$muerte, p = 0.7, list = FALSE)
datos_entrenamiento <- datos[indice_entrenamiento, ]
datos_prueba <- datos[-indice_entrenamiento, ]
real_data <- factor(datos_prueba$muerte)




# Instalar y cargar la librería 'neuralnet'
install.packages("neuralnet")
library(neuralnet)

# Supongamos que 'datos' es tu conjunto de datos y 'categoria' es la variable a predecir
# Asegúrate de tener tus datos preparados y la variable objetivo (en este caso 'categoria') adecuadamente codificada

# Convertir 'categoria' a factor si no lo es
datos_entrenamiento$categoria <- as.factor(# Instalar y cargar la librería 'neuralnet'
install.packages("neuralnet")
library(neuralnet)

# Supongamos que 'datos' es tu conjunto de datos y 'categoria' es la variable a predecir
# Asegúrate de tener tus datos preparados y la variable objetivo (en este caso 'categoria') adecuadamente codificada

# Convertir 'categoria' a factor si no lo es
datos_entrenamiento$muerte <- as.factor(datos_entrenamiento$muerte)

# Definir la fórmula para el modelo
variables_predictoras <- names(datos_entrenamiento)[names(datos_entrenamiento) != "muerte"]

# Construir la fórmula automáticamente
formula <- as.formula(paste("muerte ~", paste(variables_predictoras, collapse = " + ")))

# Configuración de la red neuronal
modelo <- neuralnet(formula,
                    data = datos_entrenamiento,
                    hidden = c(5, 2),
                    linear.output = TRUE,
                    stepmax = 10000) 

# Mostrar resumen del modelo
print(modelo_neuralnet)

# Predecir con el conjunto de datos de prueba (supongamos que 'datos_prueba' es tu conjunto de datos de prueba)
predicciones <- predict(modelo_neuralnet, newdata = datos_prueba, type = "response")

# Mostrar matriz de confusión
confusion_matrix <- table(Real = datos_prueba$categoria, Prediccion = as.factor(round(predicciones)))
print(confusion_matrix)
$categoria)

# Definir la fórmula para el modelo
formula <- categoria ~ variable1 + variable2 + ... + variableN

# Configuración de la red neuronal
configuracion <- list(
  hidden = c(5, 3),  # Número de neuronas en cada capa oculta
  learningrate = 0.01,  # Tasa de aprendizaje
  algorithm = "backprop",  # Algoritmo de entrenamiento ("backprop" para retropropagación)
  act.fct = "logistic",  # Función de activación de las neuronas ("logistic" para la sigmoide)
  linear.output = FALSE  # Si se debe aplicar una función lineal a la salida
)

# Crear el modelo neuralnet
modelo_neuralnet <- neuralnet(formula, data = datos, hidden = configuracion$hidden,
                              learningrate = configuracion$learningrate,
                              algorithm = configuracion$algorithm,
                              act.fct = configuracion$act.fct,
                              linear.output = configuracion$linear.output)

# Mostrar resumen del modelo
print(modelo_neuralnet)

# Predecir con el conjunto de datos de prueba (supongamos que 'datos_prueba' es tu conjunto de datos de prueba)
predicciones <- predict(modelo_neuralnet, newdata = datos_prueba, type = "response")

# Mostrar matriz de confusión
confusion_matrix <- table(Real = datos_prueba$categoria, Prediccion = as.factor(round(predicciones)))
print(confusion_matrix)








