library(xgboost)

# 1- Carga de datos
train_data <- train_subsample_group2
test_data <- test_subsample_group2
real_data <- factor(test_subsample_group2$muerte)
X_entrenamiento <- train_data[, -which(names(train_data) == "muerte")]
y_entrenamiento <- train_data$muerte
X_prueba <- test_data[, -which(names(test_data) == "muerte")]
y_prueba <- test_data$muerte

dtrain <- xgb.DMatrix(data = as.matrix(X_entrenamiento), label = y_entrenamiento)
dtest <- xgb.DMatrix(data = as.matrix(X_prueba), label = y_prueba)

# Define parameters of the model
parametros <- list(
  objective = "binary:logistic",  # Para problemas de clasificación binaria
  eval_metric = "logloss"          # Métrica de evaluación
)

# Generate the model
modelo <- xgboost(params = parametros, data = dtrain, nrounds = 100)

# Predictions
predicciones <- predict(modelo, dtest)
predicciones <- round(predicciones)
predicciones <- factor(predicciones)

# Validate the model
predicciones_numericas <- as.numeric(as.character(predicciones)) 
matriz_confusion <- table(ifelse(predicciones_numericas > 0.5, 1, 0), y_prueba)
precision <- sum(diag(matriz_confusion)) / sum(matriz_confusion)
cat("Precisión del modelo en el conjunto de prueba:", precision, "\n")


matriz_confusion <- confusionMatrix(predicciones, real_data)
print(matriz_confusion)

roc_objeto <- roc(test_data$muerte, as.numeric(predicciones))
auc_roc <- auc(roc_objeto)
print(paste("Área bajo la curva ROC (AUC-ROC):", auc_roc))
