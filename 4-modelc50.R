library(C50)


# MODEL C50

# 1- Data load
train_data <- train_subsample_group2
test_data <- test_subsample_group2
real_data <- factor(test_subsample_group2$muerte)
datos <- train_data
datos$muerte <- as.factor(datos$muerte)

# Crear el modelo C5.0
modelo_c50 <- C5.0(datos[, -which(names(datos) == "muerte")], datos$muerte)
test_data$muerte <- as.factor(test_data$muerte)
predicciones <- predict(modelo_c50, newdata = test_data)


matriz_confusion <- confusionMatrix(predicciones, real_data)
roc_objeto <- roc(test_data$muerte, predictions_RF_ROUND)
auc_roc <- auc(roc_objeto)
print(paste("Área bajo la curva ROC (AUC-ROC):", auc_roc))

