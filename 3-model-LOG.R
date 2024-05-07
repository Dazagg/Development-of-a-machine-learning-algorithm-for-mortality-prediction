library(pROC)
library(corrplot)
library(caret)
library(randomForest)
library(e1071)
library(xgboost)

# Logistic REG

# 1- Data load
train_data <- train_subsample_group2
test_data <- test_subsample_group2
real_data <- factor(test_subsample_group2$muerte)

# Model 3 Logistic REG
modelo_logistico <- glm(muerte ~ ., data = train_data, family = "binomial")
predicciones_logisticas <- predict(modelo_logistico, newdata = test_data, type = "response")
predictions_LOG_ROUND <- round(predicciones_logisticas, 0)
predictions_LOG <- factor(predictions_LOG_ROUND)

matriz_confusion <- confusionMatrix(predictions_LOG, real_data)
print(matriz_confusion)
roc_objeto <- roc(test_data$muerte, predictions_LOG_ROUND)
auc_roc <- auc(roc_objeto)
print(paste("Área bajo la curva ROC (AUC-ROC):", auc_roc))
