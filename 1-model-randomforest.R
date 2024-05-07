library(pROC)
library(corrplot)
library(randomForest)

# RANDOM FOREST

# 1- Data load
train_data <- train_subsample_group1
test_data <- test_subsample_group1
real_data <- factor(test_subsample_group1$muerte)

# 2- Random forest model
modelo_RF <- randomForest(muerte ~ ., data = train_data)
predictions_RF_NUM <- predict(modelo_RF, newdata = test_data)
predictions_RF_ROUND <- round(predictions_RF_NUM, 0)
predictions_RF <- factor(predictions_RF_ROUND)

matriz_confusion <- confusionMatrix(predictions_RF, real_data)
print(matriz_confusion)
roc_objeto <- roc(test_data$muerte, predictions_RF_ROUND)
auc_roc <- auc(roc_objeto)
print(paste("(AUC-ROC):", auc_roc))
