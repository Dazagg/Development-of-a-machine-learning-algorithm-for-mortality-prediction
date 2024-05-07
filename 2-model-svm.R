library(pROC)
library(corrplot)
library(e1071)

# Support Vector machines

# 1- Data load
train_data <- train_subsample_group1
test_data <- test_subsample_group1
real_data <- factor(test_subsample_group1$muerte)

# Model 2 Support Vector machine
modelo_svm <- svm(muerte ~ ., data = train_data, kernel = "radial")
predictions_svm_NUM <- predict(modelo_svm, newdata = test_data)
predictions_svm_ROUND <- round(predictions_svm_NUM, 0)
predictions_svm <- factor(predictions_svm_ROUND)

matriz_confusion <- confusionMatrix(predictions_svm, real_data)
print(matriz_confusion)
roc_objeto <- roc(test_data$muerte, predictions_svm_ROUND)
auc_roc <- auc(roc_objeto)
print(paste("(AUC-ROC):", auc_roc))
