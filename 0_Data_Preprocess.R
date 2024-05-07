# DATA PREPROCESS FOR RICA1 DATABASE

##------------------------------------------------------
##------------------------------------------------------
##------------------------------------------------------
  
##Libraries
library(tidyverse)
library(naniar)
library(dplyr)
library(lubridate)
library(modeest)
library(foreign)
library(corrplot)
library(caret)

##------------------------------------------------------
##------------------------------------------------------
##------------------------------------------------------
  
##Functions
  
  Mode <- function(x) {
    unique_x <- unique(x)
    unique_x[which.max(tabulate(match(x, unique_x)))]
  }
  
  ## One hot encoding
  one_hot_encoding <- function(datos, columna) {
    # Aplicar one-hot encoding a la columna especificada excluyendo el intercepto
    encoded_columna <- model.matrix(~ factor(datos[[columna]]) - 1)
    
    # Verificar si hay una columna de intercepto y eliminarla si existe
    if ("(Intercept)" %in% colnames(encoded_columna)) {
      encoded_columna <- encoded_columna[, -1]
    }
    
    # Renombrar las columnas para mayor claridad
    colnames(encoded_columna) <- paste0(columna, "_", seq_len(ncol(encoded_columna)))
    
    # Eliminar una columna redundante si es generada
    if (ncol(encoded_columna) > 2) {
      encoded_columna <- encoded_columna[, -5, drop = FALSE]
    }
    
    # Crear un nuevo dataset con todas las variables excepto la columna especificada
    nuevo_datos <- cbind(datos[, !names(datos) %in% columna], encoded_columna)
    
    return(nuevo_datos)
  }
  
  ## Remove outliers
  remove_outliers <- function(data, variable) {
    Q1 <- quantile(data[[variable]], 0.25)
    Q3 <- quantile(data[[variable]], 0.75)
    IQR_value <- Q3 - Q1
    
    lower_limit <- Q1 - 3 * IQR_value
    upper_limit <- Q3 + 3 * IQR_value
    
    data <- data[data[[variable]] >= lower_limit & data[[variable]] <= upper_limit, ]
    
    return(data)
  }
  
##------------------------------------------------------
##------------------------------------------------------
##------------------------------------------------------
    
##STEPS
    
  # 1- Load the data
  RICA = as.data.frame(RICA1)
  
  # 2- Clean patients with "muerte" val = null
  RICA <- RICA[!is.na(RICA$muerte),]
  
  # 3- Remove non medical variables
  RICA <- RICA[, -which(names(RICA) %in% c("var1", "estado_v"))]
  
  # 4- Remove variables with mostly null variables (more than 10%)
  baseline01 <- 0.1  
  prop_missing_values <- colMeans(is.na(RICA))
  RICA <- RICA[, prop_missing_values <= baseline01]
  
  # 5- Generate variable age
  RICA$edad <- as.period(interval(RICA$var5, RICA$var3)) / years(1)
  RICA$edad <- floor(RICA$edad)  
  RICA <- RICA[, -which(names(RICA) %in% c("var5", "var3"))]
  
  # 6- Input
  RICA <- RICA %>% mutate_all(~ ifelse(is.na(.), Mode(.), .))
  RICA$superv_dias[is.na(RICA$superv_dias)] <- 382
  
  # 7- Remove outliers
  variables_with_outliers <- c("var9", "var10", "var11", "var12", "var13", "var54", "var57", "var58", "MDRD")
  
  for (variable in variables_with_outliers) {
    RICA <- remove_outliers(RICA, variable)
  }
  
  # 8- Group variables to reduce dimensionality --> IR, ARAII, Etiologia, EKG, Hipertension arterial
  ## IR
  RICA$var30 <- ifelse(RICA$var30 %in% c(1, 4), 0, 
                ifelse(RICA$var30 %in% c(5, 3), 2,
                ifelse(RICA$var30 == 2, 1, NA)))
  
  ## Hipertension arterial
  RICA$var16 <- ifelse(RICA$var16 %in% c(2, 3), 1, 
                ifelse(RICA$var16 == 1, 0, NA))
  
  ## ARAII
  RICA$ARAII <- ifelse(RICA$ARAII %in% c(1, 2, 3, 4), 1, 
                ifelse(RICA$ARAII == 0, 0, NA)) 
  
  ## Etiología
  RICA$var73 <- replace(RICA$var73, RICA$var73 == 1, 0)
  RICA$var73 <- replace(RICA$var73, RICA$var73 == 2, 1)
  RICA$var73 <- replace(RICA$var73, RICA$var73 == 3, 2)
  RICA$var73 <- replace(RICA$var73, RICA$var73 == 7, 3)
  RICA$var73 <- ifelse(RICA$var73 %in% c(4, 5, 6, 8, 9, 10), 4, RICA$var73)
  
  ## EKG
  RICA$var128 <- ifelse(RICA$var128 %in% c(1, 4), 0, 
                 ifelse(RICA$var128 %in% c(2, 3), 1,
                 ifelse(RICA$var128 == 5, 2,       
                 ifelse(RICA$var128 == 6, 3, NA))))
  
  # 9- Change variables of binary variables (1 & 2) to (0 & 1)
  ## var6 sexo
  RICA$var6 <- replace(RICA$var6, RICA$var6 == 1, 0)
  RICA$var6 <- replace(RICA$var6, RICA$var6 == 2, 1)
  
  ## var22 Displemia
  RICA$var22 <- replace(RICA$var22, RICA$var22 == 1, 0)
  RICA$var22 <- replace(RICA$var22, RICA$var22 == 2, 1)
  
  ## var17 Diabetes
  RICA$var17 <- replace(RICA$var17, RICA$var17 == 1, 0)
  RICA$var17 <- replace(RICA$var17, RICA$var17 == 2, 1)
  
  ## var39 EPOC
  RICA$var39 <- replace(RICA$var39, RICA$var39 == 1, 0)
  RICA$var39 <- replace(RICA$var39, RICA$var39 == 2, 1)
  
  ## var131 Rx Torax
  RICA$var131 <- replace(RICA$var131, RICA$var131 == 1, 0)
  RICA$var131 <- replace(RICA$var131, RICA$var131 == 2, 1)
  
  ## var132 Rx Cardiomegalia
  RICA$var132 <- replace(RICA$var132, RICA$var132 == 1, 0)
  RICA$var132 <- replace(RICA$var132, RICA$var132 == 2, 1)
  
  
  ## 10- Apply one hot encoding for: "var30", "var73", "var100", "var128","var133", "var134", "var166" y "FEVI_types"
  RICA <- one_hot_encoding(RICA, "var30")
  RICA <- one_hot_encoding(RICA, "var73")
  RICA <- one_hot_encoding(RICA, "var100")
  RICA <- one_hot_encoding(RICA, "var128")
  RICA <- one_hot_encoding(RICA, "var133")
  RICA <- one_hot_encoding(RICA, "var134")
  RICA <- one_hot_encoding(RICA, "var166")
  RICA <- one_hot_encoding(RICA, "FEVI_types")
  RICA_one_hot_encoding <- RICA
  RICA <- RICA[, -which(names(RICA) %in% c("var16_3", "var30_3", "var73_4", "var100_4", "var128_4","var133_4", "var134_3", "var166_3", "FEVI_types_3"))]
  
  ## 11- Normalized data
  RICA_normalized <- apply(RICA, 2, function(x) (x - min(x)) / (max(x) - min(x)))
  RICA_normalized <- as.data.frame(RICA_normalized)
  
  ## 12- Remove highly correlated variables
  matrix_correlation <- cor(RICA_normalized[, -which(names(RICA_normalized) == "muerte")])
  high_correlation_pairs <- which(abs(matrix_correlation) > 0.7 & upper.tri(matrix_correlation), arr.ind = TRUE)
  columns_to_remove <- as.integer(c(53, 54, 55, 61, 64))
  RICA_GROUP1 <- RICA_normalized[, -columns_to_remove]
  
  ## 13- Balance data
  mayoritary_class <- which(RICA_GROUP1$muerte == 0)
  minoritary_class <- which(RICA_GROUP1$muerte == 1)
  subsample <- sample(mayoritary_class, length(minoritary_class), replace = FALSE)
  RICA_GROUP2 <- rbind(RICA_GROUP1[RICA_GROUP1$muerte == 1, ], RICA_GROUP1[subsample, ])
  
  ## 14- Divide between Train and Test data and store it
  set.seed(124)
  train_group1 <- createDataPartition(RICA_GROUP1$muerte, p = 0.7, list = FALSE)
  train_subsample_group1 <- RICA_GROUP1[train_group1, ]
  test_subsample_group1 <- RICA_GROUP1[-train_group1, ]
  write.table(train_subsample_group1, file = "train_subsample_group1.txt")
  write.table(test_subsample_group1, file = "test_subsample_group1.txt")
  
  train_group2 <- createDataPartition(RICA_GROUP2$muerte, p = 0.7, list = FALSE)
  train_subsample_group2 <- RICA_GROUP2[train_group2, ]
  test_subsample_group2 <- RICA_GROUP2[-train_group2, ]
  write.table(train_subsample_group2, file = "train_subsample_group2.txt")
  write.table(test_subsample_group2, file = "test_subsample_group2.txt")
