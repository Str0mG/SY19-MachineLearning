
# Apprentissage -----------------------------------------------------------
library(kernlab)
library(MASS)
library(caret)
library(nnet)
library(randomForest)


# Phonemes
donnees <- read.table("donnees\\phonemes_train.txt", header = TRUE)
donnees <- as.data.frame(donnees)
# Transformation de la variable y en numerique
donnees$y <- as.factor(donnees$y)
rf.fit <- randomForest(y ~ ., data = donnees, ntree = 1000, keep.forest = FALSE, importance = TRUE, type = "classification")
ImpData <- as.data.frame(importance(rf.fit))
ImpData$Var.Names <- row.names(ImpData)
ImpData_sorted <- ImpData[order(-ImpData$MeanDecreaseGini), ]
top_20_variables_phon <- ImpData_sorted[1:20, ]
donnees_pca_reduites <- donnees[, top_20_variables_phon$Var.Names]
fit1 <- ksvm(
      x = as.matrix(donnees_pca_reduites[, 1:20]),
      type = "one-svc",
      kernel = "rbfdot",
      kpar = list(sigma = 0.5),
      nu = 0.1,
)
donnees_pca_reduites$y <- as.factor(donnees$y)

model.phoneme <- randomForest(y ~ ., data = donnees_pca_reduites, ntree = 200, mtry = 8)

#Robotics
robo <- read.table("donnees/robotics_train.txt")
model.robotics <- ksvm(y~.,data=robo ,scaled=TRUE ,type="eps-svr",
                       kernel="rbfdot" ,C=3 ,epsilon=1e-09, sigma=1e-08)

#Construction
donnees <- read.table("donnees\\construction_train.txt", header = TRUE)
rf.fit <- randomForest(y ~ ., data = donnees, ntree = 1000, keep.forest = FALSE, importance = TRUE, type = "regression")
ImpData <- as.data.frame(importance(rf.fit))
ImpData$Var.Names <- row.names(ImpData)
ImpData_sorted <- ImpData[order(-ImpData$`%IncMSE`), ]
top_20_variables <- ImpData_sorted[1:20, ]
features <- donnees[, top_20_variables$Var.Names]
features$y <- donnees$y
set.seed(123)
grid <- expand.grid(mtry = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20))
ctrl <- trainControl(method = "cv", number = 20)
rf_grid <- train(y ~ ., data = features, method = "rf", trControl = ctrl, tuneGrid = grid)
best_mtry <- rf_grid$bestTune$mtry
best_mtry
model.construction <- randomForest(y ~ ., data = features, ntree = 200, mtry = best_mtry)

# Prediction --------------------------------------------------------------

# Phonemes
prediction_phoneme <- function(dataset) {
  library(kernlab)
  library(caret)
  library(nnet)
  library(randomForest)
  dataset <- dataset[, top_20_variables_phon$Var.Names]
  pred_svm <- predict(fit1, as.matrix(dataset))
  
  indices_false_svm <- which(!pred_svm)
  df_false_svm <- data.frame(Index = indices_false_svm, Value = rep("?", length(indices_false_svm)), stringsAsFactors = FALSE)

  indices_true_svm <- which(pred_svm)
  df_filtrado <- dataset[row.names(dataset) %in% indices_true_svm, ]

  pred1 <- predict(model.phoneme, newdata = df_filtrado)

  df_combinado <- data.frame(Index = indices_true_svm, Value = pred1)

  df_combinado2 <- rbind(df_false_svm, df_combinado)

  df_combinado2 <- df_combinado2[order(df_combinado2$Index), ]

  predictions <- df_combinado2$Value
  return(predictions)
}

#Robotics
prediction_robotics <- function(dataset) {
  library(kernlab)
  library(MASS)
  
  pred <- predict(model.robotics, newdata=dataset)
  return(pred)
}

#Construction
prediction_construction <- function(dataset) {
  library(randomForest)
  library(caret)
  dataset <- dataset[, top_20_variables$Var.Names]
  pred <- predict(model.construction, newdata = dataset)
  return(pred)
}

# Save
save(
  prediction_phoneme,
  prediction_robotics,
  prediction_construction,
  fit1,
  model.phoneme,
  model.robotics,
  model.construction,
  top_20_variables,
  top_20_variables_phon,
  rf.fit,
  file = "env.RData"
)