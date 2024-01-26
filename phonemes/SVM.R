



# SVM
library(kernlab)
library(caret)
library(nnet)

donnees <- read.table("donnees\\phonemes_train.txt", header = TRUE)
donnees <- as.data.frame(donnees)

# Transformation de la variable y en numerique
donnees$y <- as.numeric(as.factor(donnees$y))
# Sélection des variables pour fda
donnees_pca <- donnees[, 1:256]



# Normalisation des données
donnees_pca <- scale(donnees_pca)

# Application de l'ACP
resultat_acp <- princomp(donnees_pca)
k <- 10
donnees_pca_reduites <- predict(resultat_acp)[, 1:k]

# Transformation en dataframe
donnees_pca_reduites <- as.data.frame(donnees_pca_reduites)
donnees_pca_reduites$y <- donnees$y
K <- 5
fold <- sample(K, nrow(donnees_pca_reduites), replace = TRUE)
table(fold)

acc <- matrix(0, nrow = 5, ncol = K)


for (i in 1:5) {
  for (k in 1:K) {
    train <- donnees_pca_reduites[fold != k, ]
    test <- donnees_pca_reduites[fold == k, ]

    cat(sprintf("Processing fold %i\n", k))
    # Exclure la classe 4
    train <- subset(train, train$y != i)

    parametres <- expand.grid(
      sigma = c(0.01, 0.1, 1, 10),
      C = c(0.01, 0.1, 1, 10)
    )

    # Criar uma matriz com numeros aleatorios
    acc <- matrix(0, nrow = 5, ncol = 5)
    # data.frame com os indices dos valores falsos
    df_svm <- data.frame(Index = 0, Value = 0)
    
    # criar uma coluna y no data.frame df_svm com numerows aleatorios
    df_svm$y <- sample(1:5, nrow(df_svm), replace = TRUE)

    # SVM One class
    model <- train(
      y ~ .,
      data = df_svm,
      method = "oneClassSVM",
      trControl = trainControl(method = "cv", number = 5),
      tuneGrid = parametres
    )

    # Ajuster le modèle SVM pour la classe 4
    fit1 <- ksvm(
      x = as.matrix(train[, 1:10]),
      type = "one-svc",
      kernel = "rbfdot",
      kpar = list(sigma = 0.01),
      nu = 0.05,
      C = 1,
    )

    train$y <- as.factor(train$y)
    library(e1071)
    # SVM
    modele <- svm(y ~ ., data = train, kernel = "radial", cost = 1, gamma = 0.001)


    # Faire des prédictions avec le SVM
    pred_svm <- predict(fit1, as.matrix(test[, 1:10]))
    indices_false_svm <- which(!pred_svm)
    df_svm <- data.frame(Index = indices_false_svm, Value = i)

    # Ordenar o DF test, pois estava desordenado
    df_new <- data.frame(test, stringsAsFactors = FALSE, row.names = 1:nrow(test))
    indices_true_svm <- which(pred_svm)
    df_true_test <- df_new[row.names(df_new) %in% indices_true_svm, ]

    # Prédiction des classes avec le modèle multinomial
    pred_multinom <- predict(modele, newdata = df_true_test[, 1:10])
    df_multinom <- data.frame(Index = indices_true_svm, Value = pred_multinom)

    # Fusionner les data frames
    df_combined <- rbind(df_svm, df_multinom)
    df_combined <- df_combined[order(df_combined$Index), ]

    # Afficher le résultat final
    predictions <- df_combined$Value
    accuracy <- mean(predictions == test$y)
    acc[i, k] <- accuracy
    
  }
}

par(mfrow = c(1, 5))

# Diminuir altura e largura do gráfico
par(plt = c(0.1, 0.6, 0.1, 0.6))

for (i in 1:5) {
  boxplot(acc[i, ], main = paste("Classe", i), ylim = c(0.6, 1))
}

# Acuracy média para cada classe

for(i in 1:5){
  print(mean(acc[i,]))
}

