



# SVM
library(kernlab)
library(caret)
library(nnet)

donnees <- read.table("donnees\\phonemes_train.txt", header = TRUE)
donnees <- as.data.frame(donnees)

# Transformation de la variable y en numerique
donnees$y <- as.factor(donnees$y)
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

    # Ajuster le modèle SVM pour la classe i
    fit1 <- ksvm(
        x = as.matrix(donnees_pca_reduites[, 1:10]),
        type = "one-svc",
        kernel = "rbfdot",
        kpar = list(sigma = 0.1),
        nu = 0.05,
    )

    train$y <- as.factor(train$y)
    
    # Random Forest
    library(randomForest)
    modele <- randomForest(y ~ ., data = train, ntree = 200, mtry = 8)


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

# Restaure o tamanho padrão se necessário
par(mar = c(5, 4, 4, 2) + 0.1, mgp = c(3, 1, 0))

for(i in 1:5){
  print(mean(acc[i,]))
}