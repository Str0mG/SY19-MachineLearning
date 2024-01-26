



# SVM
library(kernlab)
library(caret)
library(nnet)

donnees <- read.table("phonemes\\phonemes_train.txt", header = TRUE)
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

# Diviser le dataset en train et test
set.seed(123)
trainIndex <- createDataPartition(donnees_pca_reduites$y, p = 0.8, list = FALSE, times = 1)
train <- donnees_pca_reduites[trainIndex, ]
test <- donnees_pca_reduites[-trainIndex, ]

# Exclure la classe 4
train <- subset(train, train$y != 4)

# Ajuster le modèle SVM pour la classe 4
fit1 <- ksvm(
  x = as.matrix(train[, 1:10]),
  type = "one-svc",
  kernel = "rbfdot",
  kpar = list(sigma = 0.05),
  nu = 0.05,
)

# Ajuster le modèle multinomial pour les autres classes
modele <- multinom(y ~ ., data = train)

# Faire des prédictions avec le SVM
pred_svm <- predict(fit1, as.matrix(test[, 1:10]))
indices_false_svm <- which(!pred_svm)
df_svm <- data.frame(Index = indices_false_svm, Value = 4)

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
print(paste("Accuracy:", accuracy))