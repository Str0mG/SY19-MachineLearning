library(MASS)
library(naivebayes)
library(class)
library(mclust)
library(caret)
library(corrplot)

cons <- read.table("donnees/construction_train.txt")
n = nrow(cons)
p = ncol(cons)

# Corrélation -------------------------------------------------------------
cor_matrix <- cor(cons[,1:p-1])
corrplot(cor_matrix, method = "color")

# ACP ---------------------------------------------------------------------
X<-cons
X<-scale(X)
pca<-princomp(X)
Z<-pca$scores
lambda<-pca$sdev**2
plot(cumsum(lambda)/sum(lambda),type="l",xlab="q", ylab="proportion of explained variance")

pairs(Z[,1:5],col=cons$y)

cons_pca = Z[,1:20]
cons_pca_df = data.frame(cons_pca, cons$y)

# plot useless ------------------------------------------------------------


p1 = ggplot(cons_pca_df,aes(x = Comp.1)) + geom_histogram(bins = 30) +theme(legend.position = "none")
p2 = ggplot(cons_pca_df,aes(x = Comp.2)) + geom_histogram(bins = 30) +theme(legend.position = "none")
p3 = ggplot(cons_pca_df,aes(x = Comp.3)) + geom_histogram(bins = 30) +theme(legend.position = "none")
p4 = ggplot(cons_pca_df,aes(x = Comp.4)) + geom_histogram(bins = 30) +theme(legend.position = "none")
p5 = ggplot(cons_pca_df,aes(x = Comp.5)) + geom_histogram(bins = 30) +theme(legend.position = "none")
p6 = ggplot(cons_pca_df,aes(x = Comp.6)) + geom_histogram(bins = 30) +theme(legend.position = "none")
p7 = ggplot(cons_pca_df,aes(x = Comp.7)) + geom_histogram(bins = 30) +theme(legend.position = "none")
p8 = ggplot(cons_pca_df,aes(x = Comp.8)) + geom_histogram(bins = 30) +theme(legend.position = "none")
p9 = ggplot(cons_pca_df,aes(x = Comp.9)) + geom_histogram(bins = 30) +theme(legend.position = "none")
p10 = ggplot(cons_pca_df,aes(x = Comp.10)) + geom_histogram(bins = 30) +theme(legend.position = "none")
p11 = ggplot(cons_pca_df,aes(x = Comp.11)) + geom_histogram(bins = 30) +theme(legend.position = "none")
p12 = ggplot(cons_pca_df,aes(x = Comp.12)) + geom_histogram(bins = 30) +theme(legend.position = "none")
p13 = ggplot(cons_pca_df,aes(x = Comp.13)) + geom_histogram(bins = 30) +theme(legend.position = "none")
p14 = ggplot(cons_pca_df,aes(x = Comp.14)) + geom_histogram(bins = 30) +theme(legend.position = "none")
p15 = ggplot(cons_pca_df,aes(x = Comp.15)) + geom_histogram(bins = 30) +theme(legend.position = "none")
p16 = ggplot(cons_pca_df,aes(x = Comp.16)) + geom_histogram(bins = 30) +theme(legend.position = "none")
p17 = ggplot(cons_pca_df,aes(x = Comp.17)) + geom_histogram(bins = 30) +theme(legend.position = "none")
p18 = ggplot(cons_pca_df,aes(x = Comp.18)) + geom_histogram(bins = 30) +theme(legend.position = "none")
p19 = ggplot(cons_pca_df,aes(x = Comp.19)) + geom_histogram(bins = 30) +theme(legend.position = "none")
p20 = ggplot(cons_pca_df,aes(x = Comp.20)) + geom_histogram(bins = 30) +theme(legend.position = "none")

plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9, p10, p11, p12, p13, p14, p15, p16, p17, p18, p19, p20)

# res ---------------------------------------------------------------------

cross_validation = function(model, data, K){
  mse <- rep(0, K)
  n <- nrow(data)
  fold = sample(K, n, replace = TRUE)
  for (k in 1:K) {
    data.train = data[fold != k,]
    y.train = data[fold != k,]$y
    data.test = data[fold == k,]
    y.test = data[fold == k,]$y
    
    fit <- model(y~.,data=data.train)
    
    pred <- predict(fit,newdata=data.test)
    mse[k] = mean((y.test - pred)^2)
  }
  return(mse)
}
var = c("Comp.1", "Comp.3", "Comp.5", "Comp.8", "Comp.9", "Comp.10",
        "Comp.17", "Comp.18", "y")
colnames(cons_pca_df)[colnames(cons_pca_df) == "cons.y"] <- "y"
res_data = cons_pca_df[,var]

res_data = data.frame(cons[, 'y'])
#"4X92" "2X57" "3X45" "3X48" "3X57" "X55"  "X57"  "X93"
res_data[, '4X92'] = cons[, 'X92']^4
res_data[, '2X57'] = cons[, 'X57']^2
res_data[, '3X45'] = cons[, 'X45']^3
res_data[, '3X48'] = cons[, 'X48']^3
res_data[, '3X57'] = cons[, 'X57']^3
res_data[, 'X55'] = cons[, 'X55']
res_data[, 'X57'] = cons[, 'X57']
res_data[, 'X93'] = cons[, 'X93']
colnames(res_data)[colnames(res_data) == "cons....y.."] <- "y"

res_mse = cross_validation(lm, res_data, 10)
mean(res_mse)




