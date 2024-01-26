mean_acc_trans_var = c();
NOMBRE_CV = 5;


multivar_selection_inner_cv = function(data){
  all_data = data.frame(y = data$y);
  function_list = c(fourth, square, cub, identity);#, square, cub, fourth, fith, inverse, ln);
  all_kept_vars = c()
  for (func in function_list){
    transformation = func(data);
    trans_data = transformation[[1]];
    prefix = transformation[[2]];
    
    for (col in colnames(trans_data)){
      if (col != "y"){
        all_data[, col] = trans_data[, col];
      }
    }
    
    cv_selection = do_cv_selection(trans_data, prefix);
    print("kept vars");
    print(cv_selection);
    all_kept_vars = append(all_kept_vars, cv_selection);
  }
  print("All kept vars");
  print(all_kept_vars);
  selected_data = all_data[, c("y", all_kept_vars)];
  #print(selected_data);
  last_cv_selection = do_cv_selection(selected_data, "all");
  return(last_cv_selection);
}

do_cv_selection = function(data, prefix){
  n = nrow(data);
  k_cv = NOMBRE_CV;
  cv_index = get_cv_index(n, k_cv);
  cv_acc = c()
  cv_forms = list();
  for (k in 1:k_cv){
    #d?but de boucle
    print(k);
    val_i = cv_index[k];
    test_subset = data[val_i[[1]],];
    n.test = nrow(test_subset)
    train_subset = data[-val_i[[1]],];
    
    #fitting
    bic_lm = lm(y~.,data = train_subset)
    bic_selection = stepAIC(bic_lm, scope = y ~ ., direction = "both",
                            k=log(nrow(train_subset)));
    bic_form = formula(bic_selection);
    cv_forms = append(cv_forms, bic_form);
    bic_lm = lm(bic_form, data = train_subset);
    
    #testing
    bic_lm_pred = predict(bic_lm, newdata = test_subset);
    #dn_bic_lm_pred = bic_lm_pred*sd_data["y"];
    
    #evaluating
    perf = table(test_subset$y, bic_lm_pred)
    bic_acc = sum(diag(perf))/n.test
    cv_acc[k] = bic_acc
  }
  temp =mean_acc_trans_var;
  temp[prefix] = mean(cv_acc); # impossible lorsqu'on a ln ou 1/
  assign("mean_mse_trans_var", temp, .GlobalEnv);
  best_vars = get_kept_var(cv_forms, k_cv)
  return(best_vars)
}


get_cv_index = function(data_set_size, k_fold){
  n = data_set_size;
  k_cv = k_fold;
  subset_size = n/k_cv;
  random_i = sample(n, n);
  subset_index = list();
  for (k in 1:k_cv){
    subset_index = append(subset_index,
                          list(random_i[(1+subset_size*(k-1)):(subset_size*k)]));
  }
  return(subset_index)
}

get_kept_var = function(cv_forms, k_cv){
  count = c();
  for (form in cv_forms){
    for (var_name in all.vars(form)){
      if (var_name %in% names(count)){
        count[var_name] = count[var_name] + 1;
      }
      else if(var_name != "y"){
        count[var_name] = 1;
      }
    }
  }
  print("count");
  print(count);
  kept_vars = c();
  for (var in names(count)){
    if (count[var] == k_cv){
      kept_vars = c(kept_vars, var)
    }
  }
  print("kept_vars in get_kept_vars");
  print(kept_vars);
  return(kept_vars);
}


x_val = function(data_f){
  return(data_f[, colnames(data_f)!="y"])
}

x_val_mat = function(data_f){
  return(as.matrix(data_f[, colnames(data_f)!="y"]))
}


std_mse = function(CV_mse){
  k = length(CV_mse);
  mean_mse = mean(CV_mse);
  var_mse = (1/k) * sum((CV_mse-mean_mse)^2);
  std_mse = sqrt(var_mse/k);
  return(std_mse);
}

# Algo --------------------------------------------------------------------
library(MASS)
library(nnet)
library(naivebayes)
library(class)
library(mclust)
library(stringr)
source('non_linearities.R')
robo <- read.table("donnees/robotics_train.txt")
n <- nrow(robo)
p <- ncol(robo)

robo_var = multivar_selection_inner_cv(robo)

#[1] "count"
#4X2 2X3 3X7  X1  X2  X3  X4  X5  X6  X7  X8 2X2 3X8 4X3 3X1 
#3   4   5   5   5   5   5   5   5   5   5   2   2   1   1 
#[1] "kept_vars in get_kept_vars"
#[1] "3X7" "X1"  "X2"  "X3"  "X4"  "X5"  "X6"  "X7"  "X8" 


source('non_linearities.R')
cons <- read.table("donnees/construction_train.txt")
n = nrow(cons)
p = ncol(cons)

X<-cons
X<-scale(X)
pca<-princomp(X)
Z<-pca$scores
lambda<-pca$sdev**2

cons_pca = Z[,1:20]
cons_pca_df = data.frame(cons_pca, cons$y)
colnames(cons_pca_df)[colnames(cons_pca_df) == "cons.y"] <- "y"

cons_var = multivar_selection_inner_cv(cons)

#[1] "count"
#4Comp.2  2Comp.2  3Comp.2  3Comp.3  3Comp.7   Comp.1   Comp.3   Comp.4   Comp.5   Comp.8 
#4        3        4        4        4        5        5        4        5        5 
#Comp.9  Comp.10  Comp.14  Comp.17  Comp.18  Comp.12 4Comp.19  2Comp.1  2Comp.7  3Comp.1 
#5        5        2        5        5        3        2        3        1        1 
#3Comp.10   Comp.2  4Comp.1  2Comp.6 3Comp.11 2Comp.19 
#4        1        1        2        2        1 
#[1] "kept_vars in get_kept_vars"
#[1] "Comp.1"  "Comp.3"  "Comp.5"  "Comp.8"  "Comp.9"  "Comp.10" "Comp.17" "Comp.18"
 

#[1] "count"
#4X92 2X57 2X93 3X17 3X45 3X48 3X57  X50  X55  X57  X93  X92 4X57 
#5    5    3    2    5    5    5    4    5    5    5    2    1 
#[1] "kept_vars in get_kept_vars"
#[1] "4X92" "2X57" "3X45" "3X48" "3X57" "X55"  "X57"  "X93" 


