identity = function(data){
  prefix = "1";
  return(list(data, prefix));
}

square = function(data){
  prefix = "2";
  new_data = data.frame(y = data$y);
  for (col in colnames(data)[colnames(data)!="y"]){
    new_data[, str_c(prefix, col)] = data[, col]^2;
    }
  return(list(new_data, prefix));
}

cub = function(data){
  prefix = "3";
  new_data = data.frame(y = data$y);
  for (col in colnames(data)[colnames(data)!="y"]){
    new_data[, str_c(prefix, col)] = data[, col]^3;
  }
  return(list(new_data, prefix));
}

fourth = function(data){
  prefix = "4";
  new_data = data.frame(y = data$y);
  for (col in colnames(data)[colnames(data)!="y"]){
    new_data[, str_c(prefix, col)] = data[, col]^4;
  }
  return(list(new_data, prefix));
}

fith = function(data){
  prefix = "5";
  new_data = data.frame(y = data$y);
  for (i in 1:50){
    new_data[, str_c(prefix, "X", i)] = data[, str_c("X", i)]^5;
  }
  return(list(new_data, prefix));
}

inverse = function(data){
  prefix = "1/";
  new_data = data.frame(y = data$y);
  for (i in 1:50){
    new_data[, str_c(prefix, "X", i)] = 1/data[, str_c("X", i)];
  }
  return(list(new_data, prefix));
}

ln = function(data){
  prefix = "ln";
  new_data = data.frame(y = data$y);
  for (i in 1:50){
    new_data[, str_c(prefix, "X", i)] = log(data[, str_c("X", i)]);
  }
  return(list(new_data, prefix));
}