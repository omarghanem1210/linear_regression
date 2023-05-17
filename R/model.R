linear_model <- function(equation, data_set=NULL, path=NULL){
  if(!is.null(path)){
    data <- read.csv(path)
    data <- na.omit(data)
  }
  else{
    data <- na.omit(data_set)
  }

  if(!is.formula(equation)){
    equation = as.formula(equation)
  }

  #checking if all the variable in the formula exist and that they are numeric
  columns = names(data)
  variables = all.vars(equation)

  for(variable in variables){
    if(!variable %in% columns){
      stop(paste('The following variable names is not in the columns: ', variable))
    }
    is.numeric(data[variable])
  }
  n <-  nrow(data)

  x <-  as.matrix(cbind(rep(1, n), subset(data, select = variables[2:length(variables)])))
  y <-  as.matrix(data[, variables[1]])

  B <- solve(t(x) %*% x) %*% t(x) %*% y
  sse <- t(y - x%*%B) %*% (y - x%*%B)

  value <- list(B = B, SSE = sse)
  attr(value, 'class') <- 'linear_model'

  return(value)

}
