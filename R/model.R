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
  p = length(variables)

  x <-  as.matrix(cbind(Intercept=rep(1, n), subset(data, select = variables[2:length(variables)])))
  y <-  as.matrix(data[, variables[1]])
  B <- solve(t(x) %*% x) %*% t(x) %*% y

  sse <- drop(t(y - x%*%B) %*% (y - x%*%B))
  sst <- drop(t(y) %*% y - nrow(data) * compute_mean(y[, 1]))
  ssr <- sst - sse
  mse <- sse / (n - p)
  variance_B <- solve(t(x) %*% x) * mse

  value <- list(B=B, SSE=sse, SST=sst, SSR=ssr,df_regression = p-1, df_residuals=n-p,
                variance_covariance_matrix = variance_B)
  attr(value, 'class') <- 'linear_model'

  return(value)

}
