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

  y <-  data[, variables[1]]
  x <-  data[, variables[2]]
  n <-  length(x)


  sxx <-  compute_variance(x) * (n-1)
  syy <-  compute_variance(y) * (n-1)
  sxy <-  compute_covariance(x, y) * (n-1)
  slope <-  sxy / sxx
  intercept <-  compute_mean(y) - slope * (compute_mean(x))

  ssr <-  slope * sxy
  predictions <-  intercept + slope * x
  sse <-  compute_sum((predictions - y)**2)

  value <- list(B_0 = intercept, B_1 = slope, Sxx = sxx, Syy = syy, Sxy = sxy, SSR = ssr,
                SSE = sse, SSE_df = n-2, mean = compute_mean(x))
  attr(value, 'class') <- 'linear_model'

  return(value)
}
