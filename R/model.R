linear_model <- function(equation, data){
  if(!is.formula(equation)){
    equation = as.formula(equation)
  }

  #checking if all the variable in the formula exist and that they are numeric
  variables = all.vars(equation)
  for(variable in variables){
    if(!variable %in% columns){
      stop(paste('The following variable names is not in the columns: ', variable))
      is.numeric(data[variable])
    }
  }
  y = data[variables[0]]
  x = data[variables[1]]

  sxx = compute_mean(x) * n
  syy = compute_mean(y) * n
  sxy = compute_covariance(x, y) * (n-1)
  slope = sxy / sxx
  intercept = syy / n - slope * (sxx / n)

  ssr = slope * sxy
  predictions = intercept + slope * x
  sse = compute_sum((predictions - y)**2)

  value <- list(B_0 = intercept, B_1 = slope, Sxx = sxx, Syy = syy, Sxy = sxy, SSR = ssr,
                SSE = sse)
  attr(value, 'class') <- 'linear_model'

  return(value)
}
