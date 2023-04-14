# This file will contain miscellaneous functions

is_vector_numeric <- function(x){
  if(!is.vector(x, mode = 'numeric')){
    stop('TypeError: The argument should be a numeric vector')

  }
}

compute_sum <- function(x){
  is_vector_numeric(x)
  sum <- 0

  for(element in x){
    sum <- sum + element
  }
  return(sum)
}

is.formula <- function(x){
  inherits(x,'formula')
}

print.linear_model <- function(object){
  names <- c('Intercept', 'Slope', 'SXX', 'SYY', 'SXY', 'SSR', 'SSE')
  values <- c(object$B_0, object$B_1, object$Sxx, object$Syy, object$Sxy, object$SSR, object$SSE)
  table <- data.frame(names = names, values = values)

  print(as.table(as.matrix(table)))
}

predict.linear_model <- function(object, x){
  is.numeric(x)
  y <- object$B_0 + object$B_1 * x

  return(data.frame(Inputs = x, predictions = y))

}

plot.linear_model <- function(object, y, x){
  plot(x, y)
  abline(a = object$B_0, b = object$B_1)
}
