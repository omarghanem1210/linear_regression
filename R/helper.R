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

predict.linear_model <- function(object, predictors){
  x <- as.matrix(cbind(rep(1, nrow(predictors)), predictors))
  predictions <- x %*% object$B
  return(data.frame(predictions = predictions))

}
