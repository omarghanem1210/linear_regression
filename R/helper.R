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
