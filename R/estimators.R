# This file will only contain functions that estimate parameters

compute_mean <- function(x){
  is_vector_numeric(x)

  sum = 0
  for(element in x){
    sum <- sum + element
  }
  mean <-  sum / length(x)

  return(mean)
}

compute_variance <- function(x){
  is_vector_numeric(x)

  x_bar <-  compute_mean(x)
  sum <-  0
  for(element in x){
    sum <- sum + (element - x_bar)**2
  }
  variance <- sum / (length(x) - 1)

  return(variance)

}

compute_covariance <- function(x, y){
  is_vector_numeric(x)
  is_vector_numeric(y)
  if(!(length(x) == length(y))){
    stop('The two vectors should have the same length')
  }

  covariance = compute_mean(x * y) - compute_mean(x) * compute_mean(y)
  return(covariance)
}
