# This file will only contain functions that compute point and interval estimators

compute_mean <- function(x){
  is_vector_numeric(x)

  sum = compute_sum(x)
  mean <-  sum / length(x)

  return(mean)
}

compute_variance <- function(x){
  is_vector_numeric(x)

  x_bar <-  compute_mean(x)
  sum <-  compute_sum((x - x_bar)**2)
  variance <- sum / (length(x) - 1)

  return(variance)

}

compute_covariance <- function(x, y){
  is_vector_numeric(x)
  is_vector_numeric(y)
  if(!(length(x) == length(y))){
    stop('The two vectors should have the same length')
  }

  x_bar = compute_mean(x)
  y_bar = compute_mean(y)
  sum <- compute_sum((x - x_bar)*(y - y_bar))

  covariance = sum / (length(x) - 1)
  return(covariance)
}
