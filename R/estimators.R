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

confint.linear_model <- function(object, alpha=0.05){
  t <- qt(1-alpha/2, object$SSE_df)
  mse <- object$SSE / (object$SSE_df)
  n <- object$SSE_df + 2

  b_1_lower_bound <- object$B_1 - t*sqrt(mse/object$Sxx)
  b_1_upper_bound <- object$B_1 + t*sqrt(mse/object$Sxx)

  b_0_lower_bound <- object$B_0 - t*sqrt((mse * (1/n + object$mean**2 / object$Sxx)))
  b_0_upper_bound <- object$B_0 + t*sqrt((mse * (1/n + object$mean**2 / object$Sxx)))

  lower_bounds <- c(b_0_lower_bound, b_1_lower_bound)
  upper_bounds <- c(b_0_upper_bound, b_1_upper_bound)
  names <- c('Intercept', 'Slope')

  intervals <- data.frame(names = names, lower_bound = lower_bounds, upper_bound = upper_bounds,
                          row.names = 'names')
  return(intervals)
}

response_confint <- function(object, x, alpha=0.05){
  t <- qt(1-alpha/2, object$SSE_df)
  mse <- object$SSE / (object$SSE_df)
  y <- object$B_0 + object$B_1 * x
  n <- object$SSE_df+2

  mean_response_lower_bounds <- y - t*sqrt(mse*(1/n + (x - object$mean)**2/object$Sxx))
  mean_response_upper_bounds <- y + t*sqrt(mse*(1/n + (x - object$mean)**2/object$Sxx))

  predicted_response_lower_bounds <- y - t*sqrt(mse*(1 + 1/n + (x - object$mean)**2/object$Sxx))
  predicted_response_upper_bounds <- y + t*sqrt(mse*(1 + 1/n + (x - object$mean)**2/object$Sxx))

  lower_bound <- c(mean_response_lower_bounds, predicted_response_lower_bounds)
  upper_bound <- c(mean_response_upper_bounds, predicted_response_upper_bounds)

  intervals <- data.frame(Predictions = y, mean_lower_bounds = mean_response_lower_bounds,
                          mean_upper_bounds = mean_response_upper_bounds
                          ,predicted_lower_bounds = predicted_response_lower_bounds,
                          predicted_upper_bounds = predicted_response_upper_bounds)
  return(intervals)
}
