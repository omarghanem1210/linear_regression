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
  parameters <- object$B
  standard_deviation <- diag(object$variance_covariance_matrix)^(1/2)

  t <- qt(1 - alpha/2, object$df_residuals)
  lower_bounds <- parameters - t * standard_deviation
  upper_bounds <- parameters + t * standard_deviation

  confidence_intervals <- data.frame(lower_bounds = lower_bounds,
                                     upper_bounds = upper_bounds)
  row.names(confidence_intervals) <- row.names(parameters)
  return(confidence_intervals)
}

mean_response_confint <- function(object, predictors, alpha=0.05){
  x <- as.matrix(cbind(rep(1, nrow(predictors)), predictors))
  predictions <- predict(object, predictors)

  t <- qt(1 - alpha/2, object$df_residuals)
  mse <- object$SSE / object$df_residuals
  lower_bounds <- predictions - t * ((mse * x %*% solve(t(x) %*% x) %*% t(x))^(1/2))
  upper_bounds <- predictions + t * ((mse * x %*% solve(t(x) %*% x) %*% t(x))^(1/2))

  colnames(lower_bounds) <- c('mean_response_lower_bounds')
  colnames(upper_bounds) <- c('mean_response_upper_bounds')

  return(data.frame(predictions = predictions, mean_response_lower_bounds = lower_bounds,
                    mean_response_upper_bounds = upper_bounds))
}

new_observation_confint <- function(object, predictors, alpha=0.05){
  x <- as.matrix(cbind(rep(1, nrow(predictors)), predictors))
  predictions <- predict(object, predictors)

  t <- qt(1 - alpha/2, object$df_residuals)
  mse <- object$SSE / object$df_residuals
  lower_bounds <- predictions - t * ((mse * x %*% solve(t(x) %*% x) %*% t(x) + mse)^(1/2))
  upper_bounds <- predictions + t * ((mse * x %*% solve(t(x) %*% x) %*% t(x) + mse)^(1/2))

  colnames(lower_bounds) <- c('new_response_lower_bounds')
  colnames(upper_bounds) <- c('new_response_upper_bounds')


  return(data.frame(predictions = predictions, new_response_lower_bounds = lower_bounds,
                    new_response_upper_bounds = upper_bounds))
}


