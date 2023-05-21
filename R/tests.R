# This file will contain hypothesis tests, diagnosis plots and other general tests

f_test <- function(object, alpha = 0.05){
  # Tests the null hypothesis which is that all parameters = 0 vs the alternative hypothesis
  # That at least one parameter does not equal 0
  result <- 'Null hypothesis is not rejected'

  f <- qf(1 - alpha, object$df_regression, object$df_residuals)
  test_statistic <- (object$SSR / object$df_regression) / (object$SSE / object$df_residuals)

  if(test_statistic > f){
    result <- 'Null hypothesis is rejected'
  }


  return(data.frame(f_critical = f, f_statisitc = test_statistic, result = result))

}

plot.linear_model <- function(object){
  plot(object$y_predicted, object$residuals, xlab='Predictions', ylab='Residuals')
  abline(h=0)
  predictors <- subset(object$predictors, select = -c(Intercept))
  par(ask=TRUE)

  for(column in colnames(predictors)){
    plot(predictors[,column], object$residuals, xlab=column, ylab='Residuals')
    abline(h=0)
  }
  par(ask=FALSE)

}

detect_outliers <- function(object){
  residuals <- object$residuals
  x <- as.matrix(object$predictors)
  hat_matrix <- x %*% solve(t(x)%*%x) %*% t(x)
  identity_matrix <- diag(nrow = nrow(hat_matrix))
  residuals_variance <- diag((object$SSE / object$df_residuals) * (identity_matrix - hat_matrix))

  #Scale the residuals
  studentized_residuals <- residuals / sqrt(residuals_variance)

  #Returns a Boolean vector where true means that this row is an outlier
  return(abs(studentized_residuals) >= 3)
}


