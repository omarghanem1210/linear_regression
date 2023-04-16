# This file will contain hypothesis tests and other general tests

anova_table <- function(object){
  names <- c('Regression', 'Residual', 'Total')
  ss <- c(object$SSR, object$SSE, object$SSR + object$SSE)
  df <- c(1, object$SSE_df, object$SSE_df+1)
  ms <- c(object$SSR, object$SSE / object$SSE_df, NA)
  f <- c(object$SSR / (object$SSE / object$SSE_df), NA, NA)

  table <- data.frame(Source = names, SS = ss, d.f = df, MS = ms, F_Ratio = f)
  return(as.table(as.matrix(table)))
}
