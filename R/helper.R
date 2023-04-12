# This file will contain miscellaneous functions

is_vector_numeric <- function(x){
  if(!is.vector(x, mode = 'numeric')){
    stop('TypeError: The argument should be a numeric vector')

  }
}
