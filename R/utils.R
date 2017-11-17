#' Mean value of a vector
#'
#' @param x a numeric vector
#' @return the mean of a \code{(n x 1)} - vector as a scalar value
#'
#' @examples
#' x = c(2,2,2)
#' mean(x)
#' # [1] 2
#'
#' @author Darya Busen <\email{dasha-89.89@@mail.ru}>
#'
#'
#' Return the rounded mean
mean = function(x) {
  n = length(x)
  sum = sum(x)
  rnd = round(x = (1/n) * sum , digits = 3)
  return(rnd)
}
