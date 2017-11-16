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
mean = function(height.input = height) {
# Do checks
checkmate::assertVector(height.input)
checkmate::assert(checkmate::checkInteger(height.input, lower=0, min.len=1, any.missing = FALSE, null.ok=FALSE),
                  checkmate::checkNumeric(height.input, lower=0, min.len=1, any.missing = FALSE, null.ok=FALSE))
# Return rounded mean
return(round(base::mean(height.input),digits = 3))
}
