#' Get RMSE for two matrices
#'
#' @param ref.q [matrix] Numeric values of N x K
#' @param test.q [matrix] Numeric values of N x K
#'
#' @return Numeric value
#' @export
#'
#' @examples
#' test.1 <- matrix(1, nrow = 2, ncol = 2)
#' test.2 <- matrix(2, nrow = 2, ncol = 2)
#' get_rmse(test.1, test.2)
get_rmse <- function(ref.q, test.q){
  stopifnot("Hey! Matrix dimensions need to match" = dim(ref.q) == dim(test.q))
  q.mat2 <- (test.q - ref.q)^2
  nK <- sum(!is.na(test.q))
  rmse <- sqrt((1/nK)*sum(q.mat2, na.rm = TRUE))
  return(rmse)
}
