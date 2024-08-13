get_rmse <- function(ref.q, test.q){
  stopifnot("Hey! Matrix dimensions need to match" = dim(ref.q) == dim(test.q))
  q.mat2 <- (test.q - ref.q)^2
  nK <- sum(!is.na(test.q))
  rmse <- sqrt((1/nK)*sum(q.mat2, na.rm = TRUE))
  return(rmse)
}
