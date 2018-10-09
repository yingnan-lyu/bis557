
#' Fit a ridge regression model
#'
#' @description This function performs a ridge regression.
#' @param formula a formula
#' @param data a data.frame
#' @param lambda a numeric parameter
#' @return An ridge_reg object
#' @import stats MASS 
#' @examples
#' fit <- ridge_reg(Sepal.Length ~., 1.2, iris)
#' @export
ridge_reg <- function(formula, lambda, data) {
  rownames(data) <- NULL
  
  m <- model.matrix(formula, data)
  y <- matrix(data[,as.character(formula)[2]], ncol=1)
  y <- y[as.numeric(rownames(m)),, drop=FALSE]
  
  # Using svd
  svd_obj <- svd(m)
  U <- svd_obj$u
  V <- svd_obj$v
  svals <- svd_obj$d
  
  D <- diag(svals / (svals^2 + lambda))
  beta <-V %*% D %*% t(U) %*% y
  rownames(beta) <- colnames(m)
  
  # Format coef for export
  coef <- as.numeric(beta)
  names(coef) <- colnames(m)
  
  # The object to export
  ret <- list(coef = coef, lambda = lambda, formula = formula)
  class(ret) <- "ridge_reg"
  ret
}
