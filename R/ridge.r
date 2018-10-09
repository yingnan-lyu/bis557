
#' Fit a ridge regression model
#'
#' @description This function passes parameters to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @param lambda a numeric parameter
#' @return An ridge_reg object
#' @importFrom stats
#' @examples
#' fit <- ridge_reg(Sepal.Length ~., iris, 1.2)
#' summary(fit)
#' @export
ridge_reg <- function(formula, data, lambda = 1.2121212) {
  rownames(data) <- NULL
  
  m <- model.matrix(formula, data)
  y <- matrix(data[,as.character(formula)[2]], ncol=1)
  y <- y[as.numeric(rownames(m)),, drop=FALSE]
  k <- ncol(m)
  
  # Using svd
  svd_obj <- svd(m)
  U <- svd_obj$u
  V <- svd_obj$v
  svals <- svd_obj$d
  
  D <- diag(svals / (svals^2 + lambda))
  beta <-V %*% D %*% t(U) %*% y
  rownames(beta) <- colnames(m)
  
  # Using Baysien
  #beta <- solve(crossprod(m) + diag(lambda, ncol(m)), crossprod(m, y))

  # Principle Component
  #svd_obj <- svd(m)
  #U <- svd_obj$u
  #V <- svd_obj$v
  #dvals <- rep(0, ncol(m))
  #dvals[seq_len(k)] <- 1 / svd_obj[["d"]][seq_len(k)]
  #D <- diag(dvals)
  #beta <- V %*% D %*% t(U) %*% y
  
  # Format coef for export
  coef <- beta[2:k]
  names(coef) <- colnames(m[,-1])
  
  ret <- list(coef = coef, lambda = lambda, form = form)
  class(ret) <- "ridge_reg"
  ret
}
