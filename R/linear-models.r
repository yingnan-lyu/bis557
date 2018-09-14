
#' Fit a linear model
#'
#' @description This function passes parameters to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
#' @importFrom stats lm
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' summary(fit)
#' @export
linear_model <- function(formula, data) {
    mm <- model.matrix(formula,data)
  X <- mm
  y_name <- strsplit(as.character(formula)," ~ ")[[2]]
  y <- as.matrix(data[y_name])
  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  

  coeff <- round(as.numeric(beta_hat),15)
  names(coeff) <- rownames(beta_hat)

  object <- list(coefficients=coeff)
  class(object) <- "lm"
  return(object)
}
