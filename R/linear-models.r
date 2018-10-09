
#' Fit a linear model
#'
#' @description This function passes parameters to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
#' @import stats MASS
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' @export
linear_model <- function(formula, data) {
 
  # Define y
  y_name <- strsplit(as.character(formula)," ~ ")[[2]]
  y <- as.matrix(data[y_name])
  
  # Define X and calculate svd of X
  X <- stats::model.matrix(formula,data)
  QR <- qr(X)

  # Calculate beta hat
  beta_hat <- t(solve.qr(QR,y))
  beta_hat[beta_hat == 0] <- NA
  
  # Store beta hat in a data frame for coefficients
  coeff <- as.numeric(beta_hat)
  names(coeff) <- colnames(beta_hat)
  
  # Create an lm object to output
  object <- list(call=paste("lm(formula = ", format(formula), 
                            ", data = ", deparse(substitute(data)), ")", sep=""), 
                 coefficients=coeff)
  class(object) <- "lm"
  return(object)

}
