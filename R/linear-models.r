
#' Fit a linear model
#'
#' @description This function passes parameters to the lm function.
#' @param formula a formula
#' @param data a data.frame
#' @return An lm object
#' @importFrom stats lm rmarkdown
#' @examples
#' fit <- linear_model(Sepal.Length ~., iris)
#' summary(fit)
#' @export
linear_model <- function(formula, data) {
 
  # Define y
  y_name <- strsplit(as.character(formula)," ~ ")[[2]]
  y <- as.matrix(data[y_name])
  
  # Deal with colinearity in X
  #Xs <- data[,-which(colnames(data)==y_name)]
  #Xs_num <- Xs[,sapply(1:ncol(Xs), function(i) is.numeric(Xs[,i]))]
  #Xs_char <- Xs[,!sapply(1:ncol(Xs), function(i) is.numeric(Xs[,i]))]
  #tmp <- cor(Xs)
  #tmp[upper.tri(tmp)] <- 0
  #diag(tmp) <- 0
  
  #Xs <- Xs[apply(tmp, 2, function(x) any(abs(x) > 0.99))]
  #data_new <- cbind(y, Xs)
  
  # Define X and calculate svd of X
  #X <- model.matrix(formula,data_new)
  X <- stats::model.matrix(formula,data)
  #s <- svd(X)
  QR <- qr(X)
  beta_hat <- t(solve.qr(QR,y))
  beta_hat[beta_hat == 0] <- NA
  
  # Calculate beta hat
  #beta_hat <- s$v %*% diag(1/s$d) %*% t(s$u) %*% y
  #beta_hat_data <- as.data.frame(t(beta_hat), row.names = "1")
  #names(beta_hat_data) <- colnames(X)
  
  # Store beta hat in a data frame for coefficients
  #coeff <- as.data.frame(matrix(NA, 1, ncol(data)))
  #colnames(coeff) <- colnames(model.matrix(formula, data))
  
  #coeff <- merge(coeff, beta_hat_data, all.x=TRUE, all.y = TRUE)[1,]
  #coeff <- as.numeric(coeff)
  #names(coeff) <- colnames(model.matrix(formula, data))
  coeff <- as.numeric(beta_hat)
  names(coeff) <- colnames(beta_hat)
  
  # Create an lm object to output
  object <- list(call=paste("lm(formula = ", format(formula), 
                            ", data = ", deparse(substitute(data)), ")", sep=""), 
                 coefficients=coeff)
  class(object) <- "lm"
  return(object)

}
