
#' Create a new class "sparse.matrix"
#'
#' @description This function Create a new class "sparse.matrix".
#' @param i a numeric vector for rows
#' @param j a numeric vector for columns
#' @param x a numeric vector specifying values in the matrix
#' @param dims a vector with dimensions of the sparse matrix
#' @return A sparse matrix
#' @import stats MASS
#' @examples
#' sm0 <- sparse.matrix(i = c(1, 2), j = c(1, 1), x = c(1, 1))
#' @export
sparse.matrix <- function(i, j, x, dims=NULL) {
  
  sm <- data.frame(i=i, j=j, x=x)
  
  # If no dimensions are given, assume the largest coordinates are the dimensions.
  if (is.null(dims)) {
    dims <- c(max(i), max(j))
  } 
  
  r <- dims[1]; c <- dims[2]
  index <- data.frame(i=rep(1:r), j=rep(1:c, each=r))
  sm <- merge(index, sm, by = c("i", "j"), all.x = TRUE, sort=FALSE)
  sm <- sm[sm$x != 0 & !is.na(sm$x),]
  
  attr(sm$i, "label") <- r
  attr(sm$j, "label") <- c
  class(sm) <- c("sparse.matrix", class(sm))
  sm
}

#' Add two sparse matrices
#'
#' @description This function add two sparse matrices.
#' @param a a sparse matrix
#' @param b a sparse matrix
#' @return A sparse matrix
#' @import stats MASS
#' @examples
#' sm0 <- sparse.matrix(i = c(1, 2), j = c(1, 1), x = c(1, 1))
#' sm0 + sm0
#' @export
`+.sparse.matrix` <- function(a,b) {
  # Check whether the dimensions of a and b agree
  if (attr(a$i, "label") != attr(b$i, "label") ||
      attr(a$j, "label") != attr(b$j, "label")) {
    stop('The dimensions of the two input matrix do not match')
  }
  
  c <- merge(a, b, by = c("i", "j"), all = TRUE, suffixes = c("1", "2"))
  c$x1[is.na(c$x1)] <- 0
  c$x2[is.na(c$x2)] <- 0
  c$x <- c$x1 + c$x2
  c <- c[, c("i", "j", "x")]
  #sparse.matrix(i=c$i, j=c$j, x=c$x)
  sm <- data.frame(i=c$i, j=c$j, x=c$x)
  dims <- c(attr(a$i, "label"), attr(a$j, "label"))
  
  r <- dims[1]; c <- dims[2]
  index <- data.frame(i=rep(1:r), j=rep(1:c, each=r))
  sm <- merge(index, sm, by = c("i", "j"), all.x = TRUE, sort=FALSE)
  sm <- sm[sm$x != 0 & !is.na(sm$x),]
  
  attr(sm$i, "label") <- r
  attr(sm$j, "label") <- c
  class(sm) <- c("sparse.matrix", class(sm))
  sm
}



#' Transpose a sparse matrix
#'
#' @description This function transposes a sparse matrix.
#' @param x a sparse matrix
#' @return A sparse matrix
#' @import stats MASS
#' @examples
#' sm0 <- sparse.matrix(i = c(1, 2), j = c(1, 1), x = c(1, 1))
#' t(sm0)
#' @export
t.sparse.matrix <- function(x) {
  # swap i and j index
  ta <- x
  names(ta) <- c('j', 'i', 'x')
  ta <- ta[,c('i', 'j', 'x')]
  
  r <- attr(ta$i, "label"); c <- attr(ta$j, "label")
  index <- data.frame(i=rep(1:r), j=rep(1:c, each=r))
  ta <- merge(index, ta, by = c("i", "j"), all.x = TRUE, sort=FALSE)
  ta <- ta[ta$x != 0 & !is.na(ta$x),]
  
  #sparse.matrix(i=ta$i, j=ta$j, x=ta$x, dim=c(r,c))
  sm <- data.frame(i=ta$i, j=ta$j, x=ta$x)
  dims <- c(r, c)
  
  index <- data.frame(i=rep(1:r), j=rep(1:c, each=r))
  sm <- merge(index, sm, by = c("i", "j"), all.x = TRUE, sort=FALSE)
  sm <- sm[sm$x != 0 & !is.na(sm$x),]
  
  attr(sm$i, "label") <- r
  attr(sm$j, "label") <- c
  class(sm) <- c("sparse.matrix", class(sm))
  sm
}



`%*%.default` = .Primitive("%*%")  

`%*%` = function(x,...){ 
  UseMethod("%*%",x)
}

`%*%` <- function(x, y) {
  UseMethod("%*%", x)
}

#' Multiply two sparse matrices
#'
#' @description This function multiplies two sparse matrices.
#' @param a a sparse matrix
#' @param b a sparse matrix
#' @return A sparse matrix
#' @import stats MASS
#' @examples
#' sm1 <- sparse.matrix(i = c(1, 2), j = c(1, 1), x = c(3, 1), dims = c(3, 2))
#' sm3 <- sparse.matrix(i = rep(1, 3), j = 1:3, x = 1:3, dims = c(2, 3))
#' @export
`%*%.sparse.matrix` <- function(a,b) {
  #UseMethod("%*%", a)
  # Check the type of b
  if (!inherits(b, "sparse.matrix")) {
    stop("y argument is not a sparse matrix type.")
  }
  # Check whether the dimensions of a and b agree
  if (attr(a$j, "label") != attr(b$i, "label")) {
    stop('The dimensions of the two input matrix do not match')
  }
  
  if (attr(a$i, "label") == 0 || attr(a$j, "label") == 0 ||
      attr(b$i, "label") == 0 || attr(b$j, "label") == 0) {
    arow <- max(a$i); acol <- max(a$j)
    brow <- max(b$i); bcol <- max(b$j)
  } else {
    arow <- attr(a$i, "label"); acol <- attr(a$j, "label")
    brow <- attr(b$i, "label"); bcol <- attr(b$j, "label")
  }
  
  # Create an index of full-rank cols and rows
  index <- data.frame(i=rep(1:max(arow, brow)),
                      j=rep(1:max(acol, bcol), each=max(arow, brow)))
  
  c <- merge(index, a, by = c("i", "j"), all.x = TRUE)
  c <- merge(c, b, by = c("i", "j"), all = TRUE, suffixes = c("1", "2"))
  c$x1[is.na(c$x1)] <- 0
  c$x2[is.na(c$x2)] <- 0
  
  for (u in 1:arow) {
    for (v in 1:bcol) {
      c$x[c$i==u & c$j==v] <- sum(c$x1[c$i==u] * c$x2[c$j==v])
    }
  }
  
  c <- c[c$x != 0 & !is.na(c$x),]
  c <- c[, c("i", "j", "x")]
  
  #sparse.matrix(i=c$i, j=c$j, x=c$x, dim=c(attr(a$i, "label"), attr(b$j, "label")))
  sm <- data.frame(i=c$i, j=c$j, x=c$x)
  dims <- c(attr(a$i, "label"), attr(b$j, "label"))
  
  r <- dims[1]; c <- dims[2]
  index <- data.frame(i=rep(1:r), j=rep(1:c, each=r))
  sm <- merge(index, sm, by = c("i", "j"), all.x = TRUE, sort=FALSE)
  sm <- sm[sm$x != 0 & !is.na(sm$x),]
  
  attr(sm$i, "label") <- r
  attr(sm$j, "label") <- c
  class(sm) <- c("sparse.matrix", class(sm))
  sm
}
