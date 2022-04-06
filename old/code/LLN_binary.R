library(magrittr)

n <- 100
B1 <- 1000
B2 <- 800
cut <- 0

x_mat <- matrix(data = rpois(n = n * B1, lambda = 8), nrow = n, ncol = B1)
y_mat <- matrix(data = rnorm(n = n * B2, mean = 3, sd = 3), nrow = n, ncol = B2)

test_stat <- function(x_col, y_col) {
  cor(x = x_col, y = y_col) > cut
}

# We seek to estimate E(test_stat(x[,j], y[,k])); we attempt to do this through LLN.

lln_diag <- function(x_mat, y_mat) {
  if (!all(dim(x_mat) == dim(y_mat))) stop("Dimensions of x_mat and y_mat do not match.")
  B <- ncol(x_mat)
  sapply(X = seq(1, B), FUN = function(i) {
    test_stat(x_mat[,i], y_mat[,i])
  }) %>% mean()
}

lln_full <- function(x_mat, y_mat) {
  B_x <- ncol(x_mat)
  B_y <- ncol(y_mat)
  out <- matrix(data = FALSE, nrow = B_x, ncol = B_y)
  for (j in seq(1, B_x)) {
    if (j %% 10 == 0) print(j)
    for (k in seq(1, B_y)) {
      out[j,k] <- test_stat(x_mat[,j], y_mat[,k])
    }
  }
  return(out)
}

m <- lln_full(x_mat, y_mat)
