x_grid <- seq(-2.5,2.5,0.1)
y <- rnorm(1)
is <- as.integer(x_grid > y)
B <- length(is)
r <- 5
p <- sum(is)/B
x0 <- 0.5

# jth derivatives of analytic p-to-e calibrators
# 1. f_{kappa} family
f_kappa_prime <- function(j, x0 = 1/2, kappa = 1/2) {
  ls <- seq(0L, j)
  prod(kappa - ls) * x0^(kappa - j - 1)
}

M <- function(B, k, i) (1 - i/B + 1/B)^k - (1 - i/B)^k
w <- function(f, x0, B, r, i) {
  out <- 0
  for (j in seq(1, r)) {
    for (k in seq(1, j)) {
      out <- out + (f(j, x0) * (x0^(j - k)) * (-1)^(j - k) * M(B, k, i))/(factorial(k) * factorial(j - k))
    }
  }
  return(out)
}
get_int <- function(f, x0, r) {
  js <- seq(0L, r)
  top <- sapply(X = js, FUN = function(j) f_kappa_prime(j, x0)) * x0^js
  bottom <- sapply(X = js, FUN = function(j) factorial(j))
  sum(top/bottom)
}

coefs <- sapply(X = seq(1, B), FUN = function(i) w(f_kappa_prime, 0.5, B, r, i))
intercept <- get_int(f, x0, r)
intercept + sum(coefs * is)
f_kappa_prime(0, p, 1/2)


# first, examine Taylor expansion
#f_prime <- function(j, x0 = 1/2, kappa = 1/2) {
#  if (j == 0) {
#    (1/2) * x0^(-1/2)
#  } else {
#    (-1)^j * factorial(2 * j)/(2^(2 * j + 1) * factorial(j)) * x0^(-1/2 - j)
#  }
# }

f_prime_over_j_fact <- function(j, x0 = 1/2, kappa = 1/2) {
  if (j == 0) {
    exp(log(1/2) - (1/2) * log(x0))
  } else {
    (-1)^j* exp(sum(log(seq(1, 2 * j))) - (1/2 + j) * log(x0) - (2 * j + 1) * log(2) - 2 * sum(log(seq(1, j))))
  }
}

get_poly <- function(r, x0 = 1/2) {
  js <- seq(0L, r)
  coefs <- sapply(X = js, FUN = function(j) f_prime_over_j_fact(j = j, x0 = x0))
  p <- Vectorize(function(x) {
    sum(coefs * (x - x0)^js)
  })
  return(p)
}

p <- get_poly(500, 0.5)
curve(expr = (1/2) * x^(-1/2), from = 0, to = 1)
curve(expr = p, from = 0, to = 1, col = "blue", add = TRUE)
