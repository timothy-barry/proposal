x_grid <- seq(-2.5,2.5,0.25)
y <- rnorm(1)
is <- as.integer(x_grid > y)
r <- 6
B <- length(is)
idx <- seq(1, B)

p <- sum(is)/B
cs <- rnorm(r + 1, mean = 1, sd = 2)
j <- 3
js <- seq(0, r)
x0 <- 0.5

M <- function(B, k, i) (1 - i/B + 1/B)^k - (1 - i/B)^k

# function to compute the power coefficients
g <- function(x0, j, B, i) {
  ks <- seq(1, j)
  sum(x0^(j - ks) * choose(j, ks) * (-1)^(j - ks) * M(B, ks, i))
}

# function to compute the polynomial e-coefficients
f <- function(cs, x0, r, B, i) {
  out <- 0
  cs_1_idx <- cs[-1]
  for (j in seq(1, r)) {
    for (k in seq(1, j)) {
      out <- out + cs_1_idx[j] * (x0^(j - k)) * choose(j, k) * (-1)^(j - k) * M(B, k, i)
    }
  }
  return(out)
}

# compute power coefficients
power_coefs <- sapply(X = seq(1, B), FUN = function(i) g(x0, j, B, i))
power_intercept <- x0^j * (-1)^j

# compute the polynomial e-coefficients and intercept
e_coefs <- sapply(seq(1, B), function(i) f(cs, x0, r, B, i))
e_intercept <- sum(cs * x0^js * (-1)^js)

# prop 2, part 1
(sum(is))^r
sum(((B - idx + 1)^r - (B - idx)^r) * is)

# prop 2, part 2
p^r
sum(((1 - idx/B + 1/B)^r - (1 - idx/B)^r) * is)

# prop 2, part 3 (power)
(p - x0)^j
power_intercept + sum(power_coefs * is)

# prop 2, part 4 (polynomial)
sum(cs * (p - x0)^js)
e_intercept + sum(e_coefs * is)
