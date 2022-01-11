require(magrittr)
B <- 500
exp <- replicate(n = 20000, expr = {
  x <- runif(B)
  y <- runif(1)
  y < sort(x)
}) %>% t()

i <- 200
i/B
mean(exp[,i])

n <- 400
1/(n + 1)
r <- 399
term <- function(n, r) {
  k <- seq(0, n - r)
  s <- choose(n - r, k) * (-1)^k * 1/(k + r + 1)
  choose(n, r) * sum(s)
}
term(n, r)
1/(n+1)
