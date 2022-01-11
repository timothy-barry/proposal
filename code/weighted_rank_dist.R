# The ranks are uniformly distributed (regardless of underlying distribution);
# what about weighted ranks? Same deal.

B <- 1000
n <- 10000
x_grid <- seq(-2, 2, length.out = B - 1)
# a <- dnorm(x = x_grid, mean = 0, sd = 2)
a <- rep(1, B - 1)

ranks <- replicate(n = n, expr = {
  samp <- rnorm(n = B)
  t_star <- samp[1]
  t_order <- sort(samp[2:B])
  sum(a * (t_star >= t_order)) + 1
})

hist(ranks)
