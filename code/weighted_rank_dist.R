library(magrittr)

B <- 1000
n <- 10000
x_grid <- seq(-2, 2, length.out = B - 1)
a <- dnorm(x = x_grid, mean = 0, sd = 1)
# a <- c(rep(0, (B - 1)/3), rep(1, (B - 1)/3), rep(0, (B - 1)/3))

# compute the theoretical null distribution (in both unweighted and weighted cases)
theoretical_unranked_dist <- seq(1, B)
theoretical_ranked_dist <- 1 + cumsum(c(0, a))

# compute the empirical null distribution 
res <- replicate(n = n, expr = {
  samp <- rnorm(n = B)
  t_star <- samp[1]
  t_order <- sort(samp[2:B])
  unweight_rank <- sum(t_star >= t_order) + 1
  weight_rank <- sum(a * (t_star >= t_order)) + 1
  return(c(unweight_ranks = unweight_rank, weight_ranks = weight_rank))
}) %>% t()


hist(res[,"weight_ranks"], breaks = 16)
hist(res[,"unweight_ranks"], breaks = 16)

mean(res[,"weight_ranks"] >= 250.3222)
mean(res[,"unweight_ranks"] >= 900)
