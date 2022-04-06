# in this script we generate B synthetic negative control datasets;
# we estimate type-I error (with confidence) using these datasets;
# finally, we investigate type-I error control for a remaining dataset

n <- 5000
d <- 5
Z <- matrix(data = rnorm(n * d), nrow = n, ncol = d)
n_validation_sets <- 500
B_validation <- 200
B_total <- B_validation * n_validation_sets
thresh <- 0.2
ci_conf <- 0.85

# model for x
beta_confound <- c(1, -2, 1/4,  1/2, -1/2)
fam <- binomial()
mus_x <- fam$linkinv(as.numeric(Z %*% beta_confound))

# model for y
mus_y <- as.numeric(Z %*% beta_confound)

# generate X and Y matrices
xs <- sapply(X = mus_x, FUN = function(i) {
  rbinom(n = B_total, size = 1, prob = i)
}) %>% t()
ys <- sapply(X = mus_y, function(i) {
  rnorm(n = B_total, mean = i, sd = 1)
}) %>% t()

# fit linear model to check that data generation correct
p_vals <- sapply(seq(1, B_total), function(i) {
  fit <- lm(ys[,i] ~ xs[,i] + Z)
  s <- summary(fit)$coefficients
  s[,"Pr(>|t|)"][[2]]
})

# compute an upper confidence interval for the unobserved type-I error on the validation set
rejections <- p_vals < thresh
gamma_hat <- mean(rejections)
upper_ci_gamma_hat <- gamma_hat + 1.96 * sqrt(gamma_hat * (1 - gamma_hat)/B_total)
lower_ci_gamma_hat <- gamma_hat - 1.96 * sqrt(gamma_hat * (1 - gamma_hat)/B_total)

# compute an upper confidence interval on each of the validation sets
upper_cis <- sapply(X = seq(1, n_validation_sets), FUN = function(i) {
  from <- (i - 1) * B_validation + 1
  to <- i * B_validation
  curr_rejections <- rejections[seq(from, to)]
  curr_p_hat <- mean(curr_rejections)
  upper_ci <- curr_p_hat + qnorm(ci_conf) * sqrt(curr_p_hat * (1 - curr_p_hat)/B_validation)
  return(upper_ci)
})

# create plot of CIs
hist(upper_cis)
abline(v = gamma_hat, col = "darkblue", lwd = 2.0)
mean(upper_cis >= gamma_hat) # approximately 95% of the upper CIs are greater than the true type-I error, as expected

# create plot of shifted CIs
hist(upper_cis + 1 - ci_conf, xlim = c(0, max(upper_cis) + 0.2))
abline(v = gamma_hat, col = "darkblue", lwd = 2.0) # approximately 90% of the upper CIs are greater than 0.05
mean(upper_cis + 1 - ci_conf >= gamma_hat) # all of the SHIFTED CIs are greater than the true type-I error
