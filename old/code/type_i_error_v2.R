# set parameters
n_tests_per_validation_set <- 10000
n_validation_sets <- 2000
n_total <- n_tests_per_validation_set * n_validation_sets

# set p threshold and ci confidence level
p_thresh <- 0.1
delta <- 0.01

# carry out a bunch CI tests, n_tests_per_validation_set x n_validation_sets matrix of p-values
ps <- matrix(data = runif(n = n_total), nrow = n_tests_per_validation_set, ncol = n_validation_sets)
rejection_indicators <- ps <= p_thresh

upper_cis <- apply(rejection_indicators, 2, function(col) {
  X <- sum(col)
  a <- X + 1/2
  b <- n_tests_per_validation_set - X + 1/2
  qbeta(p = 1 - delta, shape1 = a, shape2 = b)
})

# histogram of the upper CIs
mean(upper_cis >= p_thresh)
hist(upper_cis)
abline(v = p_thresh, col = "darkblue", lwd = 2.5)

# histogram of the shifted upper CIs
prob_bd <- upper_cis * (1 - delta) + delta
hist(prob_bd, xlim = c(0, max(upper_cis + ci_alpha)))
abline(v = p_thresh, col = "darkblue", lwd = 2.5)
