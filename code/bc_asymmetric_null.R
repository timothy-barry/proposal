#############
# Functions
#############

library(ggplot2)
library(magrittr)

# s plot
make_s_plot <- function(x, n_bins = 10) {
  B <- length(x)
  x_null_order <- sort(x)
  x1 <- -x_null_order[seq(B/2, 1)]
  x2 <- x_null_order[seq(B/2 + 1, B)]
  to_plot <- data.frame(x1 = x1, x2 = x2, bin = factor(dplyr::ntile(x = x2, n = n_bins)))
  s_plot <- ggplot(data = to_plot, mapping = aes(x = x1, y = x2)) + geom_point(size = 1.8, alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, col = "red") + theme_bw() + xlab("Lower half quantile") + ylab("Upper half quantile") + ggtitle("S-plot")
  return(s_plot)
}

# BC method
get_bc_thresh <- function(x, q = alpha) {
  unique_x <- sort(unique(abs(x)))
  fdp_hat <- sapply(X = unique_x, FUN = function(thresh) {
    (1 + sum(x <= -thresh))/max(1, sum(x >= thresh))
  })
  compare <- fdp_hat <= q
  if (!any(compare)) Inf else min(unique_x[which(compare)])
}

# robust bc
get_rbc_q <- function(x) {
  unique_x <- sort(unique(abs(x)))
  fdp_hat <- sapply(X = unique_x, FUN = function(thresh) {
    (1 + sum(x <= -thresh))/max(1, sum(x >= thresh))
  })
  selected_q <- min(fdp_hat)
  compare <- fdp_hat <= selected_q
  bc_thresh <- if (!any(compare)) Inf else min(unique_x[which(compare)])
  return(c(q = selected_q, bc_thresh = bc_thresh))
}

# trim vector
trim_vector <- function(x, trim_pnt) {
  x[x <= -trim_pnt] <- -trim_pnt
  x[x >= trim_pnt] <- trim_pnt
  return(x)
}

# trim interval
trim_interval <- function(x, a, b, set_pnt) {
  x[x <= b & x >= a] <- set_pnt
  return(x)
}

# get the false discovery proportion and nondiscovery proportion of each method
get_fdp_ndp <- function(x_null, x_alt, bc_thresh) {
  n_discoveries <- sum(x_null >= bc_thresh) + sum(x_alt >= bc_thresh)
  n_false_discoveries <- sum(x_null >= bc_thresh)
  n_alternatives_retained <- sum(x_alt < bc_thresh)
  n_alternatives <- length(x_alt)
  emp_fdp <- n_false_discoveries/max(n_discoveries, 1)
  emp_ndp <- n_alternatives_retained/n_alternatives
  return(c(emp_fdp = emp_fdp, emp_ndp = emp_ndp))
}

###############
# End functions
###############

# generate data
set.seed(11)
n_null <- 10000
n_alt <- 1000
trim_pnt <- 3.0

x_null <- c(rnorm(n = n_null, mean = 0, sd = 1),
           runif(n = n_null/250, min = 3, max = 8),
           runif(n = n_null/250, min = -4, max = -3))
x_alt <- rnorm(n = n_alt, mean = 3, sd = 1)
x_null_trim <- trim_vector(x = x_null, trim_pnt = trim_pnt)
x_alt_trim <- trim_vector(x = x_alt, trim_pnt = trim_pnt)

# x_null <- c(runif(n = n_null/4, min = -2, max = -1),
#            rbeta(n = n_null/4, shape1 = 1, shape2 = 2) - 1,
#            rbeta(n = n_null/4, shape1 = 1, shape2 = 2),
#            runif(n = n_null/4, min = 1, max = 2.25))
# x_alt <- rnorm(n = n_alt, mean = 2.5, sd = 0.5)
# x_null_trim <- trim_interval(x = x_null, a = -1, b = 1, set_pnt = 0)
# x_alt_trim <- trim_interval(x = x_alt, a = -1, b = 1, set_pnt = 0)

# run BC
bc_thresh_raw <- get_bc_thresh(c(x_null, x_alt), 0.2)
get_fdp_ndp(x_null, x_alt, bc_thresh_raw)

bc_thesh_trim <- get_bc_thresh(c(x_null_trim, x_alt_trim), 0.2)
get_fdp_ndp(x_null_trim, x_alt_trim, bc_thesh_trim)

# plot
orig_df <- data.frame(x = c(x_null, x_alt), Distribution = c(rep("Null", length(x_null)), rep("Alternative", length(x_alt))))
trimmed_df <- data.frame(x = c(x_null_trim, x_alt_trim), Distribution = c(rep("Null", length(x_null_trim)), rep("Alternative", length(x_alt_trim))))
my_col <- c("dodgerblue3", "firebrick3")
p_grouped <- ggplot(data = orig_df, mapping = aes(x = x)) + geom_histogram(bins = 30, alpha = 0.7, position = "identity", col = "black", fill = "grey80") + theme_bw() + ggtitle("Raw density (aggregated)") + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) + ylab("")
p_orig <- ggplot(data = orig_df, mapping = aes(x = x, fill = Distribution)) + geom_histogram(bins = 30, alpha = 0.7, position = "identity", col = "black") + scale_fill_manual(values = my_col) + theme_bw() + theme(legend.position = c(0.8, 0.8)) + xlab("") + geom_vline(xintercept = bc_thresh_raw, lwd = .8) + ggtitle("Raw density") + theme(legend.position = c(0.8, 0.7), axis.text.y = element_blank(), axis.ticks.y = element_blank()) + ylab("")
p_trim <- ggplot(data = trimmed_df, mapping = aes(x = x, fill = Distribution)) + geom_histogram(bins = 30, alpha = 0.7, position = "identity", col = "black") + scale_fill_manual(values = my_col) + theme_bw() + theme(legend.position = c(0.8, 0.8)) + xlab("") + geom_vline(xintercept = bc_thresh_raw, lwd = .8) + ggtitle("Trimmed density") + theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank()) + ylab("")

ggsave(filename = "/Users/timbarry/research_code/proposal/figs/bc_robustness_aggregated.png", plot = p_grouped, device = "png", scale = 1, width = 5, height = 3.5, dpi = 330)

p_orig
s_orig <- make_s_plot(x_null) + geom_vline(xintercept = trim_pnt, col = "darkblue", lwd = 0.8) + ggtitle("Raw s-plot")

p_trim
s_trimmed <- make_s_plot(x_null_trim) + geom_vline(xintercept = trim_pnt, col = "darkblue", lwd = 0.8) + ggtitle("Trimmed s-plot")

p_out <- cowplot::plot_grid(p_orig, s_orig, p_trim, s_trimmed, nrow = 2)
ggsave(filename = "/Users/timbarry/research_code/proposal/figs/bc_robustness.png", plot = p_out, device = "png", scale = 1, width = 7, height = 5, dpi = 330)

# Idea: we might want to chop off the tails of the null distribution to improve calibration.
# We then determine the smallest q such that empirical FDR is controlled at level q.
# We run the BC procedure with this choice of q. Is this valid?
# idea: the thresholding step results in ties; we can try to break the ties by jittering the points a little.