library(ggplot2)
library(cowplot)
library(magrittr)

# s plot
make_s_plot <- function(x) {
  B <- length(x)
  x_null_order <- sort(x)
  x1 <- -x_null_order[seq(1, B/2)]
  x2 <- x_null_order[seq(B, B/2 + 1)]
  s_plot <- ggplot(data = data.frame(x1 = x1, x2 = x2) %>% dplyr::filter(x1 >= 0, x2 >= 0), mapping = aes(x = x1, y = x2)) + geom_point(size = 1.8, alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, col = "red") + theme_bw() + xlab("Lower half quantile") + ylab("Upper half quantile") + ggtitle("S-plot")
  return(s_plot)
}

#make_s_plot_2 <- function(x) {
#  B <- length(x)
#  x_sort <- sort(x)
#  x_left <- x_sort[seq(1, B/2)]
#  x_right <- x_sort[seq()]
#}

# BC method
get_bc_thresh <- function(x, q = alpha) {
  unique_x <- sort(unique(abs(x)))
  fdp_hat <- sapply(X = unique_x, FUN = function(thresh) {
    (1 + sum(x <= -thresh))/max(1, sum(x >= thresh))
  })
  compare <- fdp_hat <= q
  if (!any(compare)) Inf else min(unique_x[which(compare)])
}

set.seed(4)
# get data; trim
trim_vector <- function(x, trim_pnt) {
  x[x <= -trim_pnt] <- -trim_pnt
  x[x >= trim_pnt] <- trim_pnt
  return(x)
}
n_null <- 10000
n_alt <- 1000
x_null <- rnorm(n = n_null, mean = 0, sd = 1)
x_alt <- rnorm(n = n_alt, mean = 3.5, sd = 1)
trim_pnt <- -qnorm(0.01)
x_null_trimmed <- trim_vector(x_null, trim_pnt = trim_pnt)
x_alt_trimmed <- trim_vector(x_alt, trim_pnt = trim_pnt)

# run BC
alpha <- 0.15
bc_thresh_raw <- get_bc_thresh(c(x_null, x_alt), alpha)
bc_thresh_trim <- get_bc_thresh(c(x_null_trimmed, x_alt_trimmed), alpha)

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
get_fdp_ndp(x_null, x_alt, bc_thresh_raw)
get_fdp_ndp(x_null_trimmed, x_alt_trimmed, bc_thresh_trim)

# plot
orig_df <- data.frame(x = c(x_null, x_alt), Distribution = c(rep("Null", n_null), rep("Alternative", n_alt)))
trimmed_df <- data.frame(x = c(x_null_trimmed, x_alt_trimmed), Distribution = c(rep("Null", n_null), rep("Alternative", n_alt)))
my_col <- c("dodgerblue3", "firebrick3")
p_orig <- ggplot(data = orig_df, mapping = aes(x = x, fill = Distribution)) + geom_histogram(bins = 30, alpha = 0.7, position = "identity", col = "black") + scale_fill_manual(values = my_col) + theme(legend.position = c(0.8, 0.8)) + xlab("") + geom_vline(xintercept = bc_thresh_trim, lwd = .8) + ggtitle("Raw") + theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank()) + ylab("")
p_trim <- ggplot(data = trimmed_df, mapping = aes(x = x, fill = Distribution)) + geom_histogram(bins = 30, alpha = 0.7, position = "identity", col = "black") + scale_fill_manual(values = my_col) + xlab("") + geom_vline(xintercept = bc_thresh_trim, lwd = 0.8) + theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank()) + ggtitle("Trimmed") + ylab("")

p_out <- plot_grid(p_orig, p_trim, nrow = 1)
ggsave(filename = "/Users/timbarry/research_code/proposal/figs/binned_bc.png", width = 6, height = 3, scale = 1)

# important fact: the empirical FDP and NDP are the same for both the raw and trimmed data when the trimmed threshold is finite.

# Finally, run BH on the same data and check results
p_null <- pnorm(x_null, lower.tail = FALSE)
p_alt <- pnorm(x_alt, lower.tail = FALSE)
rejections <- p.adjust(p = c(p_null, p_alt), method = "BH") <= alpha
rejected_null <- rejections[1:length(p_null)]
rejected_alt <- rejections[(length(p_null) + 1):length(rejections)]
n_rejections <- sum(rejected_null) + sum(rejected_alt)
bh_emp_fdp <- sum(rejected_null)/n_rejections
bh_emp_ndp <- sum(!rejected_alt)/length(rejected_alt)

# The empirical FDP and NDP are similar to that of the BC method!
bh_emp_fdp
bh_emp_ndp


# Basically: the power of BC is similar to that of BH, despite the fact that is makes fewer assumptions.
# We can futher robustify BC without losing power by binning the tails; the requirement is that trim point such that we can obtain a finite BC threshold. In this case, our discovery set is 0.
# We call this procedure "binned BC." Binned BC is more robust than BC, but the probability of an infinite BC threshold (i.e., R = 0) is greater.
# BH, BC, and binned BC are all solid methods. Recommendation:
# - Use BH is neg controls are calibrated.
# - If negative controls are not calibrated but symmetric (as assessed by an s-plot), use BC.
# - If negative controls are not symmetric, try a symmetry-preserving transformation and then run BC (a special case is "binned BC").
# Questions
# - An intermediate version of the above? For example, shrink everything down by an exponential factor (or something)?
# - a two-tailed BC procedure? A simple idea: compute a p-value, and then center the p-value at 0.
# - confidence intervals or bands for s-plots?
