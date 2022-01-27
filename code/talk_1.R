library(ggplot2)
library(magrittr)
library(cowplot)

# BC method and initial plots
get_bc_thresh <- function(x, q = alpha, ret_emp_fdp = FALSE) {
  unique_x <- sort(unique(abs(x)))
  fdp_hat <- sapply(X = unique_x, FUN = function(thresh) {
    (1 + sum(x <= -thresh))/max(1, sum(x >= thresh))
  })
  compare <- fdp_hat <= q
  ret <- if (!any(compare)) Inf else min(unique_x[which(compare)])
  if (ret_emp_fdp) ret <- list(bc_thresh = ret, fdp_hat = fdp_hat, unique_x = unique_x)
  return(ret)
}

set.seed(10)
n_null <- 10000 
n_alt <- 500
x_null <- rnorm(n = n_null, mean = 0, sd = 1)
x_alt <- rnorm(n = n_alt, mean = 3.5, sd = 1)

thresh <- get_bc_thresh(x = c(x_null, x_alt), q = 0.1, ret_emp_fdp = TRUE)

data_df <- data.frame(x = c(x_null, x_alt), Distribution = c(rep("Null", n_null), rep("Alternative", n_alt)))
p_disag <- ggplot(data = data_df, mapping = aes(x = x, fill = Distribution)) + geom_histogram(bins = 30, alpha = 0.7, position = "identity", col = "black") +
  scale_fill_manual(values = my_col) + theme(legend.position = c(0.8, 0.8)) + xlab("") + ggtitle("Disaggregated test statistics") +
  theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank()) + ylab("") + theme_bw()
p_grouped <- ggplot(data = data_df, mapping = aes(x = x)) + geom_histogram(bins = 30, alpha = 0.7, position = "identity", col = "black") +
  xlab("") + ggtitle("Aggregated histogram") + theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank()) + ylab("") +
  geom_vline(xintercept = -4.5, col = "blue") + geom_vline(xintercept = 4.5, col = "blue") + 
  geom_vline(xintercept = -3.5, col = "darkred") + geom_vline(xintercept = 3.5, col = "darkred") + 
  geom_vline(xintercept = -thresh$bc_thresh, col = "darkgreen") + geom_vline(xintercept = thresh$bc_thresh, col = "darkgreen") + theme_bw()

fdp_df <- data.frame(t = thresh$unique_x, emp_fdp = thresh$fdp_hat)
p_emp_fdp <- ggplot(data = fdp_df, mapping = aes(x = t, y = emp_fdp)) + geom_point(alpha = 0.75, size = 0.8) + theme_bw() + ylab("Empirical FDP") +
  geom_hline(yintercept = 0.1) + geom_vline(xintercept = 4.5, col = "blue") + geom_vline(xintercept = 3.5, col = "darkred") + geom_vline(xintercept = thresh$bc_thresh, col = "darkgreen") + 
  ggtitle("Empirical FDP vs. t")
p_tot <- plot_grid(p_grouped, p_emp_fdp)

# small numerical experiment of power of BC vs BH
get_fdp_ndp <- function(x_null, x_alt, bc_thresh) {
  n_discoveries <- sum(x_null >= bc_thresh) + sum(x_alt >= bc_thresh)
  n_false_discoveries <- sum(x_null >= bc_thresh)
  n_alternatives_retained <- sum(x_alt < bc_thresh)
  n_alternatives <- length(x_alt)
  emp_fdp <- n_false_discoveries/max(n_discoveries, 1)
  emp_ndp <- n_alternatives_retained/n_alternatives
  return(c(emp_fdp = emp_fdp, emp_ndp = emp_ndp))
}

set.seed(10)
n_null <- 10000 
n_alt <- 500
alpha <- 0.1
sim_res <- replicate(n = 50, expr = {
  x_null <- rnorm(n = n_null, mean = 0, sd = 1)
  x_alt <- rnorm(n = n_alt, mean = 3.5, sd = 1)
  
  p_null <- pnorm(x_null, lower.tail = FALSE)
  p_alt <- pnorm(x_alt, lower.tail = FALSE)
  
  if (TRUE) {
    x_null <- p_null * -1 + 1/2
    x_alt <- p_alt * -1 + 1/2
  }
  
  rejections <- p.adjust(p = c(p_null, p_alt), method = "BH") <= alpha
  rejected_null <- rejections[1:length(p_null)]
  rejected_alt <- rejections[(length(p_null) + 1):length(rejections)]
  n_rejections <- sum(rejected_null) + sum(rejected_alt)
  bh_emp_fdp <- sum(rejected_null)/n_rejections
  bh_emp_ndp <- sum(!rejected_alt)/length(rejected_alt)
  
  bc_thresh <- get_bc_thresh(x = c(x_null, x_alt), q = 0.1, ret_emp_fdp = FALSE)
  bc_metrics <-get_fdp_ndp(x_null, x_alt, bc_thresh)
  
  return(c(bh_fdp = bh_emp_fdp, bh_ndp = bh_emp_ndp, bc_fdp = bc_metrics[["emp_fdp"]], bc_ndp = bc_metrics[["emp_ndp"]]))
}) %>% t()
colMeans(sim_res) * 100

# save plots
ggsave(filename = "/Users/timbarry/research_code/proposal/talk/disag_test_stats.pdf", plot = p_disag, device = "pdf", scale = 1, width = 6, height = 4)
ggsave(filename = "/Users/timbarry/research_code/proposal/talk/agg_test_stats.pdf", plot = p_tot, device = "pdf", scale = 1, width = 8, height = 3)

