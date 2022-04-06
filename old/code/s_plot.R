library(magrittr)
library(nimble)
library(ggplot2)
library(cowplot)
library(scales)

set.seed(12)

B <- 10000
x <- rdexp(n = B, location = 0, scale = 1)
theoretical_quantiles <- qnorm(p = seq(1, B)/B)
x_null_order <- sort(x)

# histogram
mu_hat <- mean(x)
sd_hat <- sd(x)
x_grid <- seq(from = min(x), to = max(x), length.out = 500)
y <- dnorm(x = x_grid, mean = mu_hat, sd = sd_hat)
hist_plot <- ggplot(data = data.frame(x = x), mapping = aes(x = x)) +
  geom_histogram(aes(y=..density..), fill = "white", color = "black", bins = 25) + 
  geom_line(data = data.frame(x = x_grid, y = y), mapping = aes(x = x, y = y), col = "blue") + xlim(c(-7, 7)) +
  theme_bw() + ylab("Density") + ggtitle("Histogram")

# s-plot
x1 <- -x_null_order[seq(1, B/2)]
x2 <- x_null_order[seq(B, B/2 + 1)]
s_plot <- ggplot(data = data.frame(x1 = x1, x2 = x2), mapping = aes(x = x1, y = x2)) + geom_point(size = 1.8, alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, col = "red") + theme_bw() + xlab("Lower half quantile") + ylab("Upper half quantile") + ggtitle("S-plot")

# qq-plot
qq_plot <- ggplot(data = data.frame(x1 = x_null_order, x2 = theoretical_quantiles) %>%
                    dplyr::filter(x1 >= 0, x2 >= 0), mapping = aes(x = x2, y = x1)) +
  geom_point(size = 1.8, alpha = 0.5) + geom_abline(slope = 1, intercept = 0, col = "red") + theme_bw() +
  xlab("Theoretical quantile") + ylab("Empirical quantile") + ggtitle("QQ-plot")

# combine
p_out <- plot_grid(hist_plot, s_plot, qq_plot, nrow = 1)
ggsave(filename = "/Users/timbarry/research_code/proposal/figs/s_plot.png", plot = p_out, device = "png", scale = 1, width = 7, height = 2.5, dpi = 330)
