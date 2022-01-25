library(magrittr)
library(ggplot2)

make_s_plot <- function(x) {
  B <- length(x)
  x_null_order <- sort(x)
  x1 <- -x_null_order[seq(1, B/2)]
  x2 <- x_null_order[seq(B, B/2 + 1)]
  s_plot <- ggplot(data = data.frame(x1 = x1, x2 = x2) %>% dplyr::filter(x1 >= 0, x2 >= 0), mapping = aes(x = x1, y = x2)) + geom_point(size = 1.8, alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, col = "red") + theme_bw() + xlab("Lower half quantile") + ylab("Upper half quantile") + ggtitle("S-plot")
  return(s_plot)
}

x <- c(runif(n = 1000, min = -2, max = -1),
       rbeta(n = 1000, shape1 = 1, shape2 = 2) - 1,
       rbeta(n = 1000, shape1 = 1, shape2 = 2),
       runif(n = 1000, min =1, max = 2))
make_s_plot(x)
