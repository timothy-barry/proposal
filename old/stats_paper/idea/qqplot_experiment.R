library(ggplot2)
library(dplyr)

m1 <- 0
m2 <- 0

sd1 <- 1.0
sd2 <- 1.1
n <- 10000

ys <- rnorm(n = n, mean = m1, sd = sd1)
xs <- rnorm(n = n, mean = m2, sd = sd2)

df <- data.frame(value = c(ys, xs), type = c(rep("x", n), rep("y", n)))
plot1 <- df %>%
  ggplot(aes(x = value, fill = type)) +
  geom_histogram(color = "#e9ecef", alpha = 0.6, position = 'identity', bins = 30) +
  scale_fill_manual(values = c("#69b3a2", "#404080")) +
  labs(fill = "") + theme_bw()

# a qq-plot function
my_qq <- function(xs, ys) {
  x_sorted <- sort(xs)
  y_sorted <- sort(ys)
  p <- ggplot(data = data.frame(x = x_sorted, y = y_sorted), mapping = aes(x = x, y = y)) +
    geom_abline(slope = 1, intercept = 0, col = "red") + geom_point(size = 0.8) + theme_bw()
  return(p)
}

plot1
my_qq(xs, ys)
