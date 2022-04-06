library(tidyverse)
library(pins)
library(torch)

x <- array(runif(8), dim = c(2, 2, 2))
y <- torch_tensor(x, dtype = torch_float64())
