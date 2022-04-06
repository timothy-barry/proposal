library(magrittr)
library(mlr3)

# basic operations
data("mtcars", package = "datasets")
car_task <- as_task_regr(x = mtcars, target = "mpg")
car_task$nrow
car_task$ncol
car_task$data() %>% head()

# binary classification
data("Sonar", package = "mlbench")
