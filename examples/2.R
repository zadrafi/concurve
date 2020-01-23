library(boot)
library(glmnet)
library(concurve)
library(bcaboot)
load("~/Desktop/GitHub/Statistical-Consulting/Grant_BodyFatMeasures/Paper1/clean_imputed.RData")

y <-  model.matrix( ~ CE_LST_TOTAL, data = SEGData_sub_TOTAL)[, -1]
X <- model.matrix(CE_LST_TOTAL ~ ., data = SEGData_sub_TOTAL)[, -1]


i <- nrow(Xy)

penfunction<- function(Xy, i) {
  y <- Xy[i, 11]
  X <- Xy[i, 1:10]
  return(sqrt(min(cv.glmnet(x = X,
                     y = y,
                     family = "gaussian",
                     nfolds = 10,
                     alpha = 1,
                     type.measure = "mse",
                     trace.it = TRUE,
                     parallel = TRUE)$cvm)))
  }

glmnet_model$lambda.min

w <- curve_penalty (X = X, y = y, alpha = 1,
                nfolds = 10, family= "gaussian", type.measure = "mse", reps = 100)
