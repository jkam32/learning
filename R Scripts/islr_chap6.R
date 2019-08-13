library(ISLR)
library(leaps)

# check for missing values
apply(Hitters, MARGIN = 2, function(x) sum(is.na(x)))

hitters_df <-  na.omit(Hitters)

# fit up to 8 variables
bss_fit <- regsubsets(Salary ~ ., hitters_df)
summary(bss_fit)

# fit all
bss_full_fit <- regsubsets(Salary ~ ., hitters_df, nvmax = 19)
bss_summary <- summary(bss_full_fit)

# choice of number of variables against goodness of fit for all
# models chosen via best subsets
par(mar = rep(4, 4))
par(mfrow = c(2, 2))
plot(bss_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(bss_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq",
     type = "l")
which.max(bss_summary$adjr2)
points(11, bss_summary$adjr2[which.max(bss_summary$adjr2)], 
       col = "red", cex = 2, pch = 20)
plot(bss_summary$cp, xlab = "Number of Variables", ylab = "Cp",
     type = "l")
which.min(bss_summary$cp)
points(10, bss_summary$cp[10], col = "red", cex = 2, pch = 20)
plot(bss_summary$bic, xlab = "Number of Variables", ylab = "BIC",
     type = "l")
which.min(bss_summary$bic)
points(6, bss_summary$bic[6], col = "red", cex = 2, pch = 20)
par(mfrow = c(1, 1))

# 
plot(bss_full_fit, scale = "r2")
plot(bss_full_fit, scale = "adjr2")
plot(bss_full_fit, scale = "bic") # most optimal model located at the top row

coef(bss_full_fit, 6) # coefficients of the best model associated with bic

# forward selection
reg_fwd <- regsubsets(Salary ~ ., data = hitters_df, nvmax = 19, 
                      method = "forward")
summary(reg_fwd)

reg_bwd <- regsubsets(Salary ~ ., data = hitters_df, nvmax = 19,
                      method = "backward")
summary(reg_bwd)

# these approaches select different models
coef(reg_fwd, 7)
coef(reg_bwd, 7)
coef(bss_full_fit, 7)

#### Validation Set Approach ####

# random seed
set.seed(1)

# split into training and test set
train <- sample(c(TRUE, FALSE), nrow(hitters_df), replace = TRUE)
test <- !train

# best subset selection regression on training sets
regfit_best <- regsubsets(Salary ~ ., data = hitters_df[train, ], 
                          nvmax = 19)

# creates a design matrix out of test set
test_mat <- model.matrix(Salary ~ ., data = hitters_df[test, ])

# initializing for loops
val_error <- rep(NA, 19)

# stores the test errors
for (i in 1:19){
  # coefficients from best models, given i
  coefi <- coef(regfit_best, id = i)
  
  # predict Salary, via subsetting dataframe
  pred <- test_mat[, names(coefi)] %*% coefi
  
  # MSE
  val_error[i] <- mean((hitters_df$Salary[test] - pred)^2)
  
}
# plot(val_error, type = 'l')
which.min(val_error) #10

print("Trained on 50% data:")
coef(regfit_best, which.min(val_error))

# train the best subset selection on the full data
# however, the reason of doing so isn't clearly communicated in the text
reg_full <- regsubsets(Salary ~., data = hitters_df, nvmax = ncol(hitters_df) - 1)
print("Trained on full data:")
coef(reg_full, 10)

## Comment: Regression using best subset selection using the validation approach 
## (i.e. split into 50% train/test) yields that the best model is a 10-variable model.
## I do not get why you have to retrain the regression using best subset selection
## on the full data, and select the 10-variable model again. What's the point?


#### Choosing Models of Diff Sizes using Cross Validation Approach (10-fold) ####

## idea
## for each number of predictor, performs a 10-fold 
k <- 10
set.seed(1)
folds = sample(1:k, nrow(hitters_df), replace = TRUE)

# each fold is not that even though
cv_errors <-  matrix(NA, k, 19, dimnames = list(NULL, paste(1:19)))

# prediction function for regsubset class
# note that there is no predict.regsubsets 
predict.regsubsets <- function(object, newdata, id, ...){
  
  # in terms of formula Salary ~ .
  form <-  as.formula(object$call[[2]]) 

  # in terms of design matrix
  mat <-  model.matrix(form, newdata)
  
  #
  coefi <- coef(object, id = id)
  xvars <- names(coefi)
  mat[, xvars] %*% coefi
  }

# cross-validations
# for each fold, fit the best subset regressions, up to max variables
for (j in 1:k){
  # train on j-th fold (up to k = 10-fold in this case)
  best_fit <- regsubsets(Salary ~ ., data = hitters_df[folds != j, ], nvmax = 19)
  for (i in 1:19){
    # predict on test set
    pred <- predict(best_fit, hitters_df[folds == j, ], id = i)
    cv_errors[j, i] <- mean((hitters_df$Salary[folds ==j] - pred) ** 2)
  }
}

(mean_cv_errors <- apply(cv_errors, 2, mean))
which.min(mean_cv_errors)

best_full_fit <- regsubsets(Salary ~., data = hitters_df, nvmax = 19)
coef(best_full_fit, which.min(mean_cv_errors))

library(glmnet)
# syntax for fitting glmnet models depend on splitting the dataset into X and Y
# model.matrix automatically excludes the target variable (as specified in the formula)
x <- model.matrix(Salary ~ ., data= hitters_df)[, -1] # excludes the `1` column
y <- hitters_df$Salary
grid <- 10 ** seq(10, -2, length = 100)

# alpha = 0 -> ridge; alpha = 1 -> lasso
# by default glmnet standardizes the variable
ridge_mod <- glmnet(x, y, alpha = 0, lambda = grid)
dim(coef(ridge_mod)) # 20 x 100

## bigger lambda results in smaller l2-norm
## smaller lambda results in bigger l2-norm

# coefficients associated with a value of lambda, s here
predict(ridge_mod, s = 50, type = "coefficients")[1:20, ]

## Estimate the test errors of ridge/ lasso regression
# split into training/ test sets
set.seed(1)
train <- sample(1:nrow(x), nrow(x) / 2)
test <- (-train) # exclude train
y_test <- y[test]

ridge_mod <- glmnet(x = x[train, ], y = y[train], alpha = 0, 
                    lambda = grid, thresh = 1e-12)

# fit ridge regression with lambda = 4
ridge_pred <- predict(ridge_mod, s = 4, newx = x[test, ])
mean((ridge_pred - y_test) ** 2)

# if fit using mean from the training set (i.e. only intercepts)
mean((mean(y[train]) - y_test) ** 2)

# fit only intercepts corresponds to lambda as a big value
ridge_pred2 <- predict(ridge_mod, s = 1e10, newx = x[test, ])
mean((ridge_pred2 - y_test) ** 2)

# So fit lambda = 4 is better than just fitting an intercept
# now check against ordinary least square, lambda = 0
ridge_pred3 <- predict(ridge_mod, s = 0, newx = x[test, ])
mean((ridge_pred3 - y_test) ** 2)

# 10-fold CV by default to select lambda
set.seed(1)
cv_out <- cv.glmnet(x[train, ], y[train], alpha = 0)
plot(cv_out)
bestlam <- cv_out$lambda.min

ridge_pred4 <- predict(ridge_mod, s = bestlam, newx = x[test, ]) 
mean((ridge_pred4 - y_test) ** 2)

# lasso
lasso_mod <- glmnet(x = x[train, ], y = y[train], alpha = 1, lambda = grid)

# cv for selecting lambda
set.seed(1)
cv_out <- cv.glmnet(x[train, ], y[train], alpha = 1)
bestlamlasso <- cv_out$lambda.min
lasso_pred <- predict(lasso_mod, s = bestlamlasso, newx = x[test, ])
mean((lasso_pred - y_test) ** 2)

out <- glmnet(x[train, ], y[train], alpha = 1)

# some of the coefficients are zeroised
lasso_coef <- predict(out, s = bestlamlasso, type = "coefficients")[1:20, ]

# PCR
library(pls)
set.seed(2)
pcr_fit <- pcr(Salary ~ ., data = hitters_df, scale = TRUE, validation = "CV")
summary(pcr_fit)
validationplot(pcr_fit, val.type = "MSEP")

# fit on train, predict on test
set.seed(1)
pcr_fit <- pcr(Salary ~ ., data = hitters_df, subset = train, scale = TRUE, 
               validation = "CV")
validationplot(pcr_fit, val.type = "MSEP") # how to extract the ncomp again?
pcr_pred <- predict(pcr_fit, x[test, ], ncomp = 7) # recall x is a design matrix
mean((pcr_pred-hitters_df$Salary[test] )** 2)

# fit on full data
pcr_fit <- pcr(y ~ x, scale = TRUE, ncomp = 7)
summary(pcr_fit)

# Partial Least Squares (train/ test split)
set.seed(1)
pls_fit <- plsr(Salary ~ ., data = hitters_df, subset = train, scale = TRUE,
                validation = "CV")
summary(pls_fit) # ncomp = 2 (eyeballing on the statistic) yields the lowest CV MSE
pls_pred <- predict(pls_fit, newdata = x[test, ], ncomp = 2)
mean((pls_pred - hitters_df$Salary[test]) ** 2)

# PLS (full data)
pls_fit <- plsr(y ~ x, scale = TRUE, ncomp = 2)
summary(pls_fit) # note that 2 variables explain about 46% of variance in salary
                 # compared to 7 variables used in PCR for the same example
# pls_pred <- predict(pls_fit)
# mean((pls_pred - hitters_df$Salary) ** 2)
