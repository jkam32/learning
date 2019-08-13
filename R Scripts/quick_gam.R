library(mgcv)
library(gamair)
# data
mcycle <- MASS::mcycle

# check
head(mcycle)
plot(mcycle, type = "l")

# linear model fit
lm_fit <- lm(accel ~ times, data = mcycle)

# visualize model fit
termplot(lm_fit, partial.resid = TRUE, se = TRUE)

# non-linear fit
gam_fit <- gam(accel ~ s(times), data = mcycle) # note the s(.), smooth terms in formula
plot(gam_fit, residuals = TRUE, pch = 1)

# extract the coefficients of basis function, there are 9
coef(gam_fit) 

# testing the GAM model complexity
gam_k3 <- gam(accel ~ s(times, k = 3), data = mcycle)
gam_k20 <- gam(accel ~ s(times, k = 20), data = mcycle)

# visualize
par(mfrow = c(1, 2))
plot(gam_k3, residuals = TRUE, pch = 1)
plot(gam_k20, residuals = TRUE, pch = 1)

# smoothing parameter (\lambda)
gam_fit <- gam(accel ~ s(times), data = mcycle, method = "REML")
gam_fit$sp # smoothing parameter of the model

# fix smoothing parameter at 0.1
gam_fit_s1 <- gam(accel ~ s(times), data = mcycle, sp = 0.1)
gam_fit_s2 <- gam(accel ~ s(times), data = mcycle, sp = .0001)

# plot models
par(mfrow = c(2, 1))
plot(gam_fit_s1, residuals = TRUE, pch = 1)
plot(gam_fit_s2, residuals = TRUE, pch = 1)

# putting complexity and smoothing parameter together
par(mfrow = c(1, 1))
gam_sk <- gam(accel ~ s(times, k = 50), sp = .0001, data = mcycle)
plot(gam_sk, residuals = TRUE, pch = 1)

# multivariate GAM
data("mpg", package = "gamair")
gam_mv <- gam(city.mpg ~ s(weight) + s(length) + s(price), data = mpg)
# see the plot against predictors one at a time
plot(gam_mv, pages = 1)

# add categorical variables
gam_mv_2 <- gam(city.mpg ~ s(weight) + s(length) + s(price) + fuel + drive + style,
                data = mpg, method = "REML")
plot(gam_mv_2, all.terms = TRUE, pages = 1)
