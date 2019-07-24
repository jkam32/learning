library(splines)
library(ISLR)

#### Cubic Splines ####
# cubic splines with knots specification
par(mfrow = c(1, 2))
attach(Wage)
agelims <- range(age)
age_grid <- seq(from = agelims[1], to = agelims[2])
fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)
pred <- predict(fit, newdata = list(age = age_grid), se = TRUE)
plot(age, wage, col = "gray")
lines(age_grid, pred$fit, lwd = 2)
lines(age_grid, pred$fit + 2 * pred$se.fit, lty = "dashed")
lines(age_grid, pred$fit - 2 * pred$se.fit, lty = "dashed")
# detach(Wage)

# cubic splines with uniform knots, by specifying degrees of freedom
dim(bs(age, knots = c(25, 40, 60)))
dim(bs(age, df = 6))
attr(bs(age, df = 6), "knots")

# Natural Cubic Spline ####

# natural cubic spline (smoothed at the boundary)
# note that df = 4 (intercept + x + x2 + x3 + 3 knots + 2 knots at boundary = 9 df)
# However, there are 2 additional constraints at each boundary knot (2 X 2 = 4),
# resulting in 5 df. Since the intercept is already specified in the `lm` formula
# so we further minus 1 df, resulting in df = 4.
# dev.off()
fit2 <- lm(wage ~ ns(age, df = 4), data = Wage)
# summary(fit2)
pred2 <- predict(fit2, newdata = list(age = age_grid), se.fit = TRUE)
plot(age, wage, col = "grey")
lines(age_grid, pred2$fit, col = "red", lwd = 2)
lines(age_grid, pred2$fit + 2 * pred2$se.fit, lty = "dashed")
lines(age_grid, pred2$fit - 2 * pred2$se.fit, lty = "dashed")

# so natural cubic spline has lower standard error at the boundary compared to
# vanilla cubic spline

# smoothing spline ####

par(mfrow = c(1, 1))
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(age, wage, df = 16)
fit2 <- smooth.spline(age, wage, cv = TRUE)
fit2$df
# lines.smooth.spline creates a smoother
lines(fit, col = "red", lwd = 2)

# smoother compared to specifying df manually
lines(fit2, col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"),
       col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)

# local regression / loess ####
plot(age, wage, xlim = agelims, cex = .5, col = "darkgrey")
title("Local Regression")
fit <- loess(wage ~ age, span = .2, data = Wage)
fit2 <- loess(wage ~ age, span = .5, data = Wage)
# span := % of points in the neighborhood of points to be taken for
# regression.
lines(age_grid, predict(fit, data.frame(age = age_grid)), col = "red", 
      lwd = 2)
# the larger the span, the smoother the line
lines(age_grid, predict(fit2, data.frame(age = age_grid)), col = "blue", 
      lwd = 2)

# Generalized Additive Model (GAM) ####
