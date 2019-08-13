rw <- arima.sim(model = list(order = c(0, 1, 0)), n = 100, sd = 2)
ts.plot(rw)

rw_diff <- diff(rw)
ts.plot(rw_diff)

rw_diff_est <- arima(rw_diff, order = c(0, 0, 0))
rw_diff_est$coef
rw_diff_est$sigma2
ts.plot(rw)
abline(a = 0, b = rw_diff_est$coef)
