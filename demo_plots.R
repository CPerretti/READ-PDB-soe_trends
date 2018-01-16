#set.seed(123)
ts2plot <-
  -0.262 + (0.051 * c(1:30)) + #(0.05 * c(1:30))^2 +
  arima.sim(list(ar = 0.433),
            n=30, 
            rand.gen=rnorm, 
            sd = 0.54^0.5)

dat <- data.frame(series = as.numeric(ts2plot),
                  time = 1:30)




# Fit linear model
lm_out <- fit_lm(dat = dat)
lm_out_last10 <- fit_lm(dat = dat %>% 
                          dplyr::filter(time >= 20))

newtime <- seq(min(dat$time), max(dat$time), length.out=100)
newdata <- data.frame(time = newtime,
                      time2 = newtime^2)
lm_pred <- AICcmodavg::predictSE(lm_out$model, 
                                 newdata = newdata,
                                 se.fit = TRUE)
newtime_last10 <- seq(21, 30, length.out=10)
newdata_last10 <- data.frame(time = newtime_last10,
                             time2 = newtime_last10^2)
lm_pred_last10 <- AICcmodavg::predictSE(lm_out_last10$model, 
                                       newdata = newdata_last10,
                                       se.fit = TRUE)
lm_ci95_last10 <- 
  lm_pred_last10$fit + 1.96*lm_pred_last10$se.fit
lm_ci05_last10 <- 
  lm_pred_last10$fit - 1.96*lm_pred_last10$se.fit

# Make plot
plot(series~time, data = dat)
polygon(c(rev(newtime_last10), newtime_last10), 
        c(rev(lm_ci95_last10), lm_ci05_last10), 
        col = 'grey95', border = NA)
lines(newtime_last10, lm_ci95_last10, 
      lty = 'dashed', col = 'black')
lines(newtime_last10, lm_ci05_last10, 
      lty = 'dashed', col = 'black')
lines(newtime, lm_pred$fit, col = "orange")
lines(series ~ time, data = dat)
points(series ~ time, data = dat)





