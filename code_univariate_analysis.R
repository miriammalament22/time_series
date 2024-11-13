# Load libraries
library(plm)
library(AER)
library(jtools)
library(moments)
library(readxl)
library(ggthemes)
library(foreign)
library(readr)
library(multiwayvcov)
library(stats)
library(summarytools)
library(sandwich)
library(ggfortify)
library(dynlm)
library(lmtest)
library(nlme)
library(orcutt)
library(quantmod)
library(urca)
library(forecast)
library(dplyr)
library(ggplot2)
library(broom)
library(lfe)
library(haven)
library(stargazer)
library(estimatr)
library(knitr)
library(magrittr)
library(texreg)
library(car)
library(tidyverse)
library(vtable)
library(skimr)
library(zoo)
library(mFilter)
library(fastDummies)
library(Hmisc)

# -----------------------------------------------------------------------------
# --- Ejercicio 1 ---

# Set working directory
setwd("C:/Users/Magia/Documents/Econometria")

# Load the EMAE dataset from Excel
EMAE <- read_excel("emae_update.xlsx")
View(EMAE)

# Create date column, log(EMAE) variable, and temporal variables
EMAE <- EMAE %>%
  mutate(
    date = as.yearmon(paste(year, month), "%Y %m"),
    log_emae = log(emae),
    t = row_number(),
    t2 = t^2,
    t3 = t^3,
    t4 = t^4,
    t5 = t^5
  )

# Create subsets for data up to 2019 and 2021
EMAE_2019 <- filter(EMAE, year <= 2019)
EMAE_2021 <- filter(EMAE, year <= 2021)

# Plot the original series to analyze trend and seasonality
plot(EMAE$date, EMAE$emae, type = "l", lwd = 2, col = "black", xlab = "Years", ylab = "EMAE")

# Plot the log(EMAE) series
plot(EMAE$date, EMAE$log_emae, type = "l", lwd = 2, col = "black", xlab = "Years", ylab = "log(EMAE)")

# -- Punto 1 --
# Create and compare models for both sets
models_2019 <- list(
  lm(log_emae ~ t, data = EMAE_2019),
  lm(log_emae ~ t + t2, data = EMAE_2019),
  lm(log_emae ~ t + t2 + t3, data = EMAE_2019)
)
models_2021 <- list(
  lm(log_emae ~ t, data = EMAE_2021),
  lm(log_emae ~ t + t2, data = EMAE_2021),
  lm(log_emae ~ t + t2 + t3, data = EMAE_2021)
)

stargazer(models_2019, se = starprep(models_2019, se_type = "stata"), type = "latex")

stargazer(models_2021, se = starprep(models_2021, se_type = "stata"), type = "latex")

# Plot trends to compare in the pre-pandemic period
# Linear Trend
windows(width = 10, height = 8)
log_emae_hat_2019_t1 <- predict(models_2019[[1]])
log_emae_hat_2021_t1 <- predict(models_2021[[1]])
plot(EMAE_2019$date, log_emae_hat_2019_t1, type = "l", lwd = 2, col = "red", xlim = c(1993, 2020), ylim = c(4.3, 5.2), xlab = "Years", ylab = "log(EMAE)")
lines(EMAE_2021$date, log_emae_hat_2021_t1, type = "l", lwd = 2, col = "blue")
lines(EMAE_2021$date, EMAE_2021$log_emae, type = "l", lwd = 1, col = "black")
legend("topleft", legend = c("log_emae", "2019", "2021"), col = c("black", "red", "blue"), lty = 1:3, ncol = 1, cex = 1)

# Quadratic Trend
windows(width = 10, height = 8)
log_emae_hat_2019_t2 <- predict(models_2019[[2]])
log_emae_hat_2021_t2 <- predict(models_2021[[2]])
plot(EMAE_2019$date, log_emae_hat_2019_t2, type = "l", lwd = 2, col = "red", xlim = c(1993, 2020), ylim = c(4.3, 5.2), xlab = "Years", ylab = "log(EMAE)")
lines(EMAE_2021$date, log_emae_hat_2021_t2, type = "l", lwd = 2, col = "blue")
lines(EMAE_2021$date, EMAE_2021$log_emae, type = "l", lwd = 1, col = "black")
legend("topleft", legend = c("log_emae", "2019", "2021"), col = c("black", "red", "blue"), lty = 1:3, ncol = 1, cex = 1)

# Cubic Trend
windows(width = 10, height = 8)
log_emae_hat_2019_t3 <- predict(models_2019[[3]])
log_emae_hat_2021_t3 <- predict(models_2021[[3]])
plot(EMAE_2019$date, log_emae_hat_2019_t3, type = "l", lwd = 2, col = "red", xlim = c(1993, 2021), ylim = c(4.3, 5.2), xlab = "Years", ylab = "log(EMAE)")
lines(EMAE_2021$date, log_emae_hat_2021_t3, type = "l", lwd = 2, col = "blue")
lines(EMAE_2021$date, EMAE_2021$log_emae, type = "l", lwd = 1, col = "black")
legend("topleft", legend = c("log_emae", "2019", "2021"), col = c("black", "red", "blue"), lty = 1:3, ncol = 1, cex = 1)

# HP Filter
hpf_2021 <- hpfilter(EMAE_2021$log_emae, type = "lambda", freq = 129600)
out_2021 <- xts(cbind(hpf_2021$x, hpf_2021$trend, hpf_2021$cycle), EMAE_2021$date)
colnames(out_2021) <- c("x", "trend", "cycle")

hpf_2019 <- hpfilter(EMAE_2019$log_emae, type = "lambda", freq = 129600)
out_2019 <- xts(cbind(hpf_2019$x, hpf_2019$trend, hpf_2019$cycle), EMAE_2019$date)
colnames(out_2019) <- c("x", "trend", "cycle")

windows(width = 10, height = 8)
plot(out_2021[, "x"], type = "n", main = "HP Filter log(EMAE), 1993-2019 in red and 1993-2021 in blue", col = "black", xlim = c(1993, 2021), ylim = c(4.3, 5.2))
lines(out_2021[, "trend"], col = "blue", lwd = 2)
lines(out_2019[, "trend"], col = "red", lwd = 2)

EMAE_2021 <- dummy_cols(EMAE_2021, select_columns = 'month')
EMAE_2019 <- dummy_cols(EMAE_2019, select_columns = 'month')

reg_et3_2019 <- lm(log_emae ~ t + t2 + t3 + month_1 + month_2 + month_3 + month_4 + month_5 + month_6 + month_9 + month_10 + month_11, data = EMAE_2019)
reg_et3_2021 <- lm(log_emae ~ t + t2 + t3 + month_1 + month_2 + month_3 + month_4 + month_5 + month_6 + month_9 + month_10 + month_11, data = EMAE_2021)

EMAE_2021 <- mutate(EMAE_2021, emae_cycle_HP = out_2021$cycle)
reg_eHP_2021 <- lm(emae_cycle_HP ~ month_1 + month_2 + month_3 + month_4 + month_5 + month_6 + month_9 + month_10 + month_11, data = EMAE_2021)

# Extract residuals
residuals_2019 <- residuals(reg_et3_2019)
residuals_2021 <- residuals(reg_et3_2021)
residuals_HP_2021 <- residuals(reg_eHP_2021)

windows(width = 10, height = 8)
plot(EMAE_2021$date, residuals_2021, type = "l", lwd = 2, col = "blue", xlab = "Years", ylab = "Cycle log(EMAE)")
lines(EMAE_2019$date, residuals_2019, type = "l", lwd = 2, col = "red")
abline(h = 0, col = "black", lwd = 1, lty = 2)
legend("topright", legend = c("2021", "2019"), col = c("blue", "red"), lty = 1:2, ncol = 1, cex = 1)

# ACF and PACF plots
acf_bos <- list(
  acf(residuals_2019, lag.max = 30, plot = TRUE),
  pacf(residuals_2019, lag.max = 30, plot = TRUE),
  acf(residuals_2021, lag.max = 30, plot = TRUE),
  pacf(residuals_2021, lag.max = 30, plot = TRUE)
)

# ARIMA model selection
ar_aic_bic <- list(
  auto.arima(residuals_2019, ic = "aic", trace = TRUE),
  auto.arima(residuals_2019, ic = "bic", trace = TRUE),
  auto.arima(residuals_2021, ic = "aic", trace = TRUE),
  auto.arima(residuals_2021, ic = "bic", trace = TRUE),
  auto.arima(residuals_HP_2021, ic = "aic", trace = TRUE),
  auto.arima(residuals_HP_2021, ic = "bic", trace = TRUE)
)

# -----------------------------------------------------------------------------
# --- Ejercicio 2 ---

# Set working directory
setwd("C:/Users/Magia/Documents/Econometria")

# Load the inflation dataset from Excel
inf_og <- read_excel("inflation_base")
inf <- subset(inf_og, period <= 218)
View(inf)

# Create date column and temporal variables
inf <- mutate(
  inf,
  date = as.yearmon(paste(year, month), "%Y %m"),
  t = row_number(),
  t2 = t^2,
  t3 = t^3
)

# Plot the original series to analyze trend and seasonality
windows(width = 20, height = 12)
plot(inf$date, inf$mon_inf, type = "l", lwd = 2, col = "black", xlab = "Month", ylab = "Inflation MoM")

# Create and compare models
models <- list(
  lm(mon_inf ~ t, data = inf),
  lm(mon_inf ~ t + t2, data = inf),
  lm(mon_inf ~ t + t2 + t3, data = inf)
)
stargazer(models, se = starprep(models, se_type = "stata"), type = "latex")

# Plot trends to compare
windows(width = 10, height = 8)
inf_hat_t1 <- predict(models[[1]])
plot(inf$date, inf_hat_t1, type = "l", lwd = 2, col = "red", ylim = c(0, 6), xlab = "Month", ylab = "Inflation MoM")
lines(inf$date, inf$mon_inf, type = "l", lwd = 1, col = "black")
legend("topleft", legend = c("Inflation", "Linear trend"), col = c("black", "red"), lty = 1:2, ncol = 1, cex = 1)

windows(width = 10, height = 8)
inf_hat_t2 <- predict(models[[2]])
plot(inf$date, inf_hat_t2, type = "l", lwd = 2, col = "red", ylim = c(0, 6), xlab = "Month", ylab = "Inflation MoM")
lines(inf$date, inf$mon_inf, type = "l", lwd = 1, col = "black")
legend("topleft", legend = c("Inflation", "Cuadratic trend"), col = c("black", "red"), lty = 1:2, ncol = 1, cex = 1)

windows(width = 10, height = 8)
inf_hat_t3 <- predict(models[[3]])
plot(inf$date, inf_hat_t3, type = "l", lwd = 2, col = "red", ylim = c(0, 6), xlab = "Month", ylab = "Inflation MoM")
lines(inf$date, inf$mon_inf, type = "l", lwd = 1, col = "black")
legend("topleft", legend = c("Inflation", "Cubic trend"), col = c("black", "red"), lty = 1:2, ncol = 1, cex = 1)

windows(width = 10, height = 8)
plot(inf$date, inf_hat_t1, type = "l", lwd = 2, col = "red", ylim = c(0, 6), xlab = "Month", ylab = "Inflation MoM")
lines(inf$date, inf_hat_t2, type = "l", lwd = 2, col = "blue")
lines(inf$date, inf_hat_t3, type = "l", lwd = 2, col = "green")
lines(inf$date, inf$mon_inf, type = "l", lwd = 2, col = "black")
legend("topleft", legend = c("Inflation", "Linear trend", "Cuadratic trend", "Cubic trend"), col = c("black", "red", "blue", "green"), lty = 1:4, ncol = 1, cex = 1)

inf <- dummy_cols(inf, select_columns = 'month')

reg_et1 <- lm(mon_inf ~ t + month_1 + month_2 + month_3 + month_4 + month_5 + month_6 + month_9 + month_10 + month_11, data = inf)
stargazer(models[[1]], reg_et1, se = starprep(list(models[[1]], reg_et1), se_type = "stata"), type = "text")

residuals_et1 <- residuals(reg_et1)
checkresiduals(residuals_et1)

windows(width = 20, height = 12)
plot(inf$date, residuals_et1, type = "l", lwd = 2, col = "blue", xlab = "Month", ylab = "Cycle Inflation MoM")
abline(h = 0, col = "black", lwd = 1, lty = 2)
legend("topright", legend = c("2021", "2019"), col = c("blue", "red"), lty = 1:2, ncol = 1, cex = 1)

# ACF and PACF plots
acf_pacf_bos <- list(
  acf(residuals_et1, lag.max = 30, plot = TRUE),
  pacf(residuals_et1, lag.max = 30, plot = TRUE)
)

ar_aic_bic <- list(
  auto.arima(residuals_et1, ic = "aic", trace = TRUE),
  auto.arima(residuals_et1, ic = "bic", trace = TRUE)
)

inf_reg <- subset(inf, select = -c(t2, t3))
regressors <- inf_reg[, 6:17]
ar_complete <- Arima(inf$mon_inf, xreg = as.matrix(regressors), order = c(1, 0, 0))
summary(ar_complete)
coeftest(ar_complete)
checkresiduals(ar_complete)

# Plotting residuals for AR Complete model
windows(width = 20, height = 12)
plot(inf$date, residuals(ar_complete), type = "l", lwd = 2, col = "blue",
     xlab = "Month", ylab = "Residuals Inflation MoM")
abline(h = 0, col = "black", lwd = 1, lty = 2)
legend("topright", legend = c("Residuals"), col = c("blue"), lty = 1, ncol = 1, cex = 1)

# Observing the ACF and PACF of the cycles
windows(width = 10, height = 8)
acf(residuals(ar_complete), lag.max = 30, plot = TRUE)

windows(width = 10, height = 8)
pacf(residuals(ar_complete), lag.max = 30, plot = TRUE)

# Box-Ljung test on residuals
Box.test(residuals(ar_complete), lag = 1, type = "Ljung")

# Forecasting future values
inf_og$t <- as.numeric(rownames(inf_og))
inf_og <- dummy_cols(inf_og, select_columns = 'month')
inf_og <- subset(inf_og, select = -c(t2, t3))

x_future <- inf_og[219:228, 6:17]
fcast <- forecast(ar_complete, xreg = as.matrix(x_future))
autoplot(fcast) + xlab("Month") + ylab("Inflation MoM")

# Predictions and Confidence Intervals
prediction <- data.frame(
  lower = fcast$lower[, 2],  # 95% lower limit
  mean = fcast$mean,         # Point forecast
  upper = fcast$upper[, 2]   # 95% upper limit
)

inf_og <- inf_og %>%
  mutate(
    lower = ifelse(period <= 218, mon_inf, prediction$lower),
    puntual = ifelse(period <= 218, mon_inf, prediction$mean),
    upper = ifelse(period <= 218, mon_inf, prediction$upper)
  )

# Plotting the forecast
windows(width = 20, height = 12)
plot(inf_og$date, inf_og$puntual, type = "l", lwd = 2, col = "red", 
     xlab = "Years", ylab = "Inflation MoM")
lines(inf_og$date, inf_og$lower, type = "l", lwd = 1, col = "blue")
lines(inf_og$date, inf_og$upper, type = "l", lwd = 1, col = "blue")
lines(inf_og$date[1:218], inf_og$mon_inf[1:218], type = "l", lwd = 1, col = "black")

# Forecasting Inflation for 2022
inf_anual_2022_FC <- ((1 + inf_og$mon_inf[217] / 100) * (1 + inf_og$mon_inf[218] / 100) *
                        (1 + inf_og$puntual[219] / 100) * (1 + inf_og$puntual[220] / 100) * 
                        (1 + inf_og$puntual[221] / 100) * (1 + inf_og$puntual[222] / 100) * 
                        (1 + inf_og$puntual[223] / 100) * (1 + inf_og$puntual[224] / 100) * 
                        (1 + inf_og$puntual[225] / 100) * (1 + inf_og$puntual[226] / 100) * 
                        (1 + inf_og$puntual[227] / 100) * (1 + inf_og$puntual[228] / 100) - 1) * 100

# Search for the best ARMA model up to (6,6) using AIC and BIC
akaike <- list()
k <- 1

for (i in 0:6) {
  for (j in 0:6) {
    akaike[[k]] <- list(arima(residuals_et1, order = c(i, 0, j)))
    k <- k + 1
  }
}

best_mod_aic <- sapply(akaike, AIC)
best_mod_bic <- sapply(akaike, BIC)
rezagos <- data.frame(AIC = best_mod_aic, BIC = best_mod_bic)
rezagos

# Checking manually which models have the lowest AIC and BIC values for ARMA (0,0) to (6,6)
ar_models <- list(
  arima(residuals_et1, order = c(0, 0, 0)), arima(residuals_et1, order = c(1, 0, 0)),
  arima(residuals_et1, order = c(2, 0, 0)), arima(residuals_et1, order = c(3, 0, 0)),
  arima(residuals_et1, order = c(4, 0, 0)), arima(residuals_et1, order = c(5, 0, 0)),
  arima(residuals_et1, order = c(6, 0, 0)), arima(residuals_et1, order = c(0, 0, 1)),
  arima(residuals_et1, order = c(1, 0, 1)), arima(residuals_et1, order = c(2, 0, 1)),
  arima(residuals_et1, order = c(3, 0, 1)), arima(residuals_et1, order = c(4, 0, 1)),
  arima(residuals_et1, order = c(5, 0, 1)), arima(residuals_et1, order = c(6, 0, 1)),
  arima(residuals_et1, order = c(0, 0, 2)), arima(residuals_et1, order = c(1, 0, 2)),
  arima(residuals_et1, order = c(2, 0, 2)), arima(residuals_et1, order = c(3, 0, 2)),
  arima(residuals_et1, order = c(4, 0, 2)), arima(residuals_et1, order = c(5, 0, 2)),
  arima(residuals_et1, order = c(6, 0, 2)), arima(residuals_et1, order = c(0, 0, 3)),
  arima(residuals_et1, order = c(1, 0, 3)), arima(residuals_et1, order = c(2, 0, 3)),
  arima(residuals_et1, order = c(3, 0, 3)), arima(residuals_et1, order = c(4, 0, 3)),
  arima(residuals_et1, order = c(5, 0, 3)), arima(residuals_et1, order = c(6, 0, 3)),
  arima(residuals_et1, order = c(0, 0, 4)), arima(residuals_et1, order = c(1, 0, 4)),
  arima(residuals_et1, order = c(2, 0, 4)), arima(residuals_et1, order = c(3, 0, 4)),
  arima(residuals_et1, order = c(4, 0, 4)), arima(residuals_et1, order = c(5, 0, 4)),
  arima(residuals_et1, order = c(6, 0, 4)), arima(residuals_et1, order = c(0, 0, 5)),
  arima(residuals_et1, order = c(1, 0, 5)), arima(residuals_et1, order = c(2, 0, 5)),
  arima(residuals_et1, order = c(3, 0, 5)), arima(residuals_et1, order = c(4, 0, 5)),
  arima(residuals_et1, order = c(5, 0, 5)), arima(residuals_et1, order = c(6, 0, 5)),
  arima(residuals_et1, order = c(0, 0, 6)), arima(residuals_et1, order = c(1, 0, 6)),
  arima(residuals_et1, order = c(2, 0, 6)), arima(residuals_et1, order = c(3, 0, 6)),
  arima(residuals_et1, order = c(4, 0, 6)), arima(residuals_et1, order = c(5, 0, 6)),
  arima(residuals_et1, order = c(6, 0, 6))
)

aic_bic_values <- data.frame(
  aic = sapply(ar_models, AIC),
  bic = sapply(ar_models, BIC)
)
aic_bic_values