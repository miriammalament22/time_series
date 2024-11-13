# -----------------------------------------------------------------------------
# Libraries
# -----------------------------------------------------------------------------
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
library(dplyr)    # For data manipulation
library(ggplot2)  # For plotting
library(broom)    # For tidy regression outputs
library(lfe)      # For IV regressions
library(haven)    # For reading .dta files
library(stargazer)# For pretty tables
library(estimatr) # For lm_robust
library(knitr)    # For markdown tables
library(magrittr)
library(texreg)
library(car)
library(tidyverse)
library(vtable)
library(skimr)
library(zoo)      # For managing dates
library(mFilter)
library(fastDummies)
library(tseries)
library(Hmisc)

# -----------------------------------------------------------------------------
# --- Points 1 and 2 ---
# -----------------------------------------------------------------------------

# Load and prepare the data
setwd("C:/Users/Magia/Documents/Econometria")
inf_og <- read_excel("inf_tcn_base.xlsx")

# Create log variables
inf_og$log_ipc <- log(inf_og$ipc)
inf_og$log_tcn <- log(inf_og$tc_nom)

inf <- subset(inf_og, period <= 204)

# Create time series
ts_log_ipc <- ts(inf$log_ipc, start = c(2003, 1), frequency = 12)
ts_log_tcn <- ts(inf$log_tcn, start = c(2003, 1), frequency = 12)

# Plot time series
windows(width = 10, height = 8)
autoplot(cbind(ts_log_ipc, ts_log_tcn)) + 
  ggtitle("Time Series of log(ipc) and log(tcn)") +
  xlab("Year") + 
  ylab("Log Value") +
  theme_minimal()

# Create differenced time series
ts_diff_log_ipc <- diff(ts_log_ipc)
ts_diff_log_tcn <- diff(ts_log_tcn)

# Plot differenced time series
windows(width = 10, height = 8)
autoplot(cbind(ts_diff_log_ipc, ts_diff_log_tcn)) + 
  ggtitle("Differenced Time Series of log(ipc) and log(tcn)") + 
  xlab("Year") + 
  ylab("Differenced Log Value") +
  theme_minimal()

# -----------------------------------------------------------------------------
# --- Points 3 and 4 ---
# -----------------------------------------------------------------------------

# Estimate the model using dynlm
reg_1 <- dynlm(ts_diff_log_ipc ~ ts_diff_log_tcn + L(ts_diff_log_tcn, 1:12), start = c(2004, 2), end = c(2019, 12))
summary(reg_1)
coeftest(reg_1)
coeftest(reg_1, vcov. = NeweyWest(reg_1, lag = 12, adjust = TRUE, verbose = TRUE))
stargazer(reg_1)

# Residuals
res_reg_1 <- reg_1$residuals

# Plot ACF and PACF of residuals
windows(width = 10, height = 8)
acf(res_reg_1, lag.max = 20, main = "ACF of Residuals") 
pacf(res_reg_1, lag.max = 20, main = "PACF of Residuals")

# Alternative model specification for linear hypothesis tests
reg_2 <- dynlm(ts_diff_log_ipc ~ ts_diff_log_tcn + L(ts_diff_log_tcn, 1:12), start = c(2004, 2), end = c(2019, 12))
summary(reg_2)
coeftest(reg_2)
coeftest(reg_2, vcov. = NeweyWest(reg_2, lag = 12, adjust = TRUE, verbose = TRUE))
stargazer(reg_2)

# Impact effect (contemporaneous, N = 0)
eff_IMP <- summary(reg_1)$coefficients[2, 1]
lht(reg_1, "ts_diff_log_tcn = 0", white.adjust = "hc1")

# Short-run effects
eff_1 <- summary(reg_1)$coefficients[2, 1] + summary(reg_1)$coefficients[3, 1]
lht(reg_2, "ts_diff_log_tcn + L(ts_diff_log_tcn, 1) = 0", white.adjust = "hc1")

eff_3 <- summary(reg_1)$coefficients[2, 1] + summary(reg_1)$coefficients[3, 1] + summary(reg_1)$coefficients[4, 1] + summary(reg_1)$coefficients[5, 1]
lht(reg_2, "ts_diff_log_tcn + L(ts_diff_log_tcn, 1) + L(ts_diff_log_tcn, 2) + L(ts_diff_log_tcn, 3) = 0", white.adjust = "hc1")

eff_6 <- summary(reg_1)$coefficients[2, 1] + summary(reg_1)$coefficients[3, 1] + summary(reg_1)$coefficients[4, 1] + summary(reg_1)$coefficients[5, 1] + summary(reg_1)$coefficients[6, 1] + summary(reg_1)$coefficients[7, 1] + summary(reg_1)$coefficients[8, 1]
lht(reg_2, "ts_diff_log_tcn + L(ts_diff_log_tcn, 1) + L(ts_diff_log_tcn, 2) + L(ts_diff_log_tcn, 3) + L(ts_diff_log_tcn, 4) + L(ts_diff_log_tcn, 5) + L(ts_diff_log_tcn, 6) = 0", white.adjust = "hc1")

# Long-run effect (N = 12)
eff_LR <- summary(reg_1)$coefficients[2, 1] + summary(reg_1)$coefficients[3, 1] + summary(reg_1)$coefficients[4, 1] + summary(reg_1)$coefficients[5, 1] + summary(reg_1)$coefficients[6, 1] + summary(reg_1)$coefficients[7, 1] + summary(reg_1)$coefficients[8, 1] + summary(reg_1)$coefficients[9, 1] + summary(reg_1)$coefficients[10, 1] + summary(reg_1)$coefficients[11, 1] + summary(reg_1)$coefficients[12, 1] + summary(reg_1)$coefficients[13, 1]
lht(reg_2, "ts_diff_log_tcn + L(ts_diff_log_tcn, 1) + L(ts_diff_log_tcn, 2) + L(ts_diff_log_tcn, 3) + L(ts_diff_log_tcn, 4) + L(ts_diff_log_tcn, 5) + L(ts_diff_log_tcn, 6) + L(ts_diff_log_tcn, 7) + L(ts_diff_log_tcn, 8) + L(ts_diff_log_tcn, 9) + L(ts_diff_log_tcn, 10) + L(ts_diff_log_tcn, 11) + L(ts_diff_log_tcn, 12) = 0", white.adjust = "hc1")

# -----------------------------------------------------------------------------
# --- Points 5, 6, and 7 ---
# -----------------------------------------------------------------------------

# Estimate ADL model using dynlm
reg_adl <- dynlm(ts_diff_log_ipc ~ ts_diff_log_tcn + L(ts_diff_log_ipc, 1), start = c(2004, 2), end = c(2019, 12))
summary(reg_adl)
coeftest(reg_adl)
coeftest(reg_adl, vcov. = NeweyWest(reg_adl, lag = 12, adjust = TRUE, verbose = TRUE))
stargazer(reg_adl)

# Residuals
res_reg_adl <- reg_adl$residuals

# Plot ACF and PACF of residuals
windows(width = 10, height = 8)
acf(res_reg_adl, lag.max = 20, main = "ACF of ADL Model Residuals")
pacf(res_reg_adl, lag.max = 20, main = "PACF of ADL Model Residuals")

# Impact effect (contemporaneous, N = 0)
eff_adl_IMP <- summary(reg_adl)$coefficients[2, 1]
nlWaldtest(reg_adl, "b[2] = 0")

# Short-run effects
eff_adl_1 <- summary(reg_adl)$coefficients[2, 1] * (1 + summary(reg_adl)$coefficients[3, 1])
nlWaldtest(reg_adl, "b[2]*(1 + b[3]) = 0", white.adjust = "hc1")

eff_adl_3 <- summary(reg_adl)$coefficients[2, 1] * (1 + (summary(reg_adl)$coefficients[3, 1])^1 + (summary(reg_adl)$coefficients[3, 1])^2 + (summary(reg_adl)$coefficients[3, 1])^3)
nlWaldtest(reg_adl, "b[2] * (1 + (b[3])^1 + (b[3])^2 + (b[3])^3) = 0", white.adjust = "hc1")

eff_adl_6 <- summary(reg_adl)$coefficients[2, 1] * (1 + (summary(reg_adl)$coefficients[3, 1])^1 + (summary(reg_adl)$coefficients[3, 1])^2 + (summary(reg_adl)$coefficients[3, 1])^3 + (summary(reg_adl)$coefficients[3, 1])^4 + (summary(reg_adl)$coefficients[3, 1])^5 + (summary(reg_adl)$coefficients[3, 1])^6)
nlWaldtest(reg_adl, "b[2] * (1 + (b[3])^1 + (b[3])^2 + (b[3])^3 + (b[3])^4 + (b[3])^5 + (b[3])^6) = 0", white.adjust = "hc1")

eff_adl_12 <- summary(reg_adl)$coefficients[2, 1] * (1 + (summary(reg_adl)$coefficients[3, 1])^1 + (summary(reg_adl)$coefficients[3, 1])^2 + (summary(reg_adl)$coefficients[3, 1])^3 + (summary(reg_adl)$coefficients[3, 1])^4 + (summary(reg_adl)$coefficients[3, 1])^5 + (summary(reg_adl)$coefficients[3, 1])^6 + (summary(reg_adl)$coefficients[3, 1])^7 + (summary(reg_adl)$coefficients[3, 1])^8 + (summary(reg_adl)$coefficients[3, 1])^9 + (summary(reg_adl)$coefficients[3, 1])^10 + (summary(reg_adl)$coefficients[3, 1])^11 + (summary(reg_adl)$coefficients[3, 1])^12)
nlWaldtest(reg_adl, "b[2] * (1 + (b[3])^1 + (b[3])^2 + (b[3])^3 + (b[3])^4 + (b[3])^5 + (b[3])^6 + (b[3])^7 + (b[3])^8 + (b[3])^9 + (b[3])^10 + (b[3])^11 + (b[3])^12) = 0", white.adjust = "hc1")

# Long-run effect
eff_adl_LR <- summary(reg_adl)$coefficients[2, 1] / (1 - summary(reg_adl)$coefficients[3, 1])
nlWaldtest(reg_adl, "b[2]/(1 - b[3]) = 0", white.adjust = "hc1")

# -----------------------------------------------------------------------------
# --- Punto 8 ---
# -----------------------------------------------------------------------------

# Model comparison using information criteria
glance(reg_2)
glance(reg_adl)

AIC(reg_2)
AIC(reg_adl)

BIC(reg_2)
BIC(reg_adl)

# The ADL model is better based on information criteria (Refer to Appendix below)

# -----------------------------------------------------------------------------
# --- Punto 9 ---
# -----------------------------------------------------------------------------

# Caution while inferring causality: consider factors impacting both tcn and ipc
# Think about simultaneity: inflation causes devaluations in tcn, and devaluations in tcn 
# can propagate inflation, impacting local prices of tradable goods.

# Summary: asserting causality is highly complex.

# -----------------------------------------------------------------------------
# Appendix: Finding the best model
# -----------------------------------------------------------------------------

k <- 1
akaike <- list()
for (i in 0:12) {
  for (j in 0:12) {
    if (j == 0) {
      akaike[[k]] <- dynlm(ts_diff_log_ipc ~ ts_diff_log_tcn + L(ts_diff_log_tcn, 0:i), start = c(2004, 2), end = c(2019, 12))
    } else {
      akaike[[k]] <- dynlm(ts_diff_log_ipc ~ ts_diff_log_tcn + L(ts_diff_log_ipc, 1:j) + L(ts_diff_log_tcn, 0:i), start = c(2004, 2), end = c(2019, 12))
    }
    k <- k + 1
  }
}

best_mod_aic <- sapply(akaike, AIC)
best_mod_bic <- sapply(akaike, BIC)

# Best model by BIC criteria is model [[54]], or i = 4 and j = 1
best_model_index <- which.min(best_mod_bic)

# Apply best model
reg_best <- dynlm(ts_diff_log_ipc ~ ts_diff_log_tcn + L(ts_diff_log_tcn, 1:4) + L(ts_diff_log_ipc, 1), start = c(2004, 2), end = c(2019, 12))
coeftest(reg_best)
coeftest(reg_best, vcov. = NeweyWest(reg_best, lag = 12, adjust = TRUE, verbose = TRUE))

res_reg_best <- reg_best$residuals

# Plot ACF and PACF of best model residuals
windows(width = 10, height = 8)
acf(res_reg_best, lag.max = 20, main = "ACF of Best Model Residuals")
pacf(res_reg_best, lag.max = 20, main = "PACF of Best Model Residuals")

# Compare models
AIC(reg_adl)
AIC(reg_best)

BIC(reg_adl)
BIC(reg_best)

summary(reg_best)

# Estimate effects for the best model
eff_best_1 <- summary(reg_best)$coefficients[3, 1] + summary(reg_best)$coefficients[2, 1] * (1 + summary(reg_best)$coefficients[7, 1])
nlWaldtest(reg_best, "b[3] + b[2] * (1 + b[7]) = 0", white.adjust = "hc1")

# ----------- End of Appendix ----------- 