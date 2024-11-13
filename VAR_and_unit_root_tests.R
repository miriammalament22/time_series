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
library(jtool)
library(car)
library(tidyverse)
library(vtable)
library(skimr)
library(zoo)
library(mFilter)
library(fastDummies)
library(tseries)
library(Hmisc)
library(vars)

# -----------------------------------------------------------------------------
# --- Ex 1, Punto 1 ---
# -----------------------------------------------------------------------------

# Load and prepare the data
setwd("C:/Users/Magia/Documents/Econometria")
inf_og <- read_excel("inf_tcn_base.xlsx")

# Create log variables and subset the data
inf_og <- inf_og %>%
  mutate(log_ipc = log(ipc), log_tcn = log(tc_nom)) %>%
  filter(period <= 204)

# Create time series for log(ipc) and log(tcn)
ts_log_ipc <- ts(inf_og$log_ipc, start = c(2003, 1), frequency = 12)
ts_log_tcn <- ts(inf_og$log_tcn, start = c(2003, 1), frequency = 12)

# Plot log time series
windows(width = 10, height = 8)
autoplot(cbind(ts_log_ipc, ts_log_tcn), facets = TRUE)

# Create differenced variables
ts_diff_log_ipc <- diff(ts_log_ipc) * 100
ts_diff_log_tcn <- diff(ts_log_tcn) * 100

# Plot differenced time series
windows(width = 10, height = 8)
autoplot(cbind(ts_diff_log_ipc, ts_diff_log_tcn), facets = TRUE)

# Create dataframe for VAR
df_var <- cbind(ts_diff_log_tcn, ts_diff_log_ipc)
colnames(df_var) <- c("tcn", "ipc")

# Run VAR and SVAR model
lag_select <- VARselect(df_var, lag.max = 8)
lag_select$selection

var1 <- VAR(df_var, p = 5, type = "const")
summary(var1)

# Cholesky decomposition matrix
mat <- matrix(c(NA, 0, NA, NA), ncol = 2, byrow = TRUE)
C <- matrix(data = mat, ncol = 2, nrow = 2, byrow = TRUE)
C

svar1 <- SVAR(var1, p = 5, Amat = C, estmethod = c("scoring", "direct"), hessian = TRUE)
summary(svar1)

irf_var2 <- irf(svar1, impulse = "tcn", response = c("tcn", "ipc"), n.ahead = 20, boot = TRUE, ortho = FALSE)

# Plot SVAR impulse response
windows(width = 10, height = 8)
plot(irf_var2)

# Cumulative response
bq1 <- BQ(var1)
summary(bq1)

irf_cum <- irf(bq1, impulse = "tcn", response = c("tcn", "ipc"), n.ahead = 20, boot = TRUE, ortho = FALSE, cumulative = TRUE)

# Plot cumulative impulse response
windows(width = 10, height = 8)
plot(irf_cum)

# EPTR (Exchange Pass-Through Rate) calculation using 20 periods ahead
eptr <- 4.0675201 / 7.642981
eptr


# -----------------------------------------------------------------------------
# --- Ex 1, Punto 2 ---
# -----------------------------------------------------------------------------

library(lpirfs)
library(tidyverse)
library(ggpubr)
library(gridExtra)
library(readxl)
library(vars)
library(ggplot2)
library(zoo)
library(writexl)

# Prepare endogenous data
inf_lp <- inf_og %>%
  filter(period > 1) %>%
  mutate(diff_log_ipc = diff(log_ipc) * 100, diff_log_tcn = diff(log_tcn) * 100)

endog_data <- inf_lp %>% select(diff_log_ipc, diff_log_tcn)
shock <- inf_lp %>% select(diff_log_tcn)

# Estimate linear model
results_lin_iv <- lp_lin_iv(endog_data, lags_endog_lin = 5, shock = shock, trend = 0, confint = 1.96, hor = 20)

# Plot results
iv_lin_plots <- plot_lin(results_lin_iv)

windows(width = 10, height = 8)
plot_lin(results_lin_iv)[[1]]

windows(width = 10, height = 8)
plot_lin(results_lin_iv)[[2]]

# Calculate EPTR
resp_imp_tcn <- results_lin_iv$irf_lin_mean[2, 1]
resp_acum_ipc <- cumsum(results_lin_iv$irf_lin_mean[1, 1:20])[20]
resp_acum_tcn <- cumsum(results_lin_iv$irf_lin_mean[2, 1:20])[20]

eptr_imp_lp <- resp_acum_ipc / resp_imp_tcn
eptr_imp_lp

eptr_acum_lp <- resp_acum_ipc / resp_acum_tcn
eptr_acum_lp

# EPTR is around 48.4% for 20 periods ahead


# -----------------------------------------------------------------------------
# --- Ex 2 ---
# -----------------------------------------------------------------------------

# Load additional datasets
base_1 <- read_csv("ARG_TCR_month.csv")
base_2 <- read_csv("ARG_TCR_year.csv")
base_3 <- read_csv("ARG_TCR_multi.csv")

# Create necessary variables
base_1 <- base_1 %>%
  mutate(TCR_arg = log(ner) + log(ipc_usa) - log(ipc_arg))

# Create time series for plotting
log_tcr_1 <- ts(base_1$TCR_arg, start = c(1943, 1), frequency = 12)
log_tcr_2 <- ts(log(base_2$TCR_arg), start = 1810, frequency = 1)
log_tcr_3 <- ts(log(base_3$tcr_multi), start = c(1991, 1), frequency = 12)

# Plot log time series
autoplot(log_tcr_1)
autoplot(log_tcr_2)
autoplot(log_tcr_3)

# Unit root tests
# 1. DF Test
df1 <- ur.df(log_tcr_1, type = "drift", lags = 0)
summary(df1)

df2 <- ur.df(log_tcr_2, type = "trend", lags = 0)
summary(df2)

df3 <- ur.df(log_tcr_3, type = "drift", lags = 0)
summary(df3)

# 2. Augmented Dickey-Fuller Test
df1_aic <- ur.df(log_tcr_1, type = "drift", lags = 12, selectlags = "AIC")
summary(df1_aic)
df1_bic <- ur.df(log_tcr_1, type = "drift", lags = 12, selectlags = "BIC")
summary(df1_bic)

df2_aic <- ur.df(log_tcr_2, type = "trend", lags = 12, selectlags = "AIC")
summary(df2_aic)
df2_bic <- ur.df(log_tcr_2, type = "trend", lags = 12, selectlags = "BIC")
summary(df2_bic)

df3_aic <- ur.df(log_tcr_3, type = "drift", lags = 12, selectlags = "AIC")
summary(df3_aic)
df3_bic <- ur.df(log_tcr_3, type = "drift", lags = 12, selectlags = "BIC")
summary(df3_bic)

# 3. DF-GLS Test
dfgls1 <- ur.ers(log_tcr_1, type = "DF-GLS", model = "constant", lag.max = 1)
summary(dfgls1)

dfgls2 <- ur.ers(log_tcr_2, type = "DF-GLS", model = "trend", lag.max = 1)
summary(dfgls2)

dfgls3 <- ur.ers(log_tcr_3, type = "DF-GLS", model = "constant", lag.max = 2)
summary(dfgls3)