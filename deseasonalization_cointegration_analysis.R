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
library(dplyr)       # For data manipulation
library(ggplot2)     # For plots
library(broom)       # For tidy regression outputs
library(lfe)         # For IV regressions
library(haven)       # For reading .dta files
library(stargazer)   # For pretty tables
library(estimatr)    # For lm_robust
library(knitr)       # For markdown tables
library(magrittr)
library(texreg)
library(jtool)
library(car)
library(tidyverse)
library(vtable)
library(skimr)
library(zoo)         # Date handling
library(mFilter)
library(fastDummies)
library(tseries)
library(forecast)
library(Hmisc)
library(vars)

# -----------------------------------------------------------------------------
# Punto 1 y 2
# -----------------------------------------------------------------------------

# Load the data
base <- read_excel("base_tp5.xlsx")
base$date <- as.yearqtr(paste(base$Year, base$Quarter), "%Y %q")
base$t <- as.numeric(rownames(base))
t <- ts(base$t, start = c(2004, 1), frequency = 4)

# Deseasonalize variables
# Variables to deseasonalize: TCRM, TOT, PTNT, NFA, RBDI
# Create quarter dummy variables
base <- dummy_cols(base, select_columns = 'Quarter')

# Deseasonalize TCRM
ts_TCRM <- ts(base$TCRM, start = c(2004, 1), end = c(2021, 4), frequency = 4)
reg_TCRM <- dynlm(ts_TCRM ~ base$Quarter_1 + base$Quarter_2 + base$Quarter_3, start = c(2004, 1), end = c(2021, 4))
res_reg_TCRM <- reg_TCRM$residuals
windows(width = 10, height = 8)
autoplot(cbind(ts_TCRM, res_reg_TCRM), facets = TRUE)

# Deseasonalize TOT
ts_TOT <- ts(base$TOT, start = c(2004, 1), end = c(2021, 4), frequency = 4)
reg_TOT <- dynlm(ts_TOT ~ base$Quarter_1 + base$Quarter_2 + base$Quarter_3, start = c(2004, 1), end = c(2021, 4))
res_reg_TOT <- reg_TOT$residuals
windows(width = 10, height = 8)
autoplot(cbind(res_reg_TCRM, res_reg_TOT), facets = TRUE)
windows(width = 10, height = 8)
qplot(res_reg_TCRM, res_reg_TOT)

# Deseasonalize PTNT (includes drought dummies)
ts_PTNT <- ts(base$PTNT, start = c(2004, 1), end = c(2021, 4), frequency = 4)
reg_PTNT <- dynlm(ts_PTNT ~ base$Quarter_1 + base$Quarter_2 + base$Quarter_3 + base$Sequia_1 + base$Sequia_2, start = c(2004, 1), end = c(2021, 4))
res_reg_PTNT <- reg_PTNT$residuals
windows(width = 10, height = 8)
autoplot(cbind(res_reg_TCRM, res_reg_PTNT), facets = TRUE)
windows(width = 10, height = 8)
qplot(res_reg_TCRM, res_reg_PTNT)

# Deseasonalize NFA
ts_NFA <- ts(base$NFA, start = c(2004, 1), end = c(2021, 4), frequency = 4)
reg_NFA <- dynlm(ts_NFA ~ base$Quarter_1 + base$Quarter_2 + base$Quarter_3, start = c(2004, 1), end = c(2021, 4))
res_reg_NFA <- reg_NFA$residuals
windows(width = 10, height = 8)
autoplot(cbind(res_reg_TCRM, res_reg_NFA), facets = TRUE)
windows(width = 10, height = 8)
qplot(res_reg_TCRM, res_reg_NFA)

# Deseasonalize RBDI
ts_RBDI <- ts(base$RBDI, start = c(2004, 1), end = c(2021, 4), frequency = 4)
reg_RBDI <- dynlm(ts_RBDI ~ base$Quarter_1 + base$Quarter_2 + base$Quarter_3, start = c(2004, 1), end = c(2021, 4))
res_reg_RBDI <- reg_RBDI$residuals
windows(width = 10, height = 8)
autoplot(cbind(res_reg_TCRM, res_reg_RBDI), facets = TRUE)
windows(width = 10, height = 8)
qplot(res_reg_TCRM, res_reg_RBDI)

# Remove constants from already deseasonalized variables: GC_d and PIBBr_d

# For GC_d
ts_GC_d <- ts(base$GC_d, start = c(2004, 1), end = c(2021, 4), frequency = 4)
reg_GC_d <- dynlm(ts_GC_d ~ 1, start = c(2004, 1), end = c(2021, 4))
res_reg_GC_d <- reg_GC_d$residuals
windows(width = 10, height = 8)
autoplot(cbind(res_reg_TCRM, res_reg_GC_d), facets = TRUE)
windows(width = 10, height = 8)
qplot(res_reg_TCRM, res_reg_GC_d)

# For PIBBr_d
ts_PIBBr_d <- ts(base$PIBBr_d, start = c(2004, 1), end = c(2021, 4), frequency = 4)
reg_PIBBr_d <- dynlm(ts_PIBBr_d ~ 1, start = c(2004, 1), end = c(2021, 4))
res_reg_PIBBr_d <- reg_PIBBr_d$residuals
windows(width = 10, height = 8)
autoplot(cbind(res_reg_TCRM, res_reg_PIBBr_d), facets = TRUE)
windows(width = 10, height = 8)
qplot(res_reg_TCRM, res_reg_PIBBr_d)

# -----------------------------------------------------------------------------
# Punto 3
# -----------------------------------------------------------------------------

# Unit root tests

# TCRM
TCRM_aic <- ur.df(res_reg_TCRM, type = "drift", selectlags = "AIC"); summary(TCRM_aic)
TCRM_bic <- ur.df(res_reg_TCRM, type = "drift", selectlags = "BIC"); summary(TCRM_bic)
dfgls <- ur.ers(res_reg_TCRM, type = "DF-GLS", model = "constant", lag.max = 1)
summary(dfgls)

# TOT
TOT_aic <- ur.df(res_reg_TOT, type = "drift", selectlags = "AIC"); summary(TOT_aic)
TOT_bic <- ur.df(res_reg_TOT, type = "drift", selectlags = "BIC"); summary(TOT_bic)
dfgls <- ur.ers(res_reg_TOT, type = "DF-GLS", model = "constant", lag.max = 1)
summary(dfgls)

# GC_d
GC_d_aic <- ur.df(res_reg_GC_d, type = "trend", selectlags = "AIC"); summary(GC_d_aic)
GC_d_bic <- ur.df(res_reg_GC_d, type = "trend", selectlags = "BIC"); summary(GC_d_bic)
dfgls <- ur.ers(res_reg_GC_d, type = "DF-GLS", model = "trend", lag.max = 1)
summary(dfgls)

# PTNT
PTNT_aic <- ur.df(res_reg_PTNT, type = "drift", selectlags = "AIC"); summary(PTNT_aic)
PTNT_bic <- ur.df(res_reg_PTNT, type = "drift", selectlags = "BIC"); summary(PTNT_bic)
dfgls <- ur.ers(res_reg_PTNT, type = "DF-GLS", model = "constant", lag.max = 1)
summary(dfgls)

# RBDI
RBDI_aic <- ur.df(res_reg_RBDI, type = "trend", selectlags = "AIC"); summary(RBDI_aic)
RBDI_bic <- ur.df(res_reg_RBDI, type = "trend", selectlags = "BIC"); summary(RBDI_bic)
dfgls <- ur.ers(res_reg_RBDI, type = "DF-GLS", model = "trend", lag.max = 1)
summary(dfgls)

# NFA
NFA_aic <- ur.df(res_reg_NFA, type = "trend", selectlags = "AIC"); summary(NFA_aic)
NFA_bic <- ur.df(res_reg_NFA, type = "trend", selectlags = "BIC"); summary(NFA_bic)
dfgls <- ur.ers(res_reg_NFA, type = "DF-GLS", model = "trend", lag.max = 1)
summary(dfgls)

# PIBBr_d
PIBBr_d_aic <- ur.df(res_reg_PIBBr_d, type = "trend", selectlags = "AIC"); summary(PIBBr_d_aic)
PIBBr_d_bic <- ur.df(res_reg_PIBBr_d, type = "trend", selectlags = "BIC"); summary(PIBBr_d_bic)
dfgls <- ur.ers(res_reg_PIBBr_d, type = "DF-GLS", model = "trend", lag.max = 1)
summary(dfgls)

# -----------------------------------------------------------------------------
# Punto 4
# -----------------------------------------------------------------------------

# Regression to test for cointegration
reg1 <- dynlm(res_reg_TCRM ~ res_reg_TOT + res_reg_GC_d + res_reg_PTNT + res_reg_NFA + res_reg_RBDI + res_reg_PIBBr_d + base$Cepo_1 + base$Cepo_2)
summary(reg1)
res_reg1 <- reg1$residuals
residualPlot(reg1)

# Unit root tests on residuals
res_aic <- ur.df(res_reg1, type = "drift", selectlags = "AIC"); summary(res_aic)
res_bic <- ur.df(res_reg1, type = "drift", selectlags = "BIC"); summary(res_bic)
dfgls <- ur.ers(res_reg1, type = "DF-GLS", model = "constant", lag.max = 1)
summary(dfgls) # Value of test-statistic is: -4.6117 < -2.59 (1 pct), reject unit root, cointegration exists

# -----------------------------------------------------------------------------
# Punto 5
# -----------------------------------------------------------------------------

# Optimal lag selection for DOLS model
p <- 1
aic <- list()
for (i in 0:4) {
  for (i_1 in 0:4) {
    for (i_2 in 0:4) {
      for (i_3 in 0:4) {
        for (i_4 in 0:4) {
          for (i_5 in 0:4) {
            aic[p] <- list(dynlm(res_reg_TCRM ~ res_reg_TOT + res_reg_GC_d + res_reg_PTNT + res_reg_NFA + res_reg_RBDI + res_reg_PIBBr_d + base$Cepo_1 + base$Cepo_2 + L(diff(res_reg_TOT), -i:i) + L(diff(res_reg_GC_d), -i_1:i_1) + L(diff(res_reg_PTNT), -i_2:-i_2) + L(diff(res_reg_NFA), -i_3:i_3) + L(diff(res_reg_RBDI), -i_4:i_4) + L(diff(res_reg_PIBBr_d), -i_5:i_5)))
            if (p == 752) {
              print(i)
              print(i_1)
              print(i_2)
              print(i_3)
              print(i_4)
              print(i_5)
            }
            p <- p + 1
          }
        }
      }
    }
  }
}

# Results of information criterion
bet_mod_aic <- lapply(aic, AIC)

# Best Model using AIC
dols_aic <- dynlm(res_reg_TCRM ~ res_reg_TOT + res_reg_GC_d + res_reg_PTNT + res_reg_NFA + res_reg_RBDI + res_reg_PIBBr_d + base$Cepo_1 + base$Cepo_2 + L(diff(res_reg_TOT), -4:4) + L(diff(res_reg_GC_d), -4:4) + L(diff(res_reg_PTNT), 0:0) + L(diff(res_reg_NFA), -4:4) + L(diff(res_reg_RBDI), -4:4) + L(diff(res_reg_PIBBr_d), -4:4))
summary(dols_aic)

coeftest(dols_aic, vcov. = NeweyWest(dols_aic, lag = 6))
texreg(dols_aic)

# -----------------------------------------------------------------------------
# Punto 6
# -----------------------------------------------------------------------------

# Cointegration relationship using the best model coefficients
# Without constant and seasonality
TCRM_eq <- 0.238844 + -2.954558 * res_reg_TOT + 6.687833 * res_reg_GC_d + 1.653245 * res_reg_PTNT + -0.327983 * res_reg_NFA + -0.107959 * res_reg_RBDI + -5.325612 * res_reg_PIBBr_d

windows(width = 10, height = 8)
autoplot(cbind(res_reg_TCRM, TCRM_eq), facets = FALSE)

# With constant and seasonality
const_y_est_TCRM <- reg_TCRM$coefficients[1] + reg_TCRM$coefficients[2] + reg_TCRM$coefficients[3] + reg_TCRM$coefficients[4]
TCRM_eq1 <- const_y_est_TCRM + TCRM_eq

windows(width = 10, height = 8)
autoplot(cbind(ts_TCRM, TCRM_eq1), facets = FALSE)
res_TCRMeq <- -ts_TCRM + TCRM_eq1
autoplot(res_TCRMeq)

# -----------------------------------------------------------------------------
# Punto 7
# -----------------------------------------------------------------------------

# Create data frame for VEC model
dset <- cbind(res_reg_TCRM, res_reg_TOT, res_reg_GC_d, res_reg_PTNT, res_reg_NFA, res_reg_RBDI, res_reg_PIBBr_d)

# Optimal lag selection for VEC model
lagselect <- VARselect(dset, lag.max = 8)
lagselect$selection

# Optimal rank selection using rank.select
rank.select(dset, lag.max = 8, r.max = 1)

# VEC Model with restricted constant and no trend
vec_4 <- VECM(dset, lag = 7, r = 2, estim = "ML", LRinclude = "const")

# Rank tests
summary(rank.test(vec_4, type = "eigen", cval = 0.05))
rank.test(vec_4, type = "trace", cval = 0.05)

# Cointegration coefficients and VEC relationship
summary(vec_4)
u <- res_reg_TCRM + 0.01704939 + 1.8246982 * res_reg_GC_d + 0.6441282 * res_reg_PTNT + 0.064175759 * res_reg_NFA + -0.008019344 * res_reg_RBDI + 0.7899719 * res_reg_PIBBr_d

TCRM_eq_vec <- const_y_est_TCRM + res_reg_TCRM - u

windows(width = 10, height = 8)
autoplot(cbind(ts_TCRM, TCRM_eq_vec), facets = FALSE)

# Using urca package
ctest1 <- ca.jo(dset, type = "trace", ecdet = "const", K = 2, spec = "longrun")
summary(ctest1) # 2 cointegration relationships

# -----------------------------------------------------------------------------
# Punto 8
# -----------------------------------------------------------------------------

# Create new dataset with data until Q4 2019 (pre-COVID) and repeat the analysis

base <- read_excel("base_tp5.xlsx")
base$date <- as.yearqtr(paste(base$Year, base$Quarter), "%Y %q")
base <- dummy_cols(base, select_columns = 'Quarter')
base$t <- as.numeric(rownames(base))
base <- subset(base, base$t <= 64)

# Deseasonalize TCRM
ts_TCRM <- ts(base$TCRM, start = c(2004, 1), end = c(2019, 4), frequency = 4)
reg_TCRM <- dynlm(ts_TCRM ~ base$Quarter_1 + base$Quarter_2 + base$Quarter_3, start = c(2004, 1), end = c(2019, 4))
res_reg_TCRM <- reg_TCRM$residuals
windows(width = 10, height = 8)
autoplot(cbind(ts_TCRM, res_reg_TCRM), facets = TRUE)

# Deseasonalize TOT
ts_TOT <- ts(base$TOT, start = c(2004, 1), end = c(2019, 4), frequency = 4)
reg_TOT <- dynlm(ts_TOT ~ base$Quarter_1 + base$Quarter_2 + base$Quarter_3, start = c(2004, 1), end = c(2019, 4))
res_reg_TOT <- reg_TOT$residuals
windows(width = 10, height = 8)
autoplot(cbind(res_reg_TCRM, res_reg_TOT), facets = TRUE)
windows(width = 10, height = 8)
qplot(res_reg_TCRM, res_reg_TOT)

# Deseasonalize PTNT (includes drought dummies)
# Deseasonalize PTNT
ts_PTNT <- ts(base$PTNT, start = c(2004, 1), end = c(2019, 4), frequency = 4)
reg_PTNT <- dynlm(ts_PTNT ~ base$Quarter_1 + base$Quarter_2 + base$Quarter_3 + base$Sequia_1 + base$Sequia_2, start = c(2004, 1), end = c(2019, 4))
res_reg_PTNT <- reg_PTNT$residuals
windows(width = 10, height = 8)
autoplot(cbind(res_reg_TCRM, res_reg_PTNT), facets = TRUE)
windows(width = 10, height = 8)
qplot(res_reg_TCRM, res_reg_PTNT)

# Deseasonalize NFA
ts_NFA <- ts(base$NFA, start = c(2004, 1), end = c(2019, 4), frequency = 4)
reg_NFA <- dynlm(ts_NFA ~ base$Quarter_1 + base$Quarter_2 + base$Quarter_3, start = c(2004, 1), end = c(2019, 4))
res_reg_NFA <- reg_NFA$residuals
windows(width = 10, height = 8)
autoplot(cbind(res_reg_TCRM, res_reg_NFA), facets = TRUE)
windows(width = 10, height = 8)
qplot(res_reg_TCRM, res_reg_NFA)

# Deseasonalize RBDI
ts_RBDI <- ts(base$RBDI, start = c(2004, 1), end = c(2019, 4), frequency = 4)
reg_RBDI <- dynlm(ts_RBDI ~ base$Quarter_1 + base$Quarter_2 + base$Quarter_3, start = c(2004, 1), end = c(2019, 4))
res_reg_RBDI <- reg_RBDI$residuals
windows(width = 10, height = 8)
autoplot(cbind(res_reg_TCRM, res_reg_RBDI), facets = TRUE)
windows(width = 10, height = 8)
qplot(res_reg_TCRM, res_reg_RBDI)

# Remove constants from already deseasonalized variables: GC_d and PIBBr_d

# For GC_d
ts_GC_d <- ts(base$GC_d, start = c(2004, 1), end = c(2019, 4), frequency = 4)
reg_GC_d <- dynlm(ts_GC_d ~ 1, start = c(2004, 1), end = c(2019, 4))
res_reg_GC_d <- reg_GC_d$residuals
windows(width = 10, height = 8)
autoplot(cbind(res_reg_TCRM, res_reg_GC_d), facets = TRUE)
windows(width = 10, height = 8)
qplot(res_reg_TCRM, res_reg_GC_d)

# For PIBBr_d
ts_PIBBr_d <- ts(base$PIBBr_d, start = c(2004, 1), end = c(2019, 4), frequency = 4)
reg_PIBBr_d <- dynlm(ts_PIBBr_d ~ 1, start = c(2004, 1), end = c(2019, 4))
res_reg_PIBBr_d <- reg_PIBBr_d$residuals
windows(width = 10, height = 8)
autoplot(cbind(res_reg_TCRM, res_reg_PIBBr_d), facets = TRUE)
windows(width = 10, height = 8)
qplot(res_reg_TCRM, res_reg_PIBBr_d)

# -----------------------------------------------------------------------------
# Punto 8.4
# -----------------------------------------------------------------------------

# Test for cointegration
reg_test_coint <- dynlm(res_reg_TCRM ~ res_reg_TOT + res_reg_GC_d + res_reg_PTNT + res_reg_NFA + res_reg_RBDI + res_reg_PIBBr_d + base$Cepo_1 + base$Cepo_2)
summary(reg_test_coint)
res_reg_test_coint <- reg_test_coint$residuals
residualPlot(reg_test_coint)

res_aic <- ur.df(res_reg_test_coint, type = "drift", selectlags = "AIC"); summary(res_aic)
res_bic <- ur.df(res_reg_test_coint, type = "drift", selectlags = "BIC"); summary(res_bic)
dfgls <- ur.ers(res_reg_test_coint, type = "DF-GLS", model = "constant", lag.max = 1)
summary(dfgls) # Value of test-statistic is: -4.0093 < -2.59 (1 pct), reject unit root, cointegration exists

# -----------------------------------------------------------------------------
# Punto 8.5
# -----------------------------------------------------------------------------

p <- 1
aic <- list()
for (i in 0:4) {
  for (i_1 in 0:4) {
    for (i_2 in 0:4) {
      for (i_3 in 0:4) {
        for (i_4 in 0:4) {
          for (i_5 in 0:4) {
            aic[p] <- list(dynlm(res_reg_TCRM ~ res_reg_TOT + res_reg_GC_d + res_reg_PTNT + res_reg_NFA + res_reg_RBDI + res_reg_PIBBr_d + base$Cepo_1 + base$Cepo_2 + L(diff(res_reg_TOT), -i:i) + L(diff(res_reg_GC_d), -i_1:i_1) + L(diff(res_reg_PTNT), -i_2:-i_2) + L(diff(res_reg_NFA), -i_3:i_3) + L(diff(res_reg_RBDI), -i_4:i_4) + L(diff(res_reg_PIBBr_d), -i_5:i_5)))
            if (p == 534) {
              print(i)
              print(i_1)
              print(i_2)
              print(i_3)
              print(i_4)
              print(i_5)
            }
            p <- p + 1
          }
        }
      }
    }
  }
}

# Results of information criterion
bet_mod_aic <- lapply(aic, AIC)
best_mod_aic <- str_remove(bet_mod_aic, "num")
best_mod_aic_2 <- as.numeric(best_mod_aic)
min_index <- which.min(best_mod_aic_2)
lags_optimos <- aic[[min_index]]
texreg(lags_optimos)

# Best DOLS Model
dols_aic <- dynlm(res_reg_TCRM ~ res_reg_TOT + res_reg_GC_d + res_reg_PTNT + res_reg_NFA + res_reg_RBDI + res_reg_PIBBr_d + base$Cepo_1 + base$Cepo_2 + L(diff(res_reg_TOT), -2:2) + L(diff(res_reg_GC_d), 0:0) + L(diff(res_reg_PTNT), -1:1) + L(diff(res_reg_NFA), -2:2) + L(diff(res_reg_RBDI), -0:0) + L(diff(res_reg_PIBBr_d), -2:2))
summary(dols_aic)

# -----------------------------------------------------------------------------
# Punto 8.6
# -----------------------------------------------------------------------------

# Cointegration relationship using the best model coefficients
const_y_est_TCRM <- reg_TCRM$coefficients[1] + reg_TCRM$coefficients[2] + reg_TCRM$coefficients[3] + reg_TCRM$coefficients[4]
TCRM_eq1 <- const_y_est_TCRM + 0.010541 + (-1.520459 * res_reg_TOT) + (0.363261 * res_reg_GC_d) + (1.498434 * res_reg_PTNT) + (-0.062407 * res_reg_NFA) + (-0.016137 * res_reg_RBDI) + (0.048144 * res_reg_PIBBr_d)

windows(width = 10, height = 8)
autoplot(cbind(ts_TCRM, TCRM_eq1), facets = FALSE)

# -----------------------------------------------------------------------------
# Punto 9
# -----------------------------------------------------------------------------

# Load new dataset with additional binary variables
rm(list = ls())
base <- read_excel("~/Downloads/base_tp5_2.xlsx")

# Deseasonalize variables: TCRM, TOT, PTNT, NFA, RBDI
base$date <- as.yearqtr(paste(base$Year, base$Quarter), "%Y %q")
base <- dummy_cols(base, select_columns = 'Quarter')
base$t <- as.numeric(rownames(base))

# Deseasonalize TCRM
ts_TCRM <- ts(base$TCRM, start = c(2004, 1), end = c(2021, 4), frequency = 4)
reg_TCRM <- dynlm(ts_TCRM ~ base$Quarter_1 + base$Quarter_2 + base$Quarter_3, start = c(2004, 1), end = c(2021, 4))
res_reg_TCRM <- reg_TCRM$residuals
windows(width = 10, height = 8)
autoplot(cbind(ts_TCRM, res_reg_TCRM), facets = TRUE)

# Deseasonalize TOT
ts_TOT <- ts(base$TOT, start = c(2004, 1), end = c(2021, 4), frequency = 4)
reg_TOT <- dynlm(ts_TOT ~ base$Quarter_1 + base$Quarter_2 + base$Quarter_3, start = c(2004, 1), end = c(2021, 4))
res_reg_TOT <- reg_TOT$residuals
windows(width = 10, height = 8)
autoplot(cbind(res_reg_TCRM, res_reg_TOT), facets = TRUE)
windows(width = 10, height = 8)
qplot(res_reg_TCRM, res_reg_TOT)

# Deseasonalize PTNT (includes drought dummies)
ts_PTNT <- ts(base$PTNT, start = c(2004, 1), end = c(2021, 4), frequency = 4)
reg_PTNT <- dynlm(ts_PTNT ~ base$Quarter_1 + base$Quarter_2 + base$Quarter_3 + base$Sequia_1 + base$Sequia_2, start = c(2004, 1), end = c(2021, 4))
res_reg_PTNT <- reg_PTNT$residuals
windows(width = 10, height = 8)
autoplot(cbind(res_reg_TCRM, res_reg_PTNT), facets = TRUE)
windows(width = 10, height = 8)
qplot(res_reg_TCRM, res_reg_PTNT)

# Deseasonalize NFA
ts_NFA <- ts(base$NFA, start = c(2004, 1), end = c(2021, 4), frequency = 4)
reg_NFA <- dynlm(ts_NFA ~ base$Quarter_1 + base$Quarter_2 + base$Quarter_3, start = c(2004, 1), end = c(2021, 4))
res_reg_NFA <- reg_NFA$residuals
windows(width = 10, height = 8)
autoplot(cbind(res_reg_TCRM, res_reg_NFA), facets = TRUE)
windows(width = 10, height = 8)
qplot(res_reg_TCRM, res_reg_NFA)

# Deseasonalize RBDI
ts_RBDI <- ts(base$RBDI, start = c(2004, 1), end = c(2021, 4), frequency = 4)
reg_RBDI <- dynlm(ts_RBDI ~ base$Quarter_1 + base$Quarter_2 + base$Quarter_3, start = c(2004, 1), end = c(2021, 4))
res_reg_RBDI <- reg_RBDI$residuals
windows(width = 10, height = 8)
autoplot(cbind(res_reg_TCRM, res_reg_RBDI), facets = TRUE)
windows(width = 10, height = 8)
qplot(res_reg_TCRM, res_reg_RBDI)

# Remove constants from already deseasonalized variables: GC_d and PIBBr_d

# For GC_d
ts_GC_d <- ts(base$GC_d, start = c(2004, 1), end = c(2021, 4), frequency = 4)
reg_GC_d <- dynlm(ts_GC_d ~ 1, start = c(2004, 1), end = c(2021, 4))
res_reg_GC_d <- reg_GC_d$residuals
windows(width = 10, height = 8)
autoplot(cbind(res_reg_TCRM, res_reg_GC_d), facets = TRUE)
windows(width = 10, height = 8)
qplot(res_reg_TCRM, res_reg_GC_d)

# For PIBBr_d
ts_PIBBr_d <- ts(base$PIBBr_d, start = c(2004, 1), end = c(2021, 4), frequency = 4)
reg_PIBBr_d <- dynlm(ts_PIBBr_d ~ 1, start = c(2004, 1), end = c(2021, 4))
res_reg_PIBBr_d <- reg_PIBBr_d$residuals
windows(width = 10, height = 8)
autoplot(cbind(res_reg_TCRM, res_reg_PIBBr_d), facets = TRUE)
windows(width = 10, height = 8)
qplot(res_reg_TCRM, res_reg_PIBBr_d)

# -----------------------------------------------------------------------------
# Punto 9.4
# -----------------------------------------------------------------------------

# Test for cointegration
reg_test_coint <- dynlm(res_reg_TCRM ~ res_reg_TOT + res_reg_GC_d + res_reg_PTNT + res_reg_NFA + res_reg_RBDI + res_reg_PIBBr_d + base$Cepo)
summary(reg_test_coint)
res_reg_test_coint <- reg_test_coint$residuals
residualPlot(reg_test_coint)

res_aic <- ur.df(res_reg_test_coint, type = "drift", selectlags = "AIC"); summary(res_aic)
res_bic <- ur.df(res_reg_test_coint, type = "drift", selectlags = "BIC"); summary(res_bic)
dfgls <- ur.ers(res_reg_test_coint, type = "DF-GLS", model = "constant", lag.max = 1)
summary(dfgls) # Value of test-statistic is: -3.1836 < -2.59 (1 pct), reject unit root, cointegration exists

# -----------------------------------------------------------------------------
# Punto 9.5
# -----------------------------------------------------------------------------

p <- 1
aic <- list()
for (i in 0:4) {
  for (i_1 in 0:4) {
    for (i_2 in 0:4) {
      for (i_3 in 0:4) {
        for (i_4 in 0:4) {
          for (i_5 in 0:4) {
            aic[p] <- list(dynlm(res_reg_TCRM ~ res_reg_TOT + res_reg_GC_d + res_reg_PTNT + res_reg_NFA + res_reg_RBDI + res_reg_PIBBr_d + base$Cepo + L(diff(res_reg_TOT), 0:0) + L(diff(res_reg_GC_d), 0:0) + L(diff(res_reg_PTNT), -1:-1) + L(diff(res_reg_NFA), -1:1) + L(diff(res_reg_RBDI), 0:0) + L(diff(res_reg_PIBBr_d), -1:1)))
            if (p == 152) {
              print(i)
              print(i_1)
              print(i_2)
              print(i_3)
              print(i_4)
              print(i_5)
            }
            p <- p + 1
          }
        }
      }
    }
  }
}

# Results of information criterion
bet_mod_aic <- lapply(aic, AIC)
best_mod_aic <- str_remove(bet_mod_aic, "num")
best_mod_aic_2 <- as.numeric(best_mod_aic)
min_index <- which.min(best_mod_aic_2)
lags_optimos <- aic[[min_index]]
texreg(lags_optimos)

# Best Model (corresponding to p = 152)
dols_aic <- dynlm(res_reg_TCRM ~ res_reg_TOT + res_reg_GC_d + res_reg_PTNT + res_reg_NFA + res_reg_RBDI + res_reg_PIBBr_d + base$Cepo + L(diff(res_reg_TOT), 0:0) + L(diff(res_reg_GC_d), 0:0) + L(diff(res_reg_PTNT), -1:-1) + L(diff(res_reg_NFA), -1:1) + L(diff(res_reg_RBDI), 0:0) + L(diff(res_reg_PIBBr_d), -1:1))
summary(dols_aic)

# -----------------------------------------------------------------------------
# Punto 9.6
# -----------------------------------------------------------------------------

# Cointegration relationship using the best model coefficients
const_y_est_TCRM <- reg_TCRM$coefficients[1] + reg_TCRM$coefficients[2] + reg_TCRM$coefficients[3] + reg_TCRM$coefficients[4]
TCRM_eq1 <- const_y_est_TCRM + 0.015503 + (-0.192853 * res_reg_TOT) + (-0.784958 * res_reg_GC_d) + (1.602751 * res_reg_PTNT) + (-0.093488 * res_reg_NFA) + (-0.009097 * res_reg_RBDI) + (-1.696051 * res_reg_PIBBr_d)

windows(width = 10, height = 8)
autoplot(cbind(ts_TCRM, TCRM_eq1), facets = FALSE)