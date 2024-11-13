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
library(ggplot2)     # For plotting
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
library(zoo)         # For managing dates
library(mFilter)
library(fastDummies)
library(tseries)
library(forecast)
library(Hmisc)

# -----------------------------------------------------------------------------
# Loading the data
# -----------------------------------------------------------------------------

# Load the dataset
df <- read.csv("~/Desktop/1 SEM 2022/Econometría avanzada/Ejercitaciones/Ejercitación 3/data_vix.csv")
View(df)

# Create log variables
df$log_pbi <- log(df$pbi)
df$log_cons <- log(df$cons)
df$log_inv <- log(df$inv)

# Apply HP filter
hp_freq <- 1600
lpbi_hp <- hpfilter(df$log_pbi, type = "lambda", freq = 1600)
lcons_hp <- hpfilter(df$log_cons, type = "lambda", freq = 1600)
linv_hp <- hpfilter(df$log_inv, type = "lambda", freq = 1600)
df$date <- as.yearqtr(paste(df$year, df$quarter), "%Y %q")

# Extracting cyclical components using HP filter
out <- xts(cbind(lpbi_hp$x, lpbi_hp$trend, lpbi_hp$cycle), df$date) 
colnames(out) <- c("log_pbi", "trend", "cycle")

out2 <- xts(cbind(lcons_hp$x, lcons_hp$trend, lcons_hp$cycle), df$date) 
colnames(out2) <- c("log_cons", "trend", "cycle")

out3 <- xts(cbind(linv_hp$x, linv_hp$trend, linv_hp$cycle), df$date) 
colnames(out3) <- c("log_inv", "trend", "cycle")

# Plot cyclical components and VIX index
windows(width = 10, height = 8)
autoplot(vix) + ggtitle("VIX Index") + xlab("Year") + ylab("VIX Value") + theme_minimal()

componente_ciclico_pbi <- out[,"cycle"]
componente_ciclico_consumo <- out2[,"cycle"]
componente_ciclico_inv <- out3[,"cycle"]

# Create plots for cyclical components
windows(width = 10, height = 8)
autoplot(componente_ciclico_pbi) + ggtitle("Cyclical Component of PBI") + theme_minimal()

windows(width = 10, height = 8)
autoplot(componente_ciclico_consumo) + ggtitle("Cyclical Component of Consumption") + theme_minimal()

windows(width = 10, height = 8)
autoplot(componente_ciclico_inv) + ggtitle("Cyclical Component of Investment") + theme_minimal()

# Plot original and trend components
par(mfrow = c(2, 1), mar = c(3, 2, 2, 1))
plot(out[,"log_pbi"], main = "Log PBI with Trend", col = "steelblue")
lines(out[,"trend"], col = "red")
legend("topright", legend = c("Log PBI", "Trend"), col = c("steelblue", "red"), lty = 1)

plot(out[,"cycle"], main = "Cyclical Component of PBI", col = "steelblue")

par(mfrow = c(2, 1), mar = c(3, 2, 2, 1))
plot(out2[,"log_cons"], main = "Log Consumption with Trend", col = "steelblue")
lines(out2[,"trend"], col = "red")
legend("topright", legend = c("Log Consumption", "Trend"), col = c("steelblue", "red"), lty = 1)

plot(out2[,"cycle"], main = "Cyclical Component of Consumption", col = "steelblue")

par(mfrow = c(2, 1), mar = c(3, 2, 2, 1))
plot(out3[,"log_inv"], main = "Log Investment with Trend", col = "steelblue")
lines(out3[,"trend"], col = "red")
legend("topright", legend = c("Log Investment", "Trend"), col = c("steelblue", "red"), lty = 1)

plot(out3[,"cycle"], main = "Cyclical Component of Investment", col = "steelblue")

# -----------------------------------------------------------------------------
# Vector Autoregression (VAR)
# -----------------------------------------------------------------------------
library(vars)

# Create time series
vix <- ts(df$vix, start = c(1990, 1), frequency = 4)
pbi <- ts(lpbi_hp$cycle, start = c(1990, 1), frequency = 4)
cons <- ts(lcons_hp$cycle, start = c(1990, 1), frequency = 4)
inv <- ts(linv_hp$cycle, start = c(1990, 1), frequency = 4)

df2 <- cbind(vix, pbi, cons, inv)

# Select optimal lag length
lagselect <- VARselect(df2, lag.max = 8)
lagselect$selection

# Estimate VAR model
var1 <- VAR(df2, p = 8, type = "const")
summary(var1)

# Impulse Response Function (IRF)
irf1 <- irf(var1, impulse = "vix", response = c("pbi", "cons", "inv"), n.ahead = 10, boot = TRUE, ortho = TRUE)
plot(irf1, main = "Impulse Response to VIX Shock")

# Forecast Error Variance Decomposition (FEVD)
fevd1 <- fevd(var1, n.ahead = 20)
plot(fevd1, main = "Forecast Error Variance Decomposition")

# -----------------------------------------------------------------------------
# Further Analysis using HP Filtered VIX
# -----------------------------------------------------------------------------

# Apply HP filter to VIX
vix_hp <- hpfilter(df$vix, type = "lambda", freq = 1600)
df$vix_hp <- vix_hp$cycle

# Create dummy variable
threshold <- sd(vix_hp$cycle)
df$vix_dumb <- ifelse(vix_hp$cycle > threshold | vix_hp$cycle < -threshold, 1, 0)
df$vix2 <- df$vix * df$vix_dumb

# Create new time series for VAR analysis
vix_2 <- ts(df$vix2, start = c(1990, 1), frequency = 4)
df3 <- cbind(vix_2, pbi, cons, inv)

# Select optimal lag length for new VAR model
lagselect <- VARselect(df3, lag.max = 8)
lagselect$selection

# Estimate new VAR model
var2 <- VAR(df3, p = 2, type = "const")
summary(var2)

# Impulse Response Function for new VAR model
irf2 <- irf(var2, impulse = "vix_2", response = c("pbi", "cons", "inv"), n.ahead = 10, boot = TRUE, ortho = TRUE)
plot(irf2, main = "Impulse Response to Filtered VIX Shock")

# Re-plot original IRF for comparison
irf1 <- irf(var1, impulse = "vix", response = c("pbi", "cons", "inv"), n.ahead = 20, boot = TRUE, ortho = TRUE)
plot(irf1, main = "Impulse Response to Original VIX Shock")