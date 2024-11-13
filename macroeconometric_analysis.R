# Librarías
library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(lubridate)
library(readxl)
library(tidyverse)
library(seasonal)

# Armado de la base de datos
setwd("~/Desktop/UdeSA/Optativas/3T/Macroeconometria/Ejercitaciones")
datos <- read_excel("Complete_Database_new.xlsx")

data <- datos

desestacionalizar <- function(data_column, start_date, end_date, log_transform = TRUE){
  # Crear objeto time series
  datos_xts <- ts(data_column, start = start_date, end = end_date, frequency = 12)
  
  # Desestacionalizar
  seas.adj <- seas(datos_xts)
  seas_df <- as.data.frame(seas.adj)
  final_data <- seas_df$final
  
  # Transformación logarítmica
  if (log_transform) {
    return(log(final_data) * 100)
  } else {
    return(final_data)
  }
}

data$IMACEC_sa <- desestacionalizar(data$IMACEC_SA, c(2000, 1), c(2023, 5))
data$IGAE_sa <- desestacionalizar(data$IGAE, c(2000, 1), c(2023, 5))
data$IPC_USA_sa <- desestacionalizar(data$IPC_USA, c(2000, 1), c(2023, 5), log_transform = FALSE)

# Adding a constant to shift all data points into the positive space
data$IPC_SAE_CHILE <- data$IPC_SAE_CHILE + abs(min(data$IPC_SAE_CHILE, na.rm = TRUE)) + 1

# Deseasonalize IPC_SAE_CHILE
data$IPC_SAE_CHILE_sa <- desestacionalizar(data$IPC_SAE_CHILE, c(2000, 1), c(2023, 5))

# Assuming 'your_data' is a numeric vector of your IPC data
ipc_ts <- ts(data, start = c(2000, 1), frequency = 12)  # for monthly data starting from January 2000

# Extract components
seasonal_component <- stl_results$time.series[,"seasonal"]
trend_component <- stl_results$time.series[,"trend"]
remainder_component <- stl_results$time.series[,"remainder"]

# Calculate the seasonally adjusted data by subtracting the seasonal component from the original data
seasonally_adjusted <- trend_component + remainder_component

# Add the seasonally adjusted data back to your data frame
data$IPC_SAE_CHILE_adjusted <- seasonally_adjusted

# If you had previously shifted your data by adding a constant,
# and you want to reverse that shift after deseasonalizing:
if (exists("shift_constant")) {
  data$IPC_SAE_CHILE_adjusted <- data$IPC_SAE_CHILE_adjusted - shift_constant
}

# Ejercicio 2: Gráficos
datos <- data
datos$Fecha <- as.Date(datos$'...1', format = "%Y-%m-%d")
datos <- datos %>% arrange(Fecha)

# Ejemplo: Gráfico para IMACEC_SA y P_INT_REAL_PET
ggplot(datos) + 
  geom_line(aes(x = Fecha, y = IMACEC_SA, color = "IMACEC_SA")) +
  geom_line(aes(x = Fecha, y = P_INT_REAL, color = "P_INT_REAL_PET")) +
  scale_y_continuous(sec.axis = sec_axis(~ ., name = "Precio Internacional Real del Petróleo")) +
  labs(title = "Evolución de IMACEC_SA y Precio Internacional Real del Petróleo",
       x = "Fecha",
       y = "Índice") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_color_manual(values = c("IMACEC_SA" = "blue", "P_INT_REAL_PET" = "red"))

# Graficar múltiples series
library(gridExtra)
library(ggplot2)

graficar_serie <- function(serie_nombre) {
  max_serie <- max(datos[[serie_nombre]], na.rm = TRUE)
  max_price <- max(datos$P_INT_REAL, na.rm = TRUE)
  coeff <- max_price / max_serie
  
  ggplot(datos) + 
    geom_line(aes(x = Fecha, y = get(serie_nombre)), color = "navy") +
    geom_line(aes(x = Fecha, y = P_INT_REAL / coeff), color = "maroon") +
    scale_y_continuous(
      name = serie_nombre,
      sec.axis = sec_axis(~ . * coeff, name = "Precio del Petróleo")
    ) +
    labs(title = paste("Evolución de", serie_nombre),
         x = "Año") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
}

lista_de_series <- c("IMACEC_SA", "IGAE", "IPC_SAE_CHILE", "INPC_MEX", "TC_MEXICO", "TASA_CHILE", "TASA_MEXICO", "EMBI_CHILE", "EMBI_MEXICO", "TC_CHILE")

# Store the plots in a list
lista_de_graficos <- lapply(lista_de_series, graficar_serie)

# Display in a grid
grid.arrange(grobs = lista_de_graficos, ncol = 3)

# Ejercicio 3
# Armado de ts
IMACEC <- ts(data$IMACEC_SA, start = c(2000, 1), end = c(2023, 9), frequency = 12)
IGAE <- ts(data$IGAE, start = c(2000, 1), end = c(2023, 9), frequency = 12)
IPC_SAE_CHILE <- ts(data$IPC_SAE_CHILE, start = c(2000, 1), end = c(2023, 9), frequency = 12)
INPC_MEX <- ts(data$INPC_MEX, start = c(2000, 1), end = c(2023, 9), frequency = 12)
TC_MEXICO <- ts(data$TC_MEXICO, start = c(2000, 1), end = c(2023, 9), frequency = 12)
TASA_CHILE <- ts(data$TASA_CHILE, start = c(2000, 1), end = c(2023, 9), frequency = 12)
TASA_MEXICO <- ts(data$TASA_MEXICO, start = c(2000, 1), end = c(2023, 9), frequency = 12)
EMBI_CHILE <- ts(data$EMBI_CHILE, start = c(2000, 1), end = c(2023, 9), frequency = 12)
EMBI_MEXICO <- ts(data$EMBI_MEXICO, start = c(2000, 1), end = c(2023, 9), frequency = 12)
P_INT_REAL <- ts(data$P_INT_REAL, start = c(2000, 1), end = c(2023, 9), frequency = 12)
TC_CHILE <- ts(data$TC_CHILE, start = c(2000, 1), end = c(2023, 9), frequency = 12)

# Armar una "matriz" con todas las variables
Yl <- cbind(IMACEC, IGAE, IPC_SAE_CHILE, INPC_MEX, TC_MEXICO, TASA_CHILE, TASA_MEXICO, EMBI_CHILE, EMBI_MEXICO, TC_CHILE, P_INT_REAL)

# Eliminar las observaciones que contienen NA
Yl <- na.omit(Yl)

# Test de unit roots
library(vars)

# Analizar la estacionariedad de las variables (Test de DF)
unit_root_tests <- list()

# Realizar la prueba de raíz unitaria para cada variable en Yl
for (i in 1:ncol(Yl)) {
  var_name <- colnames(Yl)[i]
  df <- ur.df(Yl[, i], type = "trend", lags = 1, selectlags = "Fixed")
  unit_root_tests[[var_name]] <- summary(df)
}

# Ver los resultados de las pruebas de raíz unitaria
for (var_name in names(unit_root_tests)) {
  cat("Variable:", var_name, "\n")
  print(unit_root_tests[[var_name]])
  cat("\n")
}

# Create a data frame to store test statistics and p-values
library(knitr)
library(kableExtra)

# Create a data frame to store variables, decision on H0, test statistics, and critical value at 5%
results_df <- data.frame(
  Variable = character(),
  Rejection = character(),
  TestStatistic = numeric(),
  CriticalValue5pct = numeric(),
  stringsAsFactors = FALSE
)

# Extract the necessary information from each test result
for (var_name in names(unit_root_tests)) {
  test_result <- unit_root_tests[[var_name]]
  
  test_statistic <- test_result@teststat[1]  # Extracting the test statistic
  critical_value_5pct <- test_result@cval[1, "5pct"]  # Extracting the critical value at 5%
  
  rejection_decision <- ifelse(test_statistic < critical_value_5pct, "SI", "NO")
  
  results_df <- rbind(
    results_df, 
    data.frame(
      Variable = var_name, 
      Rejection = rejection_decision,
      TestStatistic = test_statistic, 
      CriticalValue5pct = critical_value_5pct,
      stringsAsFactors = FALSE
    )
  )
}

# Create a table using knitr and kableExtra, formatted for LaTeX
results_table <- results_df %>%
  kable("html") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

# View the table
print(results_table)

# Crear un vector con las variables originales
variables_no_est <- c("IMACEC", "INPC_MEX", "TC_MEXICO", "TASA_CHILE", "TASA_MEXICO", "P_INT_REAL", "TC_CHILE")

# Calcular las diferencias para obtener series estacionarias
for (variable in variables_no_est) {
  variable_d <- paste(variable, "_d", sep = "")
  assign(variable_d, diff(get(variable)))
}

# Crear una "matriz" para cada país
Yl_MEX <- cbind(IGAE, INPC_MEX_d, TC_MEXICO_d, TASA_MEXICO_d, EMBI_MEXICO, P_INT_REAL)
Yl_CHI <- cbind(IMACEC, IPC_SAE_CHILE, TASA_CHILE, EMBI_CHILE, P_INT_REAL, TC_CHILE)

# VAR para México
Yl_MEX <- na.omit(Yl_MEX)

# Selección del orden de rezagos para México
pmax <- 10
p_MEX <- VARselect(Yl_MEX, lag.max = pmax, type = "const")
p_m <- p_MEX$selection[2]

# Estimación del VAR para México
VAR_MEX <- VAR(Yl_MEX, p = p_m, type = "const")
summary(VAR_MEX)

# IRF para México
irf_mex <- irf(VAR_MEX, n.ahead = 10)
plot(irf_mex)

# VAR para Chile
Yl_CHI <- na.omit(Yl_CHI)

# Selección del orden de rezagos para Chile
p_CHI <- VARselect(Yl_CHI, lag.max = pmax, type = "const")
p_ch <- p_CHI$selection[2]

# Estimación del VAR para Chile
VAR_CHI <- VAR(Yl_CHI, p = p_ch, type = "const")
summary(VAR_CHI)

# IRF para Chile
irf_chi <- irf(VAR_CHI, n.ahead = 10)
plot(irf_chi)

# Tests de Causalidad de Granger

# Granger Causality para México
VAR.GC.test.asym_mex <- causality(VAR_MEX, cause = "P_INT_REAL")
VAR.GC.test.asym_mex
VAR.GC.test.boot_mex <- causality(VAR_MEX, cause = "P_INT_REAL", boot = TRUE, boot.runs = 2000)
VAR.GC.test.boot_mex

# Granger Causality para Chile
VAR.GC.test.asym_chi <- causality(VAR_CHI, cause = "P_INT_REAL")
VAR.GC.test.asym_chi
VAR.GC.test.boot_chi <- causality(VAR_CHI, cause = "P_INT_REAL", boot = TRUE, boot.runs = 2000)
VAR.GC.test.boot_chi

# Otros Test de Diagnóstico

# VAR stability
VAR.roots_MEX <- roots(VAR_MEX, modulus = TRUE)
VAR.roots_MEX
VAR.roots_CHI <- roots(VAR_CHI, modulus = TRUE)
VAR.roots_CHI

# Residual Serial Correlation
T_MEX <- VAR_MEX$obs
T_CHI <- VAR_CHI$obs
h.PT_MEX <- min(10, trunc(T_MEX / 5))
h.PT_CHI <- min(10, trunc(T_CHI / 5))

# Portmanteau Test
VAR.PT.test.serial_MEX <- serial.test(VAR_MEX, lags.pt = h.PT_MEX, type = "PT.asymptotic")
VAR.PT.test.serial_MEX
VAR.PT.test.serial_CHI <- serial.test(VAR_CHI, lags.pt = h.PT_CHI, type = "PT.asymptotic")
VAR.PT.test.serial_CHI

# Portmanteau Test (adjusted)
VAR.PT.test.serial.adj_MEX <- serial.test(VAR_MEX, lags.pt = h.PT_MEX, type = "PT.adjusted")
VAR.PT.test.serial.adj_MEX
VAR.PT.test.serial.adj_CHI <- serial.test(VAR_CHI, lags.pt = h.PT_CHI, type = "PT.adjusted")
VAR.PT.test.serial.adj_CHI

# Breusch-Godfrey Test
h.BG <- 6
VAR.BG.test.serial_MEX <- serial.test(VAR_MEX, lags.bg = h.BG, type = "BG")
VAR.BG.test.serial_MEX
VAR.BG.test.serial_CHI <- serial.test(VAR_CHI, lags.bg = h.BG, type = "BG")
VAR.BG.test.serial_CHI

# Breusch-Godfrey Test (adjusted)
VAR.BG.test.serial.adj_MEX <- serial.test(VAR_MEX, lags.bg = h.BG, type = "ES")
VAR.BG.test.serial.adj_MEX
VAR.BG.test.serial.adj_CHI <- serial.test(VAR_CHI, lags.bg = h.BG, type = "ES")
VAR.BG.test.serial.adj_CHI

# Residual Normality
VAR.JB.test_MEX <- normality.test(VAR_MEX, multivariate.only = FALSE)
VAR.JB.test_CHI <- normality.test(VAR_CHI, multivariate.only = FALSE)
VAR.JB.test_MEX
VAR.JB.test_CHI

# Residual Heteroskedasticity
VAR.ARCH.test_MEX <- arch.test(VAR_MEX, lags.multi = 12, multivariate.only = FALSE)
VAR.ARCH.test_MEX
VAR.ARCH.test_CHI <- arch.test(VAR_CHI, lags.multi = 12, multivariate.only = FALSE)
VAR.ARCH.test_CHI

# Stability Analysis (Structural Change)
stab.test_MEX <- stability(VAR_MEX, type = "fluctuation")
plot(stab.test_MEX)
stab.test_CHI <- stability(VAR_CHI, type = "fluctuation")
plot(stab.test_CHI)

# Formal Test
VAR_mex <- c("IGAE", "INPC_MEX_d", "TC_MEXICO_d", "TASA_MEXICO_d", "EMBI_MEXICO", "P_INT_REAL")
stability_results_mex <- sapply(VAR_mex, function(var) sctest(stab.test_MEX$stability[[var]]))

var_chi <- c("IMACEC", "IPC_SAE_CHILE", "TASA_CHILE", "EMBI_CHILE", "P_INT_REAL", "TC_CHILE")
stability_results_chi <- sapply(var_chi, function(var) sctest(stab.test_CHI$stability[[var]]))

# Table of results
library(knitr)

# Define a function to extract statistics from test objects
extract_stats <- function(test) {
  if("causality" %in% class(test)) {
    list(Statistic = test$Granger$statistic, P_Value = test$Granger$p.value)
  } else if("serial.test" %in% class(test)) {
    list(Statistic = test$serial$statistic, P_Value = test$serial$p.value)
  } else if("normality.test" %in% class(test)) {
    list(Statistic = test$jb.mul$JB, P_Value = test$jb.mul$p.value)
  } else if("arch.test" %in% class(test)) {
    list(Statistic = test$arch.mul$LM, P_Value = test$arch.mul$p.value)
  } else {
    list(Statistic = NA, P_Value = NA)
  }
}

# Extract statistics from all test objects
test_objects <- list(
  GC_Asym_MEX = VAR.GC.test.asym_mex$Granger,
  GC_Boot_MEX = VAR.GC.test.boot$Granger,
  GC_Asym_CHI = VAR.GC.test.asym_chi$Granger,
  GC_Boot_CHI = VAR.GC.test.boot_chi$Granger,
  PT_MEX = VAR.PT.test.serial_MEX$serial,
  PT_CHI = VAR.PT.test.serial_CHI$serial,
  PT_Adj_MEX = VAR.PT.test.serial.adj_MEX$serial,
  PT_Adj_CHI = VAR.PT.test.serial.adj_CHI$serial,
  BG_MEX = VAR.BG.test.serial_MEX$serial,
  BG_CHI = VAR.BG.test.serial_CHI$serial,
  BG_Adj_MEX = VAR.BG.test.serial.adj_M
  EX$serial,
  BG_Adj_CHI = VAR.BG.test.serial.adj_CHI$serial,
  JB_MEX = VAR.JB.test_MEX$jb.uni,
  JB_CHI = VAR.JB.test_CHI$jb.uni,
  ARCH_MEX = VAR.ARCH.test_MEX$arch.uni,
  ARCH_CHI = VAR.ARCH.test_CHI$arch.uni
)

# Helper function to extract p-values and results (like F-statistic, Chi-squared etc.)
extract_values <- function(obj) {
  if (is.null(obj$p.value)) {
    return(NULL)
  }
  # Extracting the value before the "p-value"
  result <- sub(".*=\\s*", "", unlist(strsplit(as.character(obj), ",[[:space:]]"))[1])
  return(data.frame(result = result, p.value = obj$p.value))
}

# Applying the helper function to each object in test_objects
results_list <- lapply(test_objects, extract_values)

# Convert the list into a data frame
results_df <- do.call(rbind, results_list)

# Add the names of the tests as a new column
results_df$test_name <- rownames(results_df)

# Reorder columns for better presentation
results_df <- results_df[, c("test_name", "result", "p.value")]

# Display the final data frame
print(results_df)

# Add significance column
results_df$Significance <- ifelse(results_df$p.value < 0.05, "Yes", "No")

# Create table with LaTeX formatting
kable(results_df, format = "latex", caption = "Results with Significance", digits = 3, align = c('l', 'r', 'r', 'c'))

#-------------------------------------------------------------------------------
# SVAR MEXICO
#-------------------------------------------------------------------------------

Yl_MEX <- cbind(P_INT_REAL, IGAE_d, INPC_MEX_d, TC_MEXICO, TASA_MEXICO, EMBI_MEXICO)

# Omit NA values
Yl_MEX <- na.omit(Yl_MEX)

# Lag Selection
pmax <- 10
p <- VARselect(Yl_MEX, lag.max = pmax, type = "const")
p <- p$selection[2]

VAR_MEX <- VAR(Yl_MEX, p = p, type = "const")

# Constraints
matC <- function(m, p, vx) {
  vy <- setdiff(1:m, vx)
  Cm <- matrix(1, m, m * p + 1)
  for (i in vx) {
    for (l in 1:p) {
      for (j in vy) {
        Cm[i, m * (l - 1) + j] <- 0
      }
    }
  }
  Cm
}

m <- VAR_MEX$K
constraints <- matC(m, p, 1)
VAR_MEX <- restrict(VAR_MEX, method = "man", resmat = constraints)

# Model Checking
roots(VAR_MEX, modulus = TRUE)
serial.test(VAR_MEX, lags.bg = 6, type = "ES")

# SVAR Estimation (AB model configuration)
Amat <- function(m){
  Amat <- diag(m)
  for (i in 2:m) {
    for (j in 1:(i - 1)) {
      Amat[i, j] <- NA
    }
  }
  Amat
}

Bmat <- function(m){
  Bmat <- matrix(0, m, m)
  for (i in 1:m) {
    Bmat[i, i] <- NA
  }
  Bmat
}

SVAR <- SVAR(VAR_MEX, Amat = Amat(m), Bmat = Bmat(m), lrtest = FALSE, max.iter = 1000)

# Structural IRF
structural_irf <- function(SVAR_model) {
  responses <- c("IGAE_d", "INPC_MEX_d", "TC_MEXICO", "TASA_MEXICO", "EMBI_MEXICO")
  irf_list <- lapply(responses, function(response) {
    irf(SVAR_model, response = response, impulse = "P_INT_REAL", n.ahead = 20, ortho = TRUE, boot = TRUE)
  })
  par(mfrow = c(3, 2))  # Adjust according to the number of plots
  lapply(irf_list, plot, main = "IRF", lwd = 3)
  par(mfrow = c(1, 1))
}

structural_irf(SVAR)

# Structural Cumulative IRF
cumulative_irf <- function(SVAR_model) {
  responses <- c("IGAE_d", "INPC_MEX_d", "TC_MEXICO", "TASA_MEXICO", "EMBI_MEXICO")
  irf_list <- lapply(responses, function(response) {
    irf(SVAR_model, response = response, impulse = "P_INT_REAL", n.ahead = 20, ortho = TRUE, boot = TRUE, cumulative = TRUE)
  })
  par(mfrow = c(3, 2))  # Adjust according to the number of plots
  lapply(irf_list, plot, main = "Cumulative IRF", lwd = 3)
  par(mfrow = c(1, 1))
}

cumulative_irf(SVAR)

# Forecast Error Variance Decomposition
SVARfevd <- fevd(SVAR, n.ahead = 20)
colors <- brewer.pal(6, "Set1")
plot(SVARfevd, col = colors)

# Historical Decomposition
HD <- SVAR.hd(SVAR)
plot.hd(Yl_MEX, HD, m)

#-------------------------------------------------------------------------------
# SVAR CHILE
#-------------------------------------------------------------------------------

data$IMPORTED_PRICE_REAL <- data$IMPORTED_PRICE_REAL * 100
data$WTI_SPOT_REAL <- data$WTI_SPOT_REAL * 100

Y_1 <- cbind(log_oil_production_diff, igrea, price)

Y_1 <- na.omit(Y_1)

p <- 24
VAR_1 <- VAR(Y_1, p = p, type = "const")
m <- VAR_1$K

b <- diag(NA, 3)
b[lower.tri(b, diag = TRUE)] <- NA

SVAR_1 <- SVAR(VAR_1, Bmat = b, lrtest = FALSE, max.iter = 1000)

# Structural IRF
structural_irf2 <- function(SVAR_model) {
  responses <- c("log_oil_production_diff", "igrea", "price")
  plots <- list()
  for (i in 1:length(responses)) {
    for (j in 1:length(responses)) {
      irf_ij <- irf(SVAR_model, response = responses[i], impulse = responses[j], n.ahead = 20, ortho = TRUE, boot = TRUE)
      plots[[length(plots) + 1]] <- irf_ij
    }
  }
  par(mfrow = c(3, 3))  # Adjust according to the number of plots
  lapply(plots, plot, main = "IRF", lwd = 3)
  par(mfrow = c(1, 1))
}

structural_irf2(SVAR_1)

#-------------------------------------------------------------------------------
# SVAR from 2000 (Kilian 2009)
#-------------------------------------------------------------------------------

data <- read_excel("Ejercicio_6.xlsx", sheet = 1)

Y_3 <- cbind(log_oil_production_diff, igrea, price)

Y_3 <- na.omit(Y_3)

p <- 24
VAR_3 <- VAR(Y_3, p = p, type = "const")
m <- VAR_3$K

b_3 <- diag(NA, 3)
b_3[lower.tri(b_3, diag = TRUE)] <- NA

SVAR_3 <- SVAR(VAR_3, Bmat = b_3, lrtest = FALSE, max.iter = 1000)

# Structural IRF for 2000 period
structural_irf3 <- function(SVAR_model) {
  responses <- c("log_oil_production_diff", "igrea", "price")
  plots <- list()
  for (i in 1:length(responses)) {
    for (j in 1:length(responses)) {
      irf_ij <- irf(SVAR_model, response = responses[i], impulse = responses[j], n.ahead = 20, ortho = TRUE, boot = TRUE)
      plots[[length(plots) + 1]] <- irf_ij
    }
  }
  par(mfrow = c(3, 3))  # Adjust according to the number of plots
  lapply(plots, plot, main = "IRF", lwd = 3)
  par(mfrow = c(1, 1))
}

structural_irf3(SVAR_3)

#-------------------------------------------------------------------------------
# Rectangle layout Structural IRF
layout_vector <- matrix(1:9, 3, 3, byrow = TRUE)
layout(matrix = layout_vector)

# Set plot sizes to fill more of the plot window
par(mar = c(4, 4, 2, 1), oma = c(1, 1, 1, 1))

# Plot each IRF
plot(irf_0_3, main = "Shock de Oferta", lwd = 3, ylab = "Diferencia Logaritmica de la producción")
plot(irf_1_3, main = "Shock de Oferta", lwd = 3, ylab = "Indice de actividad económica global")
plot(irf_2_3, main = "Shock de Oferta", lwd = 3, ylab = "Precio Real del petroleo importado por USA")
plot(irf_3_3, main = "Shock de demanda agregada", lwd = 3, ylab = "Diferencia Logaritmica de la producción")
plot(irf_4_3, main = "Shock de demanda agregada", lwd = 3, ylab = "Indice de actividad económica global")
plot(irf_5_3, main = "Shock de demanda agregada", lwd = 3, ylab = "Precio Real del petroleo importado por USA")
plot(irf_6_3, main = "Shock de demanda precautoria", lwd = 3, ylab = "Diferencia Logaritmica de la producción")
plot(irf_7_3, main = "Shock de demanda precautoria", lwd = 3, ylab = "Indice de actividad económica global")
plot(irf_8_3, main = "Shock de demanda precautoria", lwd = 3, ylab = "Precio Real del petroleo importado por USA")

#-------------------------------------------------------------------------------
# SVAR with P_INT_REAL (post-2000)
#-------------------------------------------------------------------------------

Y_4 <- cbind(log_oil_production_diff, igrea, P_INT_REAL)

Y_4 <- na.omit(Y_4)

p <- 24
VAR_4 <- VAR(Y_4, p = p, type = "const")
m <- VAR_4$K

b_4 <- diag(NA, 3)
b_4[lower.tri(b_4, diag = TRUE)] <- NA

SVAR_4 <- SVAR(VAR_4, Bmat = b_4, lrtest = FALSE, max.iter = 1000)

# Structural IRF for P_INT_REAL (post-2000)
structural_irf4 <- function(SVAR_model) {
  responses <- c("log_oil_production_diff", "igrea", "P_INT_REAL")
  plots <- list()
  for (i in 1:length(responses)) {
    for (j in 1:length(responses)) {
      irf_ij <- irf(SVAR_model, response = responses[i], impulse = responses[j], n.ahead = 20, ortho = TRUE, boot = TRUE)
      plots[[length(plots) + 1]] <- irf_ij
    }
  }
  par(mfrow = c(3, 3))  # Adjust according to the number of plots
  lapply(plots, plot, main = "IRF", lwd = 3)
  par(mfrow = c(1, 1))
}

structural_irf4(SVAR_4)

#-------------------------------------------------------------------------------
# Forecast Error Variance Decomposition (Extended)
#-------------------------------------------------------------------------------

SVARfevd_extended <- fevd(SVAR_4, n.ahead = 20)
colors <- brewer.pal(6, "Set1")
plot(SVARfevd_extended, col = colors)

HD <- SVAR.hd(SVAR_4)
plot.hd(Yl_MEX, HD, m)