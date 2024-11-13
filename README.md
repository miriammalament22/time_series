# Macroeconometric Analysis in R

## Overview

This repository is a collection of R scripts designed for various macroeconometric analyses, focusing on time series data. It includes scripts for vector autoregressions (VAR), unit root tests, deseasonalization, cointegration analysis, exchange rate pass-through, and VIX analysis. The aim is to provide a comprehensive toolkit for those interested in applying macroeconometric techniques to their datasets.

## Structure of the Repository

The repository contains the following files:

1. **VAR_and_unit_root_tests.R**: This script includes functions and procedures for conducting VAR models and unit root tests on time series data.
2. **code_univariate_analysis.R**: A script focused on univariate time series analysis, including tests for stationarity and trend detection.
3. **deseasonalization_cointegration_analysis.R**: Contains methods for deseasonalization and cointegration analysis, enabling the examination of long-run relationships in economic data.
4. **exchange_rate_pass_through.R**: Analyzes the pass-through effect of exchange rate changes on prices using time series econometric techniques.
5. **macroeconometric_analysis.R**: A comprehensive script for various macroeconometric analyses, including deseasonalization, cointegration, SVAR, and diagnostic tests.
6. **vix_analysis.R**: Involves the analysis of the VIX index and its impact on various economic indicators.

## Getting Started

### Prerequisites

To run the scripts in this repository, you will need the following R packages:

- `httr`
- `jsonlite`
- `dplyr`
- `tidyr`
- `lubridate`
- `readxl`
- `tidyverse`
- `seasonal`
- `vars`
- `tseries`
- `lmtest`
- `sandwich`
- `forecast`
- `kableExtra`
- `ggplot2`
- `gridExtra`
- `Brewer`
### Running the Scripts

Each script is designed to be run independently. Here's how to use each script:

#### VAR_and_unit_root_tests.R

This script performs VAR models and unit root tests. It is suitable for analyzing the stationarity and structural relationships of time series data.

#### code_univariate_analysis.R

This script provides tools for inspecting individual time series data properties, focusing on univariate analysis.

#### deseasonalization_cointegration_analysis.R

Useful for deseasonalizing time series data and analyzing cointegration relationships, helping understand long-run equilibria in economic data.

#### exchange_rate_pass_through.R

Examines the effect of exchange rate changes on prices, ideal for studies related to inflation and exchange rate dynamics.

#### macroeconometric_analysis.R

Combines multiple macroeconometric techniques and tests for a thorough econometric investigation.

#### vix_analysis.R

Includes analysis of the VIX index and its impact on economic indicators, particularly useful for understanding market volatility and its economic implications.

### Data

Ensure that the necessary datasets are available in the specified directories for each script to run successfully. The scripts assume data files are in the same directory unless specified otherwise. I will be uploading the data as I get permission to in the future.

### Contribution

Contributions to this repository are welcome. If you have any improvements or additional features, feel free to fork the repository and create a pull request.
