library(tidyquant)
library(tidyr)
library(tidyverse)
library(ggplot2)
library(dplyr)

RESILIO_PATH <- 'C:\\Users\\damia\\ResilioSync'

source(paste0(
  RESILIO_PATH,
  '\\FXBootcamp\\Code\\tools\\r-tools\\data-utils.R'
))

source(paste0(
  RESILIO_PATH,
  '\\FXBootcamp\\Code\\tools\\r-tools\\factor-utils.R'
))

DATA_FOLDER <- paste0(RESILIO_PATH, '\\FXBootcamp')

asset_list <- get_asset_list('AssetsDWX-FX-USD')
head(asset_list)

prices_df <- get_daily_OHLC(asset_list$Name)

# drop all but EM currencies
to_keep <- c("USDMXN", "USDSGD", "USDTRY")
prices_df <- prices_df %>%
  filter(Ticker %in% to_keep) %>%
  filter(Date > '2010-01-01', Date < '2019-01-01')

# convert to common quote currency
prices_df <- convert_common_quote_currency(prices_df, "USD")
head(prices_df)

# run momo factor calculations
momo_formation_period <- 250
factor_df <- calc_momo_factor(prices_df, momo_formation_period)

head(factor_df)

n_quantiles <- 3
quantile_df <- get_factor_quantiles(factor_df, n_quantiles)
head(quantile_df)

forward_periods = c(10, 20, 60)
analysis_df <-
  append_forward_returns(
    quantile_df,
    prices_df,
    total_return = FALSE,
    demean_returns = TRUE,
    forward_periods = forward_periods
  )
head(analysis_df)

plot_forward_mean_returns(analysis_df, plot_yearly = T)

long_short_backtest(quantile_df, prices_df, hold_period = 20, num_quantiles = 1, direction = 'momo')

long_per_quantile_backtest(quantile_df, prices_df)
