## Set DATA_FOLDER
DATA_FOLDER <- '/Users/damian.fanaro/ResilioSync/FXBootcamp'

## Get Hourly Exchange Rate Data
source(paste0(DATA_FOLDER,'/Code/tools/r-tools/data-utils.R'))

asset_list <- get_asset_list("AssetsDWX-FX-USD")
raw_prices_df <- get_hourly_OHLC(asset_list$Name)
usd_prices_df <- convert_common_quote_currency(raw_prices_df, quote_currency = 'USD')
assetNames <- distinct(usd_prices_df, Ticker) 

## Calculate Returns and Convert to Eastern Time
usd_returns <-  usd_prices_df %>%
  group_by(Ticker) %>%
  mutate(returns = Close / lag(Close) - 1)

usd_returns <- usd_returns %>%
  mutate('returns %' = returns * 100)

datetime <- as.POSIXct(paste(usd_returns$Date, usd_returns$Time), format="%Y-%m-%d %H:%M", tz='UTC')
attributes(datetime)$tzone <- 'America/New_York'
usd_returns$datetime <- datetime

returns_df <- usd_returns %>%
  mutate(hour = hour(datetime),
         year = year(datetime),
         month = month(datetime),
         day = day(datetime)) %>%
  select(Ticker, datetime, hour, year, month, day, returns) %>%
  na.omit()


## Full Sample Seasonality Plots
seasonality_barplot <- function(ticker_subset = 'EURUSD', years_subset = 2009:2019) {
  returns_df %>%
    filter(Ticker %in% ticker_subset) %>%
    filter(year %in% years_subset) %>%
    group_by(Ticker, hour) %>%
    summarise(meanreturns = mean(returns * 100)) %>%
    ggplot(aes(x=as.factor(hour), y=meanreturns, fill = Ticker)) +
    geom_bar(stat='identity', position = position_dodge()) +
    labs(title = paste('Mean Returns by Hour (ET)', years_subset[1], '-', years_subset[length(years_subset)])) +
    xlab("Hour (ET)") + 
    ylab("Mean hourly returns %") +
    facet_wrap(~Ticker) +
    theme(legend.position = 'none')
}

## Cumulative Returns for Multiple Currencies
seasonality_cumline <- function(ticker_subset = 'EURUSD', years_subset = 2009:2019) {
  returns_df %>%
    filter(Ticker %in% ticker_subset) %>%
    filter(year %in% years_subset) %>%
    group_by(Ticker, hour) %>%
    summarise(meanreturns = mean(returns * 100)) %>%
    mutate(cumreturns = cumsum(meanreturns)) %>%
    ggplot(aes(x=hour, y=cumreturns, color = Ticker)) +
    geom_line() +
    labs(title = paste('Cumulative Mean Returns by Hour (ET)', years_subset[1], '-', years_subset[length(years_subset)])) +
    xlab("Hour (ET)") + 
    ylab("Hourly returns %")
}

## Seasonality boxplot
seasonality_boxplot <- function(ticker = 'EURUSD', years_subset = 2009:2019) {
  returns_df %>%
    filter(Ticker %in% ticker) %>%
    filter(year %in% years_subset) %>%
    ggplot(aes(x=as.factor(hour), y=returns)) +
    geom_boxplot() +
    geom_jitter(width=0.1, alpha=0.2) +
    xlab("Hour (ET)") + 
    ylab("Hourly returns %") +
    labs(title = paste(ticker, 'Returns by Hour (ET)', years_subset[1], '-', years_subset[length(years_subset)]))
}

# Cumulative Returns by Currency by Year
seasonality_cumline_year <- function(ticker_subset = 'EURUSD', years_subset = 2009:2019) {
  returns_df %>%
    filter(Ticker %in% ticker_subset) %>%
    filter(year %in% years_subset) %>%
    mutate(year = as.factor(year)) %>%
    group_by(Ticker, year, hour) %>%
    summarise(meanreturns = mean(returns * 100)) %>%
    mutate(cumreturns = cumsum(meanreturns)) %>%
    ggplot(aes(x=hour, y=cumreturns, color = year)) +
    geom_line() +
    labs(title = paste('Cumulative Mean Returns by Hour (ET) By Year', years_subset[1], '-', years_subset[length(years_subset)])) +
    xlab("Hour (ET)") + 
    ylab("Hourly returns %") +
    facet_wrap(~Ticker)
} 

## Subset of currencies to plot
ticker_subset <- c('EURUSD','GBPUSD','CADUSD', 'JPYUSD','AUDUSD','CHFUSD')

## Plot
seasonality_barplot(ticker_subset)
seasonality_cumline(ticker_subset)
seasonality_boxplot('EURUSD')


for (i in 2009:2019) {
  print(seasonality_cumline(ticker_subset, i))
}

seasonality_cumline_year()
