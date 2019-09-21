# Set to your folder
DATA_FOLDER <- '/Users/damian.fanaro/ResilioSync/FXBootcamp'

# Get Exchange rate data and convert dates to ET
usd_returns <-  usd_prices_df %>%
  group_by(Ticker) %>%
  mutate(returns = Close / lag(Close) - 1)
source(paste0(DATA_FOLDER,'/Code/tools/r-tools/data-utils.R'))

asset_list <- get_asset_list("AssetsDWX-FX-USD")
raw_prices_df <- get_hourly_OHLC(asset_list$Name)
usd_prices_df <- convert_common_quote_currency(raw_prices_df, quote_currency = 'USD')
assetNames <- distinct(usd_prices_df, Ticker) 

datetime <- as.POSIXct(paste(usd_returns$Date, usd_returns$Time), format="%Y-%m-%d %H:%M", tz='UTC')
attributes(datetime)$tzone <- 'America/New_York'
usd_returns$datetime <- datetime

# Calculate returns
returns_df <- usd_returns %>%
  mutate(hour = hour(datetime),
         year = year(datetime),
         month = month(datetime),
         day = day(datetime)) %>%
  select(Ticker, datetime, hour, year, month, day, returns) %>%
  na.omit()

# Create seasonality plots
ticker_subset <- c('EURUSD','GBPUSD','CADUSD', 'JPYUSD','AUDUSD','CHFUSD')

seasonality_year_bin <- function(ticker_subset = 'EURUSD', years_subset = 2009:2019, bins = 2) {
  returns_df %>%
    filter(Ticker %in% ticker_subset) %>%
    filter(year %in% years_subset) %>%
    mutate(bin = ntile(year, bins)) %>%
    mutate(bin = as.factor(bin)) %>%
    group_by(Ticker, bin, hour) %>%
    summarise(meanreturns = mean(returns * 100)) %>%
    mutate(cumreturns = cumsum(meanreturns)) %>%
    ggplot(aes(x=hour, y=cumreturns, color = bin)) +
    geom_line() +
    labs(title = paste('Cumulative Mean Returns by Hour (ET) By Year Subset', years_subset[1], '-', years_subset[length(years_subset)])) +
    xlab("Hour (ET)") + 
    ylab("Hourly returns %") +
    facet_wrap(~Ticker)
}  

seasonality_year_bin('EURUSD', bins = 3)