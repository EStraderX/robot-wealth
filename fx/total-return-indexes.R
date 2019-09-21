DATA_FOLDER <- '/Users/damian.fanaro/ResilioSync/FXBootcamp'
data_utils <- paste0(DATA_FOLDER,'/Code/tools/r-tools/data-utils.R')
print(data_utils)
source(data_utils)

ASSET_LIST <- 'AssetsDWX-FX-USD'
asset_list <- get_asset_list(ASSET_LIST)
asset_list

prices_df <- get_daily_OHLC(asset_list$Name)
prices_df

usd_prices_df <- convert_common_quote_currency(prices_df, quote_currency = 'USD')

cad_usd <- usd_prices_df %>%
  filter(Ticker == 'CADUSD', Date == '1999-07-09')

usd_cad <- prices_df %>%
  filter(Ticker == 'USDCAD', Date == '1999-07-09')

currencies <- get_unique_currencies(usd_prices_df)
policy_rates_df <- get_policy_rates(currencies)

usd_extended_prices_df <- append_interest_rate_differential(usd_prices_df, policy_rates_df)


tickers <- distinct(usd_extended_prices_df, Ticker)[[1]]
for (i in tickers) {
  print(usd_extended_prices_df %>%
          filter(Ticker == i) %>%
          select(Ticker, Date, Spot_Return_Index, Interest_Return_Index, Total_Return_Index) %>%
          gather('Spot_Return_Index','Interest_Return_Index','Total_Return_Index', key = 'type', value = 'value') %>%
          ggplot(aes(x = Date, y = value, colour = type)) +
          geom_line() +
          labs(title = i) + 
          theme_tq())
}