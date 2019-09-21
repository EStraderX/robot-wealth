library('cluster')

DATA_FOLDER <- '/Users/damian.fanaro/ResilioSync/FXBootcamp'
data_utils <- paste0(DATA_FOLDER,'/Code/tools/r-tools/data-utils.R')
print(data_utils)
source(data_utils)

# Get asset lists
asset_list <- get_asset_list("AssetsDWX-FX-USD")
raw_prices_df <- get_daily_OHLC(asset_list$Name)
usd_prices_df <- convert_common_quote_currency(raw_prices_df, quote_currency = 'USD')
assetNames <- distinct(usd_prices_df, Ticker) 

# Get short term interest rates
currencies <- get_unique_currencies(usd_prices_df)
policy_rates_df <- get_policy_rates(currencies)
usd_extended_prices_df <- append_interest_rate_differential(usd_prices_df, policy_rates_df)

usd_returns <-  usd_extended_prices_df %>%
    group_by(Ticker) %>%
    tq_transmute_(select = "Total_Return_Index", 'ROC', n = 1, type = 'discrete', na.pad = FALSE, col_rename = 'returns')

latest_date <- as.Date('2019-07-01')

recent_rates <- policy_rates_df %>%
  filter(Date == latest_date) %>%
  select(Currency, Rate) %>%
  arrange(Rate)

usd_rate <- recent_rates %>% 
  filter(Currency == 'USD') %>%
  select(Rate)

recent_rates$carry <- recent_rates$Rate - usd_rate[[1]]

p <- ggplot(recent_rates, aes(x=reorder(Currency, carry, sum), y=carry)) +
  geom_bar(stat = 'identity') +
  labs(title = 'Carry Long USD vs Currency', x = 'Ticker')

p

plot_corr_matrix <- function(returns_df, startDate = '2016-01-01', endDate = '2019-12-31') {
  corr_data <- returns_df %>%
    filter(Date >= as.Date(startDate) & Date <= as.Date(endDate)) %>%
    spread(key = Ticker, value = returns) %>%
    na.omit() 
  
  x <- as.matrix.data.frame(corr_data[,-1])
  # Take USD out of column names to make plots nicer
  colnames(x) <- substr(colnames(x),1,3)
  
  correlation <- cor(x, method = 'pearson')
  PerformanceAnalytics::chart.Correlation(x)
}

plot_corr_matrix(usd_returns)

plot_corr_matrix(usd_returns, startDate = '2017-01-01', endDate = '2019-01-01')


plot_cor_clusters <- function(returns_df, number_of_clusters = 4, startDate = '2016-01-01', endDate = '2019-12-31') {
  corr_data <- returns_df %>%
  filter(Date >= as.Date(startDate) & Date <= as.Date(endDate)) %>%
  spread(key = Ticker, value = returns) %>%
  na.omit()

  x <- as.matrix.data.frame(corr_data[,-1])
  # Take USD out of column names to make plots nicer
  colnames(x) <- substr(colnames(x),1,3)
  
  correlation <- cor(x, method = 'pearson')
  dissimilarity <- 1 - correlation
  distance <- as.dist(dissimilarity)
  xy <- cmdscale(distance)
  fit <- kmeans(xy, number_of_clusters, iter.max=100, nstart=100)
  clusplot(xy, fit$cluster, color=TRUE, shade=TRUE, labels=3, lines=0, plotchar=F, 
    main = paste('FX Clusters', startDate, 'to', endDate), sub='') 
}

plot_cor_clusters(usd_returns, number_of_clusters = 6)


fx_pnl <- function(base.quote.buy, base.quote.sell, acnt.quote.buy, acnt.quote.sell) {
  return ((base.quote.sell/acnt.quote.sell) / (base.quote.buy/acnt.quote.buy) - 1)
}

account_cccy_trade_close <- seq(from = 0.6, to = 1.2, by = 0.05)
account_cccy_trade_close_reversed <- rev(seq(from = 0.6, to = 1.2, by = 0.05))
trade_pnl <- fx_pnl(base.quote.buy=1.05, base.quote.sell=1.43, acnt.quote.buy=0.74, acnt.quote.sell=account_cccy_trade_close)

plot(account_cccy_trade_close, trade_pnl, type='l', col='blue')
lines(account_cccy_trade_close_reversed, trade_pnl, type='l', col='orange')
abline(h=0.0, col='red', lty=2)
