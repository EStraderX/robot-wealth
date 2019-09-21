# Try to decompose NZDUSD prices
library(fpp)
library(xts)

# Get EURUSD data
DATA_FOLDER <- '/Users/damian.fanaro/ResilioSync/FXBootcamp'
source(paste0(DATA_FOLDER,'/Code/tools/r-tools/data-utils.R'))
prices_df <- get_daily_OHLC('NZDUSD')

prices_xts <- xts(prices_df$Close, order.by = prices_df$Date)
colnames(prices_xts) <- 'Close'
prices_monthly <- to.monthly(prices_xts)[, 4]
prices_ts <- ts(prices_monthly, frequency = 12) # frequency = 12 is monthly
plot(prices_ts, main = 'NZDUSD raw')

# Decomposed using moving averages
nzdusd_decomposed <- decompose(prices_ts, "additive")
plot(nzdusd_decomposed)

# Plot seasonal component for each month
sp <- nzdusd_decomposed$seasonal[1:12]
barplot(sp, names.arg = 1:12, main = 'NZDUSD monthly seasonal component')