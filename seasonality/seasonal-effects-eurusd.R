library(fpp)
library(xts)

# Get EURUSD data
DATA_FOLDER <- '/Users/damian.fanaro/ResilioSync/FXBootcamp'
source(paste0(DATA_FOLDER,'/Code/tools/r-tools/data-utils.R'))
prices_df <- get_daily_OHLC('EURUSD')

prices_xts <- xts(prices_df$Close, order.by = prices_df$Date)
colnames(prices_xts) <- 'Close'

prices_monthly <- to.monthly(prices_xts)[,4] # Just get close
prices_ts <- ts(prices_monthly, frequency = 12) # frequency = 12 is monthly
plot(prices_ts, main = 'EURUSD raw')

# Decomposed using moving averages
eurusd_decomposed <- decompose(prices_ts, "additive")
plot(eurusd_decomposed)

# Plot seasonal component for each month
sp <- eurusd_decomposed$seasonal[1:12]
barplot(sp, names.arg = 1:12, main = 'EURUSD monthly seasonal component')