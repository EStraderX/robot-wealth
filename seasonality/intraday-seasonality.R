library(tidyquant)

# Get EURUSD data
DATA_FOLDER <- '/Users/damian.fanaro/ResilioSync/FXBootcamp'
source(paste0(DATA_FOLDER,'/Code/tools/r-tools/data-utils.R'))
prices_df <- read.csv(paste0(DATA_FOLDER, '/Hourly/EURUSD.csv'), header=FALSE, stringsAsFactors = FALSE)

colnames(prices_df) <- c("Date", "Time", "Open", "High", "Low", "Close", "Volume")
datetime <- as.POSIXct(paste(prices_df$Date, prices_df$Time), format="%Y%m%d %H:%M", tz="UTC")
attributes(datetime)$tzone <- "America/New_York"

prices_xts <- xts(prices_df$Close, order.by = datetime)
colnames(prices_xts) <- 'Close'
prices_xts$hour <- hour(datetime)
prices_xts$return <- Return.calculate(prices_xts$Close)

df <- as.data.frame(coredata(prices_xts["/2018"]))
df <- na.omit(df)
day_season <- df %>% 
  group_by(hour) %>%
  summarise(mean_ret=mean(return))

barplot(height=cumsum(day_season$mean_ret), names.arg = day_season$hour, col = 'darkred', main = "EUR/USD intraday seasonality in NY time")