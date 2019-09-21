library(dplyr)

path = "data/logfile.csv"

trades <- read.csv(path, header = TRUE)

str(trades)
glimpse(trades)


# SELECT OPERATION

PnL <- select(trades, Type, Asset, Name, Profit)
glimpse(PnL)


# MUTATE OPERATION

trades$Open <- as.POSIXct(trades$Open, format="%Y-%m-%d %H:%M", tz="GMT")
trades$Close <- as.POSIXct(trades$Close, format="%Y-%m-%d %H:%M", tz="GMT")

glimpse(trades)

trades <- mutate(trades, Duration = Close - Open)
glimpse(trades)


# FILTER OPERATION

EUR_USD_Trades <- filter(trades, Asset == "EUR/USD")
glimpse(EUR_USD_Trades)

EUR_USD_JPY_Trades <- filter(trades, Asset == "EUR/USD" | Asset == "USD/JPY")
EUR_USD_JPY_Trades <- filter(trades, Asset %in% c("EUR/USD", "USD/JPY"))
glimpse(EUR_USD_JPY_Trades)

SPX_H4_winners <- filter(trades, Asset == "SPX500" & Name == "H4" & Profit > 0)
glimpse(SPX_H4_winners)


# SUMMARISE OPERATION

mean_EU <- summarise(EUR_USD_Trades, mean=mean(Profit))
sd_SPX_H4_winners <- summarise(SPX_H4_winners, sd = sd(Profit))

str(mean_EU)
str(sd_SPX_H4_winners)


# ARRANGE OPERATION

sorted <- arrange(trades, desc(Profit))
str(sorted)


# COMBINING OPERATIONS

filter(trades, Asset=='EUR/USD') %>% summarise(mean=mean(Profit))

trades %>% filter(Asset=='EUR/USD') %>% summarise(mean=mean(Profit))

trades %>%
  mutate(Duration = Close - Open) %>%
  filter(Type == 'Long') %>%
  summarise(min_duration = min(Duration),
            max_duration = max(Duration),
            avg_duration = mean(Duration))

trades %>%
  mutate(Duration = Close - Open) %>%
  filter(Profit < 0, Duration > 10) %>%
  summarise(sd = sd(Profit))

trades %>%
  filter(Profit < 0) %>%
  group_by(Asset) %>%
  summarise(avg_loser = mean(Profit))

trades %>%
  filter(Profit < 0) %>%
  group_by(Asset) %>%
  summarise(avg_loser = mean(Profit)) %>%
  mutate(rank = rank(avg_loser)) %>%
  arrange(rank)

trades %>%
  group_by(Asset, Name) %>%
  summarise(count = n())

trades <- trades %>%
  mutate(Equity = cumsum(Profit))

plot(trades$Equity, type='l', col='blue', xlab='Trade Number', ylab='Equity')

trades <- trades %>%
  mutate(Daily_Return = cumsum(Equity))

plot(trades$Daily_Return, type='l', col='orange', xlab='Trade Number', ylab='Daily Return')

eu <- trades %>%
  filter(Asset == "EUR/USD", Name == "H1") %>%
  mutate(Equity = cumsum(Profit))

plot(eu$Equity, type='l', col='darkgreen', xlab='Trade Number', ylab='Equity')
