library(gridExtra)

DATA_FOLDER <- '/Users/damian.fanaro/ResilioSync/FXBootcamp'
data_utils <- paste0(DATA_FOLDER,'/Code/tools/r-tools/data-utils.R')
print(data_utils)
source(data_utils)

ASSET_LIST <- 'AssetsBCAMP-ALL'
asset_list <- get_asset_list(ASSET_LIST)
prices_df <- get_daily_OHLC(asset_list$Name)

p <- list()
for (i in asset_list$Name) {
  ticker = (gsub('/', '', i))
  prices_df %>%
    filter(Ticker == ticker) %>%
    ggplot(aes(x = Date, y = Close)) +
    geom_line() +
    labs(title = ticker) -> p[[i]]
}
do.call(grid.arrange, p)
