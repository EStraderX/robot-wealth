DATA_FOLDER <- '/Users/damian.fanaro/ResilioSync/FXBootcamp'

library(tidyquant)

BASEPATH <- paste0(DATA_FOLDER,'/Code/seasonality/intraday-seasonality/')
USD_FOLDER <- 'USD-in-ET'
EUR_FOLDER <- 'EUR-in-CET'
JPY_FOLDER <- 'JPY-in-JST' 
FULL_DATE <- '2009_2019'
YEAR_SUBSETS <- c('2009_2011','2012_2014','2015_2017','2018_2019')

irheatmap_subset <- function(basecurrency) {
  folder <- get(paste0(basecurrency,'_FOLDER'))
  
  if (basecurrency == 'USD') tzlabel <- 'ET'
  if (basecurrency == 'EUR') tzlabel <- 'CET'
  if (basecurrency == 'JPY') tzlabel <- 'JST'
  
  for (y in YEAR_SUBSETS) {
    filelocation <- paste0(BASEPATH, folder, '/seasonality_performance_', y, '_', basecurrency, '_detrended.csv')
    df <- read.csv(filelocation, header = TRUE, stringsAsFactors = FALSE)
    df$year <- y
    if (y == YEAR_SUBSETS[1]) {
      perf <- df
    } else {
      perf <- bind_rows(perf, df)
    }
    
    maptitle <- paste('Trade on the hour in', tzlabel, '(Detrended) - ', y)
    
    # Charts by year subset for all currencies
    p <- df %>%
      group_by(Asset) %>%
      ggplot(aes(StartHour, EndHour)) +
      geom_tile(aes(fill = IR)) + 
      scale_fill_viridis_c() +
      facet_wrap(~Asset) +
      xlab(paste0('StartHour (', tzlabel, ')')) +
      ylab(paste0('End Hour (', tzlabel, ')')) +
      ggtitle(maptitle)
    
    print(p)
  }
  
  # Charts for each asset by year. 
  for (a in unique(perf$Asset)) {
    p <- perf %>%
      filter(Asset == a) %>%
      group_by(year) %>%
      ggplot(aes(StartHour, EndHour)) +
      geom_tile(aes(fill = IR)) + 
      scale_fill_viridis_c() +
      facet_wrap(~year) +
      xlab(paste0('StartHour (', tzlabel, ')')) +
      ylab(paste0('End Hour (', tzlabel, ')')) +
      ggtitle(paste(a, 'on the hour in', tzlabel, 'by date subset'))
    
    print(p)
  }
}

irheatmap_subset('USD')