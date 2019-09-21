####
# Title: Risk Premia R shiny app
# Author: Jaap
# Version: 1.0
# Version Date: 04-aug-2019 
# Required packages: shiny, PerformanceAnalytics, tidyr, xts, googledrive, DT, ggplot2, BatchGetSymbols, reshape, FRAPO
# Please install packages with dependancies
###


# This application visualizes the price and return data including the Risk Premia strategy returns for a chosen ETF basket
# Given:
# - a certain portfolio in terms of capital to invest
# - current positions in the ETF basket 
# - the latests Risk Premia weights 
# - the latest ETF price data
# the app will calculate the delta positions

# Notes:
### - Always make sure that the when you replace the ETF basket with your own tickers you should follow the sequence in symbols_RP:
# 1 = VTI eq, 2 = VWO equivalent, 3 = VEA equivalent, 4 = IEF equivalent, 5 = TLT equivalent, 6 = EMB equivalent, 7 = VNQI equivalent, 8 = GBTC
# It is hardcoded that the Risk Premia weight for symbols_RP[6], which is the EMB equivalent, should be 15% as per the strategy
# Also symbols_RP[8] which is GBTS is removed from strategy and stock return graphs since it will mess up the graph due to its high pos/neg returns

### - The data for the calculations is currently loaded from my google drive, please replace this with your own data pipeline
# If you replace the data and you do not include BTMA.AS you will get an error. In line 685 it is hardcoded to remove a datapoint since it is incorrect.
# You can use this section in the code to hardcode your own outlier removal...

### - Always run the first section of the code to get the data and calculate strategy results
# When the data is in your workspace you can launch the app to visualise it and calculate position delta's

### - For readability of the code when running the app you can close the bracket at line 43 and line 776


# Feel free to modify this app to suit your own use, I'm not the greatest coder so it is a little messy however you should be able to use it as a blueprint
# Have fun!

############
# RUN FIRST
############

#### load data and run Risk Premia backtest and weights calculator
{
library(PerformanceAnalytics)
library(tidyr)
library(xts)
require(googledrive)
require(shiny)


####### strategy code
{
  get_srp_weights <- function(rets, lookback=90, rebal = 22, equalise = c('volatility', 'variance')) {
    n_assets <- ncol(rets)
    vols <- na.omit(apply.rolling(rets[,1], width=lookback, by=rebal, FUN='StdDev.annualized'))
    for (i in 2:n_assets) {
      vols <- merge(vols, na.omit(apply.rolling(rets[,i], width=lookback, by=rebal, FUN='StdDev.annualized')))  
    }
    if (equalise == 'variance') { 
      raw_weights <- 1/vols^2
    } else {
      # else equalise volatility
      raw_weights <- 1/vols
    }
    weights <- raw_weights / rowSums(raw_weights) # Normalise weights to add to 1
    colnames(weights) <- colnames(rets)
    return(weights)
  }
  
  get_cor_adjustments <- function(rets, covar_lookback = 90, vol_weights, covarmethod=c('covariance','correlation')) {
    # Get average pairwise covariance based weights
    rebal_indexes <- index(vol_weights)
    cov_weights <- xts(matrix(0, length(rebal_indexes), ncol(rets)), order.by = index(rets[rebal_indexes]))
    colnames(cov_weights) <- colnames(rets)
    for (i in 1:length(rebal_indexes)) {
      j <- rebal_indexes[i]
      rets_roll_sample <- rets[seq(j-covar_lookback, j, 1)] 
      if (covarmethod == 'correlation') {
        sample_cov <- cor(rets_roll_sample)
      } else {
        sample_cov <- cov(rets_roll_sample)
      }
      sample_pairwise_avg <- rowMeans(sample_cov)
      # scale to a gaussian between 0 and 1
      sample_gaussian_scale <- 1 - pnorm((sample_pairwise_avg - mean(sample_pairwise_avg)) / sd(sample_pairwise_avg))
      sample_weights <- sample_gaussian_scale / sum(sample_gaussian_scale)
      cov_weights[i,] <- sample_weights
    }
    # Set adjustments to sum to zero over all assets
    norm_weights <- (cov_weights / rowSums(cov_weights)) - (1/ncol(cov_weights))
    return(norm_weights)
  }
  
  # Return cross-sectional zscore of total return momentum
  get_momo_adjustments <- function(rets, formation_period = 6 * 22, vol_weights) {
    synthetic_prices <- cumprod(1+rets) 
    rebal_indexes <- index(vol_weights)
    momo <- na.omit(xts::lag.xts(TTR::ROC(synthetic_prices, n=formation_period, type='discrete'),1))[rebal_indexes]
    momo_weights <- xts(matrix(0, length(index(momo)), ncol(vol_weights)), order.by = index(momo))
    colnames(momo_weights) <- colnames(vol_weights)
    # Iterate through and normalise to zscore
    for(i in 1:nrow(momo)) {
      r <- momo[i]
      rd <- coredata(r)
      zscale <- (r - mean(rd)) / sd(rd)
      momo_weights[i,] <- zscale
    }
    return(momo_weights)
  }
  
  # Return cross-sectional zscore of slope of moving average
  get_sma_slope_adjustment <- function(rets, formation_period = 6 * 22, vol_weights) {
    synthetic_prices <- cumprod(1+rets) 
    rebal_indexes <- index(vol_weights)
    sma <- synthetic_prices
    sma[] <- NA
    for (i in 1:ncol(synthetic_prices)) {
      sma[,i] <- TTR::SMA(synthetic_prices[,i], n =formation_period)
    }
    sma_slope <- sma / xts::lag.xts(sma) - 1
    momo <- na.omit(xts::lag.xts(sma_slope, 1))[rebal_indexes]
    momo_weights <- xts(matrix(0, length(index(momo)), ncol(vol_weights)), order.by = index(momo))
    colnames(momo_weights) <- colnames(vol_weights)
    # Iterate through and normalise to zscore
    for(i in 1:nrow(momo)) {
      r <- momo[i]
      rd <- coredata(r)
      zscale <- (r - mean(rd)) / sd(rd)
      momo_weights[i,] <- zscale
    }
    return(momo_weights)  
  }
  
  adjust_weights <- function(volweights, covaradjustments, shrinkagemultiplier) {
    w <- volweights * (1 + (covaradjustments * shrinkagemultiplier))
    return(w / rowSums(w)) # Ensure all weights sum to 1
  }
  
  adjust_weights_momo <- function(baseweights, momoadjustments, zscorecutoff = 0, multiplier = 0.1) {
    w <- baseweights * (1 + ((momoadjustments >= zscorecutoff) * multiplier))
    return (w / rowSums(w)) # Ensure all weights sum to 1
  }
  
  get_unlevered_target_weights <- function(ret, rebal=22, vol_lookback=90, cor_lookback=120, cor_shrinkage=1, adjust_momo = TRUE, momo_multiplier = 0.1) {
    volw <- get_srp_weights(ret, lookback=vol_lookback, rebal=rebal, equalise='volatility')
    coradjust <- get_cor_adjustments(ret, covar_lookback = cor_lookback, volw, covarmethod='correlation')
    coradjustweights <- adjust_weights(volw, coradjust, cor_shrinkage)
    
    # Get total return momentum adjustments at 3, 6, 9, 12 month lookbacks
    momoadjust3 <- get_momo_adjustments(ret, formation_period = 3*22, vol_weights = volw)
    momoadjustweights <- adjust_weights_momo(coradjustweights, momoadjust3, zscorecutoff = 0, multiplier = momo_multiplier)
    momoadjust6 <- get_momo_adjustments(ret, formation_period = 6*22, vol_weights = volw)
    momoadjustweights <- adjust_weights_momo(momoadjustweights, momoadjust6, zscorecutoff = 0, multiplier = momo_multiplier)
    momoadjust9 <- get_momo_adjustments(ret, formation_period = 9*22, vol_weights = volw)
    momoadjustweights <- adjust_weights_momo(momoadjustweights, momoadjust9, zscorecutoff = 0, multiplier = momo_multiplier)
    momoadjust12 <- get_momo_adjustments(ret, formation_period = 12*22, vol_weights = volw)
    momoadjustweights <- adjust_weights_momo(momoadjustweights, momoadjust12, zscorecutoff = 0, multiplier = momo_multiplier)
    
    # Get sma slope adjustments
    smaadjust3 <- get_sma_slope_adjustment(ret, formation_period = 3*22, vol_weights = volw)
    smaadjustweights <- adjust_weights_momo(momoadjustweights, smaadjust3, zscorecutoff = 0, multiplier = momo_multiplier)
    smaadjust6 <- get_sma_slope_adjustment(ret, formation_period = 6*22, vol_weights = volw)
    smaadjustweights <- adjust_weights_momo(smaadjustweights, smaadjust6, zscorecutoff = 0, multiplier = momo_multiplier)
    smaadjust9 <- get_sma_slope_adjustment(ret, formation_period = 9*22, vol_weights = volw)
    smaadjustweights <- adjust_weights_momo(smaadjustweights, smaadjust9, zscorecutoff = 0, multiplier = momo_multiplier)
    smaadjust12 <- get_sma_slope_adjustment(ret, formation_period = 12*22, vol_weights = volw)
    smaadjustweights <- adjust_weights_momo(smaadjustweights, smaadjust12, zscorecutoff = 0, multiplier = momo_multiplier)
    return(smaadjustweights)
  }
  
  get_levered_target_weights <- function(rets, unlevered_weights, vol_target = 0.1, vol_lookback = 90, maxleverage = 3) {
    # Assume we're not rebalancing any more frequently than previously
    dailyweights <- xts(matrix(NA, nrow(rets), ncol(rets)), order.by = index(rets))
    dailyweights[index(unlevered_weights),] <- coredata(unlevered_weights)
    dailyweights <- na.omit(na.locf(dailyweights))
    # Now scale the returns by the asset weights
    scaleret <- xts::lag.xts(dailyweights,1) * rets
    colnames(scaleret) <- colnames(rets)
    unl_port_returns <- xts(rowSums(scaleret), order.by = index(scaleret))
    
    rebal_indexes <- index(unlevered_weights)
    lev_weights <- xts(matrix(0, length(rebal_indexes), ncol(rets)), order.by = index(rets[rebal_indexes]))
    colnames(lev_weights) <- colnames(rets)
    for (i in 2:length(rebal_indexes)) {
      j <- rebal_indexes[i]
      rets_roll_sample <- unl_port_returns[seq(j-vol_lookback, j, 1)] 
      vol_forecast <- StdDev.annualized(rets_roll_sample)
      weight_multiplier <- vol_target / vol_forecast[1]
      if(weight_multiplier > maxleverage) weight_multiplier <- maxleverage
      lev_weights[i,] <- weight_multiplier * unlevered_weights[i,]
    }
    return(lev_weights)
    
  }
  
  get_cap_allocation_and_rescale <- function(weights, ticker, cap=0.15,leverage = 1) {
    
    # cap the allocation into ticker and rescale remaining weights
    
    if (weights[nrow(weights),paste(ticker)] > cap) {
      
      weights[nrow(weights),paste(ticker)] <- cap
      
      weights[nrow(weights),!(names(weights) %in% ticker)] <- (leverage-cap)* (weights[nrow(weights),!(names(weights) %in% ticker)]) / sum(weights[nrow(weights),!(names(weights) %in% ticker)])
      
      
    }
    
    return(weights)
    
  }
  
  
  
}

# risk stability code
{
  
  # Convert xts to df for plotting
  xts_to_tidy_df <- function(x) {
    df <- cbind(index(x), as.data.frame(coredata(x)))
    colnames(df)[1] <- 'Date'
    gather(df, key='Asset', value='Value', -Date) 
  }
  
  # Performance summary function.
  # Returns summary details and prints charts and tables as side effects
  riskperformance <- function(ret, weights, samplefreq = 'quarters', plot = TRUE, margin_cost = 0) {
    # ret <- returns_daily[,-5]
    # weights <- adjweights
    dailyweights <- xts(matrix(NA, nrow(ret), ncol(ret)), order.by = index(ret))
    dailyweights[index(weights),] <- coredata(weights)
    dailyweights <- na.omit(na.locf(dailyweights))
    
    # Now scale the returns by the asset weights
    scaleret <- xts::lag.xts(dailyweights,1) * ret
    colnames(scaleret) <- colnames(ret)
    
    # Get trades as percentage of portfolio
    tradesize <- na.omit(abs(dailyweights - xts::lag.xts(dailyweights)))
    totaltrade <- rowSums(tradesize)
    
    # Get monthly endpoints
    monthend <- endpoints(scaleret, on = samplefreq)[-1] # remove the first zero from the endpoints vector
    medt <- index(scaleret[monthend])
    
    # Create xts objects to hold volatilities and risk contributions
    vol_xts <- xts(matrix(NA, length(medt), ncol(ret)), order.by = index(scaleret[medt]))
    mrc_xts <- xts(matrix(NA, length(medt), ncol(ret)), order.by = index(scaleret[medt]))
    ts_xts <- xts(matrix(NA, length(medt), ncol(ret)), order.by = index(scaleret[medt]))
    colnames(vol_xts) <- colnames(mrc_xts) <- colnames(ts_xts) <- colnames(ret)
    
    i <- medt[1]
    
    require(FRAPO)
    
    for (n in 2:length(medt)) {
      j <- medt[n]
      r <- scaleret[paste0(i,'::',j)] # Get returns over window
      # Volatility
      vols <- StdDev.annualized(r) # Estimate volatility of each asset
      vol_xts[i,] <- vols
      # MRCD
      mrc <- FRAPO::mrc(rep(1, ncol(scaleret)), cov(r)) # Estimate risk contribution of each asset (setting weights to zero)
      mrc_xts[i,] <- mrc
      # Trades
      ts <- colSums(tradesize[paste0(i,'::',j)])
      ts_xts[i,] <- ts
      i <- j
    }
    
    
    unloadNamespace("FRAPO")
    vol_xts <- na.omit(vol_xts)
    prop_xts <- vol_xts / rowSums(vol_xts)
    mrc_xts <- na.omit(mrc_xts)
    ts_xts <- na.omit(ts_xts)
    
    
    # Realised volatility tracking error
    trackerror_vol <- mean(rowMeans(abs(prop_xts - 1/ncol(prop_xts)))) * 100
    
    # Calculate mean tracking error on MRC
    trackerror_mrc <- mean(rowMeans(abs(mrc_xts - 100/ncol(mrc_xts))))
    
    # Tradesize
    mean_tradesize <- mean(rowSums(ts_xts))
    
    # Performance
    portfolio_returns <- xts(rowSums(scaleret), order.by=index(scaleret))
    # Subtract the cost of leverage if appropriate
    if (margin_cost != 0) {
      # Look at cost of leverage
      port_weights <- xts(rowSums(dailyweights), order.by = index(dailyweights))
      margin <- port_weights - 1
      marginreturn <- margin * (margin_cost / 360) # Estimating margin cost at annualised % / 360
      portfolio_returns <- portfolio_returns - marginreturn
    }
    
    t <- table.AnnualizedReturns(portfolio_returns)
    cagr <- t[,1][1] * 100
    sharpe <- t[,1][3]
    perf <- data.frame(trackerror_vol, trackerror_mrc, mean_tradesize, cagr, sharpe)
    
    if (plot == TRUE) {
      # Realised volatility
      #print(plot(vol_xts, main = 'Realised Volatility', legend.loc='topleft'))
      #vol_df <- xts_to_tidy_df(vol_xts)
      #print(ggplot(vol_df, aes(x=Date, y=Value, fill=Asset)) + geom_area() + ggtitle('Realised Volatility'))
      
      # Realised volatility %
      #print(plot(prop_xts, main = 'Realised Volatility %', legend.loc='topleft'))
      #prop_df <- xts_to_tidy_df(prop_xts)
      #print(ggplot(prop_df, aes(x=Date, y=Value, fill=Asset)) + geom_area() + ggtitle('Realised Volatility as Proportion of Total'))
      # Calculate mean tracking error (as %age)
      #trackerror_vol <- mean(rowMeans(abs(prop_xts - 1/ncol(prop_xts)))) * 100
      
      #Risk contribution
      #print(plot(mrc_xts, main = 'Risk Contribution', legend.loc='topleft'))
      #mrc_df <- xts_to_tidy_df(mrc_xts)
      #print(ggplot(mrc_df, aes(x=Date, y=Value, fill=Asset)) + geom_area() + ggtitle('Risk Contribution'))
      # Calculate mean tracking error on MRC
      #trackerror_mrc <- mean(rowMeans(abs(mrc_xts - 100/ncol(mrc_xts))))
      
      # Tradesize
      #print(plot(ts_xts, main='Trade Size', legend.loc='topleft'))
      #tradesize_df <- xts_to_tidy_df(ts_xts)
      #print(ggplot(tradesize_df, aes(x=Date, y=Value, fill=Asset)) + geom_area() + ggtitle('Rebalance Trades'))
      #mean_tradesize <- mean(rowSums(ts_xts))
      
      # Weights
      #print(plot(weights, main='Asset weights', legend.loc='topleft'))
      # weights_df <- xts_to_tidy_df(weights)
      # print(ggplot(weights_df, aes(x=Date, y=Value, fill=Asset)) + geom_area() + ggtitle('Asset Weights'))
      
      # Performance
      #print(table.DownsideRisk(portfolio_returns))
      #print(table.Drawdowns(portfolio_returns))
      charts.PerformanceSummary_edit(portfolio_returns, main = 'Portfolio Returns')
      #print(perf[,c('cagr','sharpe')])
    }
    return(perf[,c('cagr','sharpe')])
  }
  
  # return backtest cumulative returns
  
  returns_backtest <- function(ret, weights, samplefreq = 'quarters', plot = TRUE, margin_cost = 0) {
    # ret <- returns_daily[,-5]
    # weights <- adjweights
    dailyweights <- xts(matrix(NA, nrow(ret), ncol(ret)), order.by = index(ret))
    dailyweights[index(weights),] <- coredata(weights)
    dailyweights <- na.omit(na.locf(dailyweights))
    
    # Now scale the returns by the asset weights
    scaleret <- xts::lag.xts(dailyweights,1) * ret
    colnames(scaleret) <- colnames(ret)
    
    # Get trades as percentage of portfolio
    tradesize <- na.omit(abs(dailyweights - xts::lag.xts(dailyweights)))
    totaltrade <- rowSums(tradesize)
    
    # Get monthly endpoints
    monthend <- endpoints(scaleret, on = samplefreq)[-1] # remove the first zero from the endpoints vector
    medt <- index(scaleret[monthend])
    
    # Create xts objects to hold volatilities and risk contributions
    vol_xts <- xts(matrix(NA, length(medt), ncol(ret)), order.by = index(scaleret[medt]))
    mrc_xts <- xts(matrix(NA, length(medt), ncol(ret)), order.by = index(scaleret[medt]))
    ts_xts <- xts(matrix(NA, length(medt), ncol(ret)), order.by = index(scaleret[medt]))
    colnames(vol_xts) <- colnames(mrc_xts) <- colnames(ts_xts) <- colnames(ret)
    
    i <- medt[1]
    
    require(FRAPO)
    
    for (n in 2:length(medt)) {
      j <- medt[n]
      r <- scaleret[paste0(i,'::',j)] # Get returns over window
      # Volatility
      vols <- StdDev.annualized(r) # Estimate volatility of each asset
      vol_xts[i,] <- vols
      # MRCD
      mrc <- FRAPO::mrc(rep(1, ncol(scaleret)), cov(r)) # Estimate risk contribution of each asset (setting weights to zero)
      mrc_xts[i,] <- mrc
      # Trades
      ts <- colSums(tradesize[paste0(i,'::',j)])
      ts_xts[i,] <- ts
      i <- j
    }
    
    
    unloadNamespace("FRAPO")
    vol_xts <- na.omit(vol_xts)
    prop_xts <- vol_xts / rowSums(vol_xts)
    mrc_xts <- na.omit(mrc_xts)
    ts_xts <- na.omit(ts_xts)
    
    
    # Realised volatility tracking error
    trackerror_vol <- mean(rowMeans(abs(prop_xts - 1/ncol(prop_xts)))) * 100
    
    # Calculate mean tracking error on MRC
    trackerror_mrc <- mean(rowMeans(abs(mrc_xts - 100/ncol(mrc_xts))))
    
    # Tradesize
    mean_tradesize <- mean(rowSums(ts_xts))
    
    # Performance
    portfolio_returns <- xts(rowSums(scaleret), order.by=index(scaleret))
    # Subtract the cost of leverage if appropriate
    if (margin_cost != 0) {
      # Look at cost of leverage
      port_weights <- xts(rowSums(dailyweights), order.by = index(dailyweights))
      margin <- port_weights - 1
      marginreturn <- margin * (margin_cost / 360) # Estimating margin cost at annualised % / 360
      portfolio_returns <- portfolio_returns - marginreturn
    }
    
    t <- table.AnnualizedReturns(portfolio_returns)
    cagr <- t[,1][1] * 100
    sharpe <- t[,1][3]
    perf <- data.frame(trackerror_vol, trackerror_mrc, mean_tradesize, cagr, sharpe)
    
    if (plot == TRUE) {
      # Realised volatility
      #print(plot(vol_xts, main = 'Realised Volatility', legend.loc='topleft'))
      #vol_df <- xts_to_tidy_df(vol_xts)
      #print(ggplot(vol_df, aes(x=Date, y=Value, fill=Asset)) + geom_area() + ggtitle('Realised Volatility'))
      
      # Realised volatility %
      #print(plot(prop_xts, main = 'Realised Volatility %', legend.loc='topleft'))
      #prop_df <- xts_to_tidy_df(prop_xts)
      #print(ggplot(prop_df, aes(x=Date, y=Value, fill=Asset)) + geom_area() + ggtitle('Realised Volatility as Proportion of Total'))
      # Calculate mean tracking error (as %age)
      trackerror_vol <- mean(rowMeans(abs(prop_xts - 1/ncol(prop_xts)))) * 100
      
      #Risk contribution
      #print(plot(mrc_xts, main = 'Risk Contribution', legend.loc='topleft'))
      #mrc_df <- xts_to_tidy_df(mrc_xts)
      #print(ggplot(mrc_df, aes(x=Date, y=Value, fill=Asset)) + geom_area() + ggtitle('Risk Contribution'))
      # Calculate mean tracking error on MRC
      trackerror_mrc <- mean(rowMeans(abs(mrc_xts - 100/ncol(mrc_xts))))
      
      # Tradesize
      #print(plot(ts_xts, main='Trade Size', legend.loc='topleft'))
      #tradesize_df <- xts_to_tidy_df(ts_xts)
      #print(ggplot(tradesize_df, aes(x=Date, y=Value, fill=Asset)) + geom_area() + ggtitle('Rebalance Trades'))
      #mean_tradesize <- mean(rowSums(ts_xts))
      
      # Weights
      #print(plot(weights, main='Asset weights', legend.loc='topleft'))
      #weights_df <- xts_to_tidy_df(weights)
      #print(ggplot(weights_df, aes(x=Date, y=Value, fill=Asset)) + geom_area() + ggtitle('Asset Weights'))
      
      # Performance
      #print(table.DownsideRisk(portfolio_returns))
      #print(table.Drawdowns(portfolio_returns))
      #charts.PerformanceSummary(portfolio_returns, main = 'Portfolio Returns')
      #print(perf[,c('cagr','sharpe')])
    }
    return(portfolio_returns)
  }
  
  
  riskperformance_plot_only <- function(ret, weights, samplefreq = 'quarters', plot = TRUE, margin_cost = 0) {
    # ret <- returns_daily[,-5]
    # weights <- adjweights
    dailyweights <- xts(matrix(NA, nrow(ret), ncol(ret)), order.by = index(ret))
    dailyweights[index(weights),] <- coredata(weights)
    dailyweights <- na.omit(na.locf(dailyweights))
    
    # Now scale the returns by the asset weights
    scaleret <- xts::lag.xts(dailyweights,1) * ret
    colnames(scaleret) <- colnames(ret)
    
    # Get trades as percentage of portfolio
    tradesize <- na.omit(abs(dailyweights - xts::lag.xts(dailyweights)))
    totaltrade <- rowSums(tradesize)
    
    # Get monthly endpoints
    monthend <- endpoints(scaleret, on = samplefreq)[-1] # remove the first zero from the endpoints vector
    medt <- index(scaleret[monthend])
    
    # Create xts objects to hold volatilities and risk contributions
    vol_xts <- xts(matrix(NA, length(medt), ncol(ret)), order.by = index(scaleret[medt]))
    mrc_xts <- xts(matrix(NA, length(medt), ncol(ret)), order.by = index(scaleret[medt]))
    ts_xts <- xts(matrix(NA, length(medt), ncol(ret)), order.by = index(scaleret[medt]))
    colnames(vol_xts) <- colnames(mrc_xts) <- colnames(ts_xts) <- colnames(ret)
    
    i <- medt[1]
    
    require(FRAPO)
    
    for (n in 2:length(medt)) {
      j <- medt[n]
      r <- scaleret[paste0(i,'::',j)] # Get returns over window
      # Volatility
      vols <- StdDev.annualized(r) # Estimate volatility of each asset
      vol_xts[i,] <- vols
      # MRCD
      mrc <- FRAPO::mrc(rep(1, ncol(scaleret)), cov(r)) # Estimate risk contribution of each asset (setting weights to zero)
      mrc_xts[i,] <- mrc
      # Trades
      ts <- colSums(tradesize[paste0(i,'::',j)])
      ts_xts[i,] <- ts
      i <- j
    }
    
    
    unloadNamespace("FRAPO")
    vol_xts <- na.omit(vol_xts)
    prop_xts <- vol_xts / rowSums(vol_xts)
    mrc_xts <- na.omit(mrc_xts)
    ts_xts <- na.omit(ts_xts)
    
    
    # Realised volatility tracking error
    trackerror_vol <- mean(rowMeans(abs(prop_xts - 1/ncol(prop_xts)))) * 100
    
    # Calculate mean tracking error on MRC
    trackerror_mrc <- mean(rowMeans(abs(mrc_xts - 100/ncol(mrc_xts))))
    
    # Tradesize
    mean_tradesize <- mean(rowSums(ts_xts))
    
    # Performance
    portfolio_returns <- xts(rowSums(scaleret), order.by=index(scaleret))
    # Subtract the cost of leverage if appropriate
    if (margin_cost != 0) {
      # Look at cost of leverage
      port_weights <- xts(rowSums(dailyweights), order.by = index(dailyweights))
      margin <- port_weights - 1
      marginreturn <- margin * (margin_cost / 360) # Estimating margin cost at annualised % / 360
      portfolio_returns <- portfolio_returns - marginreturn
    }
    
    t <- table.AnnualizedReturns(portfolio_returns)
    cagr <- t[,1][1] * 100
    sharpe <- t[,1][3]
    perf <- data.frame(trackerror_vol, trackerror_mrc, mean_tradesize, cagr, sharpe)
    
    if (plot == TRUE) {
      # Realised volatility
      #print(plot(vol_xts, main = 'Realised Volatility', legend.loc='topleft'))
      #vol_df <- xts_to_tidy_df(vol_xts)
      #print(ggplot(vol_df, aes(x=Date, y=Value, fill=Asset)) + geom_area() + ggtitle('Realised Volatility'))
      
      # Realised volatility %
      #print(plot(prop_xts, main = 'Realised Volatility %', legend.loc='topleft'))
      prop_df <- xts_to_tidy_df(prop_xts)
      #print(ggplot(prop_df, aes(x=Date, y=Value, fill=Asset)) + geom_area() + ggtitle('Realised Volatility as Proportion of Total'))
      # Calculate mean tracking error (as %age)
      trackerror_vol <- mean(rowMeans(abs(prop_xts - 1/ncol(prop_xts)))) * 100
      
      #Risk contribution
      #print(plot(mrc_xts, main = 'Risk Contribution', legend.loc='topleft'))
      #mrc_df <- xts_to_tidy_df(mrc_xts)
      #print(ggplot(mrc_df, aes(x=Date, y=Value, fill=Asset)) + geom_area() + ggtitle('Risk Contribution'))
      # Calculate mean tracking error on MRC
      trackerror_mrc <- mean(rowMeans(abs(mrc_xts - 100/ncol(mrc_xts))))
      
      # Tradesize
      #print(plot(ts_xts, main='Trade Size', legend.loc='topleft'))
      #tradesize_df <- xts_to_tidy_df(ts_xts)
      #print(ggplot(tradesize_df, aes(x=Date, y=Value, fill=Asset)) + geom_area() + ggtitle('Rebalance Trades'))
      #mean_tradesize <- mean(rowSums(ts_xts))
      
      # Weights
      #print(plot(weights, main='Asset weights', legend.loc='topleft'))
      weights_df <- xts_to_tidy_df(weights)
      #print(ggplot(weights_df, aes(x=Date, y=Value, fill=Asset)) + geom_area() + ggtitle('Asset Weights'))
      
      # Performance
      #print(table.DownsideRisk(portfolio_returns))
      #print(table.Drawdowns(portfolio_returns))
      charts.PerformanceSummary_edit(portfolio_returns, main = 'Portfolio Returns')
      #print(perf[,c('cagr','sharpe')])
    }
    return(perf[,c('cagr','sharpe')])
  }
}

# read csv from googledrive function
read_csv_from_googledrive <- function(id, base_url="https://drive.google.com/open?id=1PeDaQeVoYc9bfpDJDsCc2ogme27sCpY4") {
  full_url = paste0(base_url, id)
  df =  read.csv(url(full_url), header=TRUE)
  
  return(df)
}

# read csv from filesystem function
read_csv_from_filesystem <- function(id, base_url="../data/") {
  full_url = paste0(base_url, id)
  df =  read.csv(full_url, header=TRUE)
  
  return(df)
}


# edit performance summary chart such that returns are not shown

charts.PerformanceSummary_edit <- function (R, Rf = 0, main = NULL, geometric = TRUE, methods = "none", 
                                            width = 0, event.labels = NULL, ylog = FALSE, wealth.index = FALSE, 
                                            gap = 12, begin = c("first", "axis"), legend.loc = "topleft", 
                                            p = 0.95, ...) {
  begin = begin[1]
  x = checkData(R)
  colnames = colnames(x)
  ncols = ncol(x)
  length.column.one = length(x[, 1])
  start.row = 1
  start.index = 0
  while (is.na(x[start.row, 1])) {
    start.row = start.row + 1
  }
  x = x[start.row:length.column.one, ]
  if (ncols > 1) 
    legend.loc = legend.loc
  else legend.loc = NULL
  if (is.null(main)) 
    main = paste(colnames[1], "Performance", sep = " ")
  if (ylog) 
    wealth.index = TRUE
  op <- par(no.readonly = TRUE,xpd=TRUE)
  par(oma = c(2, 0, 4, 0), mar = c(1, 4, 4, 2))
  plot_object <- chart.CumReturns(x, main = "Cumulative Return", 
                                  xaxis = FALSE, legend.loc = legend.loc, event.labels = event.labels, 
                                  ylog = ylog, wealth.index = wealth.index, begin = begin, 
                                  geometric = geometric, ylab = "Cumulative Return", ...)
  par(mar = c(1, 4, 0, 2))
  freq = periodicity(x)
  switch(freq$scale, seconds = {
    date.label = "Second"
  }, minute = {
    date.label = "Minute"
  }, hourly = {
    date.label = "Hourly"
  }, daily = {
    date.label = "Daily"
  }, weekly = {
    date.label = "Weekly"
  }, monthly = {
    date.label = "Monthly"
  }, quarterly = {
    date.label = "Quarterly"
  }, yearly = {
    date.label = "Annual"
  })
  
  par(mar = c(5, 4, 0, 2))
  plot_object <- chart.Drawdown(x, geometric = geometric, main = "Drawdown", 
                                ylab = "Drawdown", event.labels = NULL, ylog = FALSE, 
                                add = TRUE, ...)
  print(plot_object)
  title(main, outer = TRUE)
  par(op)
  
}

#############################################
# Shiny Code
#############################################
# Specify ETF basket 
symbols_RP <- c('CSUS.AS',"EMIM.AS","VEVE.AS","BTMA.AS","IS04.XETRA","IUS7.XETRA","IWDP.AS",'GBTC')

# get data from google drive
# this is setup such that the first id is equal to the first entry in symbols_RP, note that you 

google_sheets_ids <- as.data.frame(t(c("1XbAH0dShSXtpK5nWDl6MbZBsiMjK-s9T", 
                                       "0B0pSzrqwgkX9bXYtaWRTTHh2cnF1Ujh5VlBnbi15cVFObVVF",
                                       "0B0pSzrqwgkX9a1NBWW9meVFWV0VFZEJfcEVXb0locWdWOGpr",
                                       "0B0pSzrqwgkX9UWVRS29rZkdhTjR2bDFySkZuTW1XZXdrMTlN",
                                       "0B0pSzrqwgkX9OHRyd3pTNkpMQzROdmpSbjNzSDJzOG92XzlV",
                                       "1v0PmPU05H3kqLYWlFdVDzU7Ed7diXnIf",
                                       "0B0pSzrqwgkX9T1Q2YUVSVWRYRUpORVVFMnRsVzV2QmdoWHlr",
                                       "1rSSQgFzbnYn9JD6ZxHWBM06YzHWn9DZB")))
google_risk_premia_data <- list()

# convert data to xts per symbol
for (i in 1:length(symbols_RP)) {
  
  #google_risk_premia_data[[i]] <- read_csv_from_googledrive(paste(google_sheets_ids[,i]))
  google_risk_premia_data[[i]] <- read_csv_from_filesystem(paste(google_sheets_ids[,i]))

  price_data = na.omit(google_risk_premia_data[[i]])
  
  dates <- price_data$Date
  
  #use adjusted column
  price_data <- as.data.frame(price_data[,"Adjusted_close"])
  rownames(price_data) <- dates
  
  
  price_data <- as.xts(price_data,order.by = as.Date(dates))
  colnames(price_data)[1] <- 'price.adjusted.close'  
  
  
  assign(symbols_RP[i],NA)
  assign(paste0(symbols_RP[i]),price_data)
  
}

# Modify specific ticker data in this section (manually remove outliers for instance)
#SEMB.LSE["2018-07-02"] <- NA
#SEMB.LSE <- SEMB.LSE["2011-01-01/"]
BTMA.AS["2019-02-01"] <- NA

# remove any zero's in the price data because this will mess up the return calculation
for (i in 1:length(symbols_RP)) {
  ticker <- get(symbols_RP[i])
  ticker <- ifelse(ticker$price.adjusted.close < 0.000001,NA,ticker )
  assign(paste0(symbols_RP[i]),na.omit(ticker))
}



for (i in 1:length(symbols_RP)) {
  
  ticker <- get(symbols_RP[i])
  returns <- na.omit(Return.calculate((ticker), method = 'discrete'))
  
  returns[is.infinite(returns),] <- NA
  
  assign(paste0('rets.',symbols_RP[i]),na.omit(returns ))
  
  
  rm(ticker)
  
  
}

ret_symbols <- NA

for (i in 1:length(symbols_RP)) {
  
  
  ret_symbols[i] <- paste0('rets.',symbols_RP[i])
  
}

returns_list <- lapply(ret_symbols, get)

merged_returns <- do.call(cbind,returns_list)

names(merged_returns) <- symbols_RP

returns_daily <- merged_returns

returns_daily[returns_daily == 0] <- NA
returns_daily[is.na(returns_daily)] <- 0

#merge prices

prices_list <- lapply(symbols_RP,get)
merged_prices <- do.call(cbind,prices_list)
names(merged_prices) <- symbols_RP

EU_etf_returns <- merged_returns

EU_etf_returns[EU_etf_returns == 0] <- NA
EU_etf_returns[is.na(EU_etf_returns)] <- 0

names(EU_etf_returns) <- symbols_RP

#######

unl_etf_weights_EU <- get_unlevered_target_weights(na.omit(returns_daily[,names(returns_daily) %in% symbols_RP]), rebal = 22, vol_lookback = 90, cor_lookback = 120, cor_shrinkage = 1, adjust_momo = TRUE, momo_multiplier = 0.1)

latest_unl_weights <- round(unl_etf_weights_EU,3)[nrow(unl_etf_weights_EU),]
capped_unl_weights <- round(get_cap_allocation_and_rescale(latest_unl_weights,paste(symbols_RP[6]),cap=0.15,leverage=1),3)

returns_backtest_EU <- returns_backtest(returns_daily[,names(returns_daily) %in% symbols_RP], weights= unl_etf_weights_EU)
names(returns_backtest_EU) <- 'backtest EU'

backtest_vs_portfolio_EU <- merge(returns_backtest_EU,returns_daily[,names(returns_daily) %in% symbols_RP])

}

###########
# RUN SECOND
##########

# run app
{
  
ui <- shinyUI(fluidPage(
  tabsetPanel(
    tabPanel('Analysis',fluid = TRUE,
  titlePanel("Risk Premia Shiny analysis"),
  
  sidebarPanel(
    helpText("With this application you can visualize the behavior of a set of stocks in a period of time, from the point of view of prices or cumulative return"),
    br(),
    tags$b(""),
    
    dateRangeInput("dates", 
                   label = h2("Select the period of your analysis"),
                   start = "2018-01-01", end = "2019-02-31"),

    selectInput("plottype", label = h2("Select the type of plot"), c("Prices", "Cumulative Return")),

    selectInput(
      'select',
      label = h2("View price chart of stock"),
      choices = symbols_RP
    ),
    helpText("")
  ), 
  
  mainPanel(
    
    # Output: Tabset w/ plot, summary, and table ----
    tabsetPanel(type = "tabs",
                tabPanel("Stock data", plotOutput("plot1")),
                tabPanel("Strategy returns", plotOutput("plot2"))
    )
    
    
  )
    ), 
  
  tabPanel('Order calculator',fluid = TRUE,
           titlePanel("Order Calculator"),
           
           sidebarPanel(
             helpText("With this application you can calculate the necessary orders based on current positions and the rebalancing outcome of the strategy"),
             br(),
             tags$b(""),
            
             numericInput('curCap', "Current capital in Euro to invest ", 0, min = NA, max = NA, step = NA,
                          width = NULL),
             
             numericInput('curPos1', paste0("Current Position in ",symbols_RP[1]), 0, min = NA, max = NA, step = NA,
                          width = NULL),
             numericInput('curPos2', paste0("Current Position in ",symbols_RP[2]), 0, min = NA, max = NA, step = NA,
                          width = NULL),
             numericInput('curPos3', paste0("Current Position in ",symbols_RP[3]), 0, min = NA, max = NA, step = NA,
                          width = NULL),
             numericInput('curPos4', paste0("Current Position in ",symbols_RP[4]), 0, min = NA, max = NA, step = NA,
                          width = NULL),
             numericInput('curPos5', paste0("Current Position in ",symbols_RP[5]), 0, min = NA, max = NA, step = NA,
                          width = NULL),
             numericInput('curPos6', paste0("Current Position in ",symbols_RP[6]), 0, min = NA, max = NA, step = NA,
                          width = NULL),
             numericInput('curPos7', paste0("Current Position in ",symbols_RP[7]), 0, min = NA, max = NA, step = NA,
                          width = NULL),
             numericInput('curPos8', paste0("Current Position in ",symbols_RP[8]), 0, min = NA, max = NA, step = NA,
                          width = NULL),
             
             helpText("....")
           ), 
           
           mainPanel(     
             
             h3('Portfolio weights'),
             h5('Note that IUS7.XETRA is capped at max 15%'),
             plotOutput('weights'),
             
             h3('Current portfolio value in Euro:'),
             textOutput("portfval"),
             
             h3('Latest prices'),
             dataTableOutput('latestprices'),
             
             h3('Current positions'),
             dataTableOutput('currPos'),
             
             h3('Position values in Euro'),
             dataTableOutput("portfVal"),

             h3('Target positions'),
             dataTableOutput("targetpositions"),
             
             h3('Delta positions'),
             dataTableOutput("deltapositions")


          )
      ) 

    )
  )
)


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output,local=TRUE) {
  

  
  
  #######
  #### load packages and necessary functions in Rshiny environment
  {
    #check loaded packages
    (.packages())
    
    #unload any packages
    #lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)
    require(DT)
    library(PerformanceAnalytics)
    require(ggplot2)
    library(quantmod)
    library(tidyr)
    library(xts)
    library(BatchGetSymbols)
    require(googledrive)
    require(shiny)
    require(reshape)
    
    Sys.setenv(TZ="UTC") 
    
    
    
  }
  #######

  #### server
  
  {

   
stocks <-   reactive({
    
    if (input$plottype == 'Cumulative Return') {
      
      
    returns_daily[paste(as.Date(input$dates[1]), as.Date(input$dates[2]), sep="::"), names(returns_daily) %in% symbols_RP[-8] ]
      

  } else if (input$plottype == 'Prices') {
    
    merged_prices[paste(as.Date(input$dates[1]), as.Date(input$dates[2]), sep="::"),names(merged_prices) == input$select]
    
  }  
    
})
  
strategyData <- reactive({
  
 
   
    backtest_vs_portfolio_EU <- na.omit(backtest_vs_portfolio_EU[paste(as.Date(input$dates[1]), as.Date(input$dates[2]), sep="::"),-9])
    
})
  
output$plot1 <-   renderPlot({
      
      if (input$plottype == 'Cumulative Return') {
      
        charts.PerformanceSummary_edit(stocks(), main = 'Performance Summary of ETF basket without GBTC plot',legend.loc='topleft',ncol=4)
      } else if (input$plottype == 'Prices') {
        
        chart_Series(stocks(),name=input$select)
        
      }
        
    })

output$plot2 <- renderPlot({

    charts.PerformanceSummary_edit(strategyData(),main = "Backtest returns vs portfolio constituents without GBTC",legend.loc='topleft',ncol=4) 
    
 
    })
  
output$currPos <- renderDataTable({
    
    
    curr_pos <- as.data.frame(t(c(input$curPos1  ,input$curPos2  ,input$curPos3  ,input$curPos4  ,input$curPos5  ,input$curPos6  ,input$curPos7,input$curPos8 )))
    
    colnames(curr_pos ) <- symbols_RP
    rownames(curr_pos ) <- Sys.Date()
    curr_pos
    })
  
output$portfVal <- renderDataTable({
    
    
    curr_pos <- as.data.frame(t(c(input$curPos1  ,input$curPos2  ,input$curPos3  ,input$curPos4  ,input$curPos5  ,input$curPos6  ,input$curPos7,input$curPos8   )))
    
    colnames(curr_pos ) <- symbols_RP
    rownames(curr_pos ) <- Sys.Date()
    
    latest_price <- tail(na.omit(tail(merged_prices[,names(merged_prices) %in% symbols_RP],10)),1)
    
    portfVal <- curr_pos * as.data.frame(latest_price)
    
    colnames(portfVal) <- colnames(curr_pos)
    rownames(portfVal) <- index(latest_price)
    portfVal})
    
output$targetpositions <- renderDataTable({
    
    
    curr_pos <- as.data.frame(t(c(input$curPos1  ,input$curPos2  ,input$curPos3  ,input$curPos4  ,input$curPos5  ,input$curPos6  ,input$curPos7,input$curPos8  )))
    
    colnames(curr_pos ) <- symbols_RP
    rownames(curr_pos ) <- "Current Positions"
    
    latest_price <- tail(na.omit(tail(merged_prices[,names(merged_prices) %in% symbols_RP],10)),1)
    
    portfVal <- curr_pos * as.data.frame(latest_price)
    
    colnames(portfVal) <- colnames(curr_pos)
    rownames(portfVal) <- index(latest_price)
    
    port_cap <- input$curCap 
    
    capital <- sum(portfVal) + input$curCap
    
    target_positions <- round(as.numeric(capital) * capped_unl_weights / as.numeric(latest_price) ,1 ) 
    
    target_positions <- as.data.frame(target_positions)
    rownames(target_positions) <- Sys.Date()
    
    
    datatable(target_positions)
    
  })
  
output$deltapositions <- renderDataTable({
    

    curr_pos <- as.data.frame(t(c(input$curPos1  ,input$curPos2  ,input$curPos3  ,input$curPos4  ,input$curPos5  ,input$curPos6  ,input$curPos7,input$curPos8  )))
    
    colnames(curr_pos ) <- symbols_RP
    rownames(curr_pos ) <- "Current Positions"
    
    latest_price <- tail(na.omit(tail(merged_prices[,names(merged_prices) %in% symbols_RP],10)),1)
    
    portfVal <- curr_pos * as.data.frame(latest_price)
    
    colnames(portfVal) <- colnames(curr_pos)
    rownames(portfVal) <- index(latest_price)
    
    port_cap <- input$curCap 
    
    capital <- sum(portfVal) + input$curCap
    
    target_positions <- round(as.numeric(capital) * capped_unl_weights / as.numeric(latest_price) ,0 ) 
    
    delta_pos <- NA
    
    for (i in 1:length(target_positions)) {
      
      rebalance <- target_positions[1,i] - curr_pos[1,i]
      
      if ( abs(rebalance)  <= 0.01) {
        
        delta_pos[i] <- 0
        
      } else {
        
        delta_pos[i] <- rebalance
        
      }
      
    }
    
    delta_pos <- as.data.frame(t(delta_pos))
    colnames(delta_pos) <- symbols_RP
    rownames(delta_pos) <- Sys.Date()
    
    datatable(delta_pos)
    
     })
  
output$latestprices <- renderDataTable({
    
                  latest_price <-   as.data.frame(tail(na.omit(tail(merged_prices[,names(merged_prices) %in% symbols_RP],10)),1))
                  datatable(latest_price)
                  
              })
  
output$portfval <-  renderText({
    
    curr_pos <- as.data.frame(t(c(input$curPos1  ,input$curPos2  ,input$curPos3  ,input$curPos4  ,input$curPos5  ,input$curPos6  ,input$curPos7,input$curPos8  )))
  
  colnames(curr_pos ) <- symbols_RP
  rownames(curr_pos ) <- "Current Positions"
  
  latest_price <- tail(na.omit(tail(merged_prices[,names(merged_prices) %in% symbols_RP],10)),1)
  
  portfVal <- curr_pos * as.data.frame(latest_price)
  
  colnames(portfVal) <- colnames(curr_pos)
  rownames(portfVal) <- index(latest_price)
  
  port_cap <- input$curCap 
  
  capital <- sum(portfVal) + input$curCap })
        
output$weights <- renderPlot({
    
  
  plot_weights <- melt(as.data.frame(capped_unl_weights))
  
  ggplot(data = plot_weights, aes(x = variable,y=100*value)) +
    geom_col() +
    coord_flip() + 
    geom_text(aes(label=paste(value*100,'%')), position=position_dodge(width=0.5), vjust=-0.5) + xlab('') + ylab('')  + coord_cartesian(ylim = c(0, 30)) + ggtitle(paste('Risk Premia EU Weights on: ',time(capped_unl_weights)))
   })
  
}

}) 

# Run the application 
shinyApp(ui = ui, server = server)
}


