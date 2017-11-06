## STXFIN
#Look at the top 15 Financial companies index by market cap

#Moving Average Code
#Research project BUS4053H
#Ashley Gengiah
#GNGASH001
install.packages("xtable")
install.packages("TTR")
install.packages("quantmod")
install.packages("PerformanceAnalytics")
install.packages("fBasics")
library(quantmod)
library(xtable)
library(PerformanceAnalytics)
library(TTR)
library(fBasics)
#JSE FIN

getSymbols('STXFIN',src='google')
FIN <- window(STXFIN['2006-05-01::2014-04-31'])

ema_ret <- function(FIN, short, long, cost = 0.00855659){
  ema_short <- EMA(Cl(FIN),n=short)    # 126 days, 6 monthly
  ema_long <- EMA(Cl(FIN),n=long)  # 252 days, yearly
  
  #generate our trading signals over the period
  
  # 1 indicates buy
  # 0 indicates do nothing
  #-1 indicates sell
  
  ema_sig <<-
    Lag(ifelse(
      Lag(ema_short) < Lag(ema_long) &
        ema_short > ema_long,
      1,
      ifelse(Lag(ema_short) > Lag(ema_long) & ema_short < ema_long, -1, 0)
    ))
  ema_sig[is.na(ema_sig)] <- 0
  
  # use the signals generated to calculate the holding periods
  ema_strat <- ifelse(ema_sig > 1, 0, 1)
  for (i in 1:length(Cl(FIN))) {
    ema_strat[i] <-
      ifelse(ema_sig[i] == 1, 1, ifelse(ema_sig[i] == -1, 0, ema_strat[i - 1]))
  }
  ema_strat[is.na(ema_strat)] <- 1
  
  # evaluate the performance of the moving average against the buy and hold
  ret <- ROC(Cl(FIN))
  ret[1] <- 0 # remove the NA for the first entry
  bhstrat <- ret #buy and hold
  
  
  ema_perf <-
    ifelse((ema_sig == 1 |
              ema_sig == -1) &
             ema_strat != Lag(ema_strat),
           (ret - cost) * ema_strat,
           ret * ema_strat
    )
  
  performance_table <- cbind(ema_perf, bhstrat)
  colnames(performance_table) <- c("ema","Buy & Hold")
  ret_table <<- table.AnnualizedReturns(performance_table)
  ret_set <- data.frame(
    short = short,
    long = long,
    matrix(ret_table$`ema`, ncol = 3), 
    matrix(ret_table$`Buy & Hold`, ncol = 3), 
    trades_buy = sum(ema_sig == 1),
    trades_sell = sum(ema_sig == -1))
  
  colnames(ret_set) <- c("short", 
                         "long", 
                         "strat_r",
                         "strat_sd",
                         "strat_sharpe",
                         "bh_r",
                         "bh_sd",
                         "bh_sharpe",
                         "buy",
                         "sell")
  
  return(ret_set)
  
}

# Adjust for more short/long strats
strats <- expand.grid(short = seq(5, 55, 25), long = seq(55, 405, 50))

# Optimisation
strat_res <- list()
for(i in 1:nrow(strats)){
  cat("Running strat: short -", strats[i,'short'], "long -", strats[i,'long'], "complete:",round(i/nrow(strats), 2)*100,"%\n")
  
  strat_res[[i]] <- ema_ret(FIN, strats[i,'short'], strats[i,'long'])
}

strat_all <- do.call(rbind, strat_res)

library(dplyr)
strat_all %>% 
  arrange(desc(strat_r)) %>% 
  filter(buy != 0) %>% 
  head

strat_all->emafin1
save(emafin1, file= "emafin1.Rdata")
