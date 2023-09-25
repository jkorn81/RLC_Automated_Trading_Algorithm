# Create Training Data for Various Zigzag Signals (-1,0,1) ----
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
# - [-] - Set Working Directory ----
setwd("C:/Users/jonat/Desktop/rl_mps_secure")
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
# - [-] - Required Packages ----
source("scripts/libs.r")
use_python("C:/Users/jonat/Anaconda3/python.exe")
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
original = read_delim("rl_agent_crowder/data/df_rates_p2.csv", escape_double = FALSE, 
                      trim_ws = TRUE, delim=",")
original = data.frame(original)
colnames(original) = c("Account","Balance","Position",'Date', 'Open', 'High', 'Low', 'Close')
balance = last(original$Account,1)
equity = last(original$Balance,1)
percent_change = round((equity-balance)/balance,2)
write.csv(percent_change,"rl_agent_crowder/outputs/pc.csv", row.names = FALSE)
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
source("scripts/daily.r")
#source_python("rl_agent_crowder/entry_indicator.py")
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
source_python("rl_agent_crowder/main.py")
setwd("C:/Users/jonat/Desktop/rl_mps_secure")
source("scripts/last_date.r")
pivot_list <- list.files(path = "rl_agent_crowder/outputs/pivots/.", recursive = TRUE, 
                         pattern = "\\.xlsx$", 
                         full.names = TRUE)
if(length(pivot_list) == 0){
  source_python("rl_agent_crowder/pivots.py")
} else {
  n_last = 4
  time = substr(lastdate, nchar(lastdate) - n_last + 1, nchar(lastdate))
  if(time == "2345"){
    source_python("rl_agent_crowder/pivots.py")
  } else {
    print("not time to re-run pivot points until 23:45pm, use existing.")
  }
}
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
setwd("C:/Users/jonat/Desktop/rl_mps_secure")
source("scripts/current_price.r")
source("scripts/pivot_rules.r")
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
state_list_neg <- list.files(path = "rl_agent_crowder/deploy_outputs/neg_theta/.", recursive = TRUE, 
                             pattern = "\\.csv$", 
                             full.names = TRUE)
if(length(state_list_neg) > 0){
  pos_negative <- read_csv("rl_agent_crowder/deploy_outputs/pos_negative.csv")
  if(nrow(pos_negative) != 0){
    pos_negative = c(pos_negative$`0`)
    pos_negative = pos_negative[pos_negative <= -0.01]
  } else if(nrow(pos_negative) == 0){
    pos_negative = c(NULL)
  } 
  
  if(length(pos_negative) != 0){
    pos_negative_count = length(pos_negative)
  } else if(length(pos_negative) == 0){
    pos_negative_count = 0
  }
  print(paste0("Negative ACM RL States ",toString(pos_negative_count)))
} else if(length(state_list_neg) == 0){
  pos_negative_count = 0
  pos_negative = c(0)
  print(paste0("No Negative RL States."))
}
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
state_list_pos <- list.files(path = "rl_agent_crowder/deploy_outputs/pos_theta/.", recursive = TRUE, 
                             pattern = "\\.csv$", 
                             full.names = TRUE)
if(length(state_list_pos) > 0){
  pos_positive <- read_csv("rl_agent_crowder/deploy_outputs/pos_positive.csv")
  if(nrow(pos_positive) != 0){
    pos_positive = c(pos_positive$`0`)
    pos_positive = pos_positive[pos_positive >= 0.01]
  } else if(nrow(pos_positive) == 0){
    pos_positive = c(NULL)
  }
  
  if(length(pos_positive) != 0){
    pos_positive_count = length(pos_positive)
  } else if(length(pos_positive) == 0){
    pos_positive_count = 0
  }
  print(paste0("Positive ACM RL States ",toString(pos_positive_count)))
} else if(length(state_list_pos) == 0){
  pos_positive_count = 0
  pos_positive = c(0)
  print(paste0("No Positive RL States."))
}
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
if(pos_positive_count > pos_negative_count){
  trade_m15 = 1
  print("rl pos count > rl neg count, open buy")
} else if(pos_positive_count < pos_negative_count){
  trade_m15 = 4
  print("rl neg count > rl pos count, open sell")
} else if(pos_positive_count == pos_negative_count){
  trade_m15 = 0
  print("rl counts even, hold.")
}
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#Store Results ----
o_trade_m15 = trade_m15
source("scripts/last_date.r")
print(paste0("montra:: trust the rl trading system..."))
print(lastdate)
print(paste0("original trade_m15 ", trade_m15))
source("scripts/close_price.r")
results_E5ZZ_m15 = last(data.frame(close,trade_m15),1)
#######################################################################################
#######################################################################################
#######################################################################################
trades <- list.files(path = "trades/.", recursive = TRUE, 
                     pattern = "\\.csv$", 
                     full.names = TRUE)
n_last <- 5                               
ext = substr(trades, nchar(trades) - n_last + 1, nchar(trades)) 
require(stringr)
require(data.table)
ext = str_remove(ext,".csv")
trade_types <- list.files(path = "outputs/trade_type/.", recursive = TRUE, 
                          pattern = "\\.csv$", 
                          full.names = TRUE)
if(length(trade_types) == 1){
  trade_type <- read_csv("outputs/trade_type/trade_type.csv")
}
# Remove to be able to accept trades apposing the RL states
if(length(ext) >= 1){
  if(last(ext,1) == 1 & trade_m15 == 4){
    trade_m15 = 98
    print("existing buys with reverse rl sell signal, so close buys.")
  }
  if(last(ext,1) == 4 & trade_m15 == 1){
    trade_m15 = 99
    print("existing sells with reverse rl buy signal, so close sells.")
  }
}
if(length(ext) >= 1){
  if(last(ext,1) == 1 & pos_positive_count == 0 & pos_negative_count == 0){
    trade_m15 = 98
    print("existing buys with no buy/sell signal, so close buys.")
  }
  if(last(ext,1) == 1 & pos_positive_count == pos_negative_count){
    trade_m15 = 98
    print("existing buys with even buy/sell signal, so close buys.")
  }
  if(last(ext,1) == 4 & pos_negative_count == 0 & pos_positive_count == 0){
    trade_m15 = 99
    print("existing sells with no sell/buy signal, so close sells.")
  }
  if(last(ext,1) == 4 & pos_positive_count == pos_negative_count){
    trade_m15 = 99
    print("existing sells with even sell/buy signal, so close sells.")
  }
}
#######################################################################################
#######################################################################################
#######################################################################################
#If Positions 0 remove Trade Files ----
source("scripts/position_count.r")
if(positions == 0 & length(trades)>=1){
  print("ea closed position/s so remove trade file/s.")
  if(last(ext,1) == 1){
    trade_m15 == 98
  }
  if(last(ext,1) == 4){
    trade_m15 == 99
  }
}
#######################################################################################
#######################################################################################
#######################################################################################
print(paste0("the current price is ", current_cp))
print(paste0("the previous price is ", prev_cp))
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
source("scripts/rsqr.r")
cl = makeCluster(detectCores() - 1)
registerDoParallel(cl)

files <- list.files(path = "scripts/bsts/.", recursive = TRUE,
                    pattern = "\\.r$", 
                    full.names = TRUE)

foreach(i = 1:length(files)) %dopar%
  {
    source(files[i])
  }
stopCluster(cl)
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
bsts1 <- read_csv("outputs/bsts_data/bsts_data1.csv")
bsts2  <- read_csv("outputs/bsts_data/bsts_data2.csv")
bsts3  <- read_csv("outputs/bsts_data/bsts_data3.csv")
bsts4  <- read_csv("outputs/bsts_data/bsts_data4.csv")
bsts5  <- read_csv("outputs/bsts_data/bsts_data5.csv")
bsts1 = bsts1$x
bsts2 = bsts2$x
bsts3 = bsts3$x
bsts4 = bsts4$x
bsts5 = bsts5$x
if(bsts1 > current_cp){
  pred.sig1 = 2
} else {
  pred.sig1 = 1
}
if(bsts2 > current_cp){
  pred.sig2 = 2
} else {
  pred.sig2 = 1
}
if(bsts3 > current_cp){
  pred.sig3 = 2
} else {
  pred.sig3 = 1
}
if(bsts4 > current_cp){
  pred.sig4 = 2
} else {
  pred.sig4 = 1
}
if(bsts5 > current_cp){
  pred.sig5 = 2
} else {
  pred.sig5 = 1
}
sum = sum(c(pred.sig1,pred.sig2,pred.sig3,pred.sig4,pred.sig5))
correction = ifelse(sum == 10, 1,
                    ifelse(sum == 9, 1,
                           ifelse(sum == 8, 1,
                                  ifelse(sum == 7, -1,
                                         ifelse(sum == 6, -1,
                                                ifelse(sum == 5, -1,
                                                       0))))))
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
library(doParallel)
cl = makeCluster(detectCores() - 1)
registerDoParallel(cl)

files <- list.files(path = "scripts/deploy/.", recursive = TRUE,
                    pattern = "\\.r$", 
                    full.names = TRUE)

foreach(i = 1:length(files)) %dopar%
  {
    source(files[i])
  }

stopCluster(cl)
library(readr)
df_total <- read_csv("outputs/classifier_data/df_total.csv")
df_total2 <- read_csv("outputs/classifier_data/df_total2.csv")
df_total3 <- read_csv("outputs/classifier_data/df_total3.csv")
df_total4 <- read_csv("outputs/classifier_data/df_total4.csv")
df_total5 <- read_csv("outputs/classifier_data/df_total5.csv")
merge = data.frame(cbind(df_total[,c(8)],df_total2[,c(8)],df_total3[,c(8)],df_total4[,c(8)],df_total5[,c(8)]))
write.csv(merge,paste0("outputs/log/","log_e5zz", lastdate,".csv"), row.names = FALSE) 
preds <- list.files(path = "outputs/log/.", recursive = TRUE, 
                    pattern = "\\.csv$", 
                    full.names = TRUE)
preds <- last(preds, 10)
for(i in seq_along(length(preds))) {
  csv= lapply(preds, read.csv, header=TRUE, sep=",")
}
for(i in seq_along(length(csv))) {  
  predictions = data.frame(rbindlist(csv))
}
predictions = last(predictions,1)
sum = sum(c(predictions$pred.sig + predictions$pred.sig.1 + predictions$pred.sig.2 + predictions$pred.sig.3 + predictions$pred.sig.4))
bar_signal = ifelse(sum == 10, 1,
                    ifelse(sum == 9, 1,
                           ifelse(sum == 8, 1,
                                  ifelse(sum == 7, -1,
                                         ifelse(sum == 6, -1,
                                                ifelse(sum == 5, -1,
                                                       0))))))
if(length(preds)>=10){
  sapply(first(preds,1), unlink)
}
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
trader_type_band <- list.files(path = "outputs/trader_type/band/.", recursive = TRUE, 
                               pattern = "\\.csv$", 
                               full.names = TRUE)
trader_type_trend <- list.files(path = "outputs/trader_type/trend/.", recursive = TRUE, 
                                pattern = "\\.csv$", 
                                full.names = TRUE)
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
source("scripts/channel_levels.r")
source("scripts/prev_bb_high.r")
source("scripts/prev_bb_low.r")
source("scripts/prev_bb_2_high.r")
source("scripts/prev_bb_2_low.r")
source("scripts/sma.r")
source("scripts/digital.r")
source("scripts/state_data.r")
if(length(trader_type_band) == 0 & length(trader_type_trend) == 0){
  source("scripts/band_entry.r")
}
if(length(trader_type_band) == 1 & length(trader_type_trend) == 0){
  source("scripts/trend_entry.r")
}
if(length(trader_type_band) == 1 & length(trader_type_trend) == 1){
  source("scripts/band_entry.r")
}
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
trade_types <- list.files(path = "outputs/trade_type/.", recursive = TRUE, 
                          pattern = "\\.csv$", 
                          full.names = TRUE)
if(length(trade_types) == 1){
  trade_type <- read_csv("outputs/trade_type/trade_type.csv")
  print(paste0("trade type is ", trade_type))
}
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
source("scripts/pop_perc_dif.r")
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
if(length(trades) == 0){
  if(trade_m15 == 1 | trade_m15 == 4){
    write.csv(current_ppd,"outputs/trade_block_tier/tbt.csv", row.names = FALSE)
  }
}
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
if(length(trades) >= 1){
  trade_block_tier <- read_csv("outputs/trade_block_tier/tbt.csv")
  if(trade_block_tier$x < current_ppd){
    print("increased rl state pop., overwrite existing tbt w/ higher rl state pop percentage.")
    write.csv(current_ppd,"outputs/trade_block_tier/tbt.csv", row.names = FALSE)
  }
  if(trade_block_tier$x > current_ppd){
    print("decreased rl state pop., overwrite existing tbt w/ lower rl state pop percentage.")
    write.csv(current_ppd,"outputs/trade_block_tier/tbt.csv", row.names = FALSE)
  }
}

#######################################################################################
#######################################################################################
#######################################################################################
if(length(trades)==0 & trade_m15 == 1){
  rsqr <- c(read_csv("outputs/rsqr/rsqr.csv"))
  source("scripts/peak_valley_entry_check2.r")
}
if(length(trades)==0 & trade_m15 == 4){
  rsqr <- c(read_csv("outputs/rsqr/rsqr.csv"))
  source("scripts/peak_valley_entry_check2.r")
} 
#######################################################################################
#######################################################################################
#######################################################################################
print(paste0("the market is ", market))
if(length(trades) == 0){
  if(trade_m15 == 1){
    if(market == "bearish"){
      trade_m15 = 0 
      print("do not open buy, price is below daily pivot point.")
    }
    if(market == "unknown"){
      trade_m15 = 0 
      print("do not open buy, price is unknown to daily pivot point.")
    }
  } else if(trade_m15 == 4){
    if(market == "bullish"){
      trade_m15 = 0 
      print("do not open sell, price is above daily pivot point.")
    }
    if(market == "unknown"){
      trade_m15 = 0 
      print("do not open sell, price is unknown to daily pivot point.")
    }
  } else {
    print("do not run pivot direction check, there is no signal to confirm.")
  }
}
#######################################################################################
#######################################################################################
#######################################################################################
source("scripts/vhf.r")
print(paste0("the vhf direction is ", vhfd, " and the level is ", vhf_level,"."))
if(length(trades) == 0){
  if(trade_m15 == 1){
    if(vhfd == "trending" & vhf_level == "strong_trend"){
      trade_m15 = trade_m15
      print(paste0("accept entry, the vhf direction is ", vhfd, " and the level is ", vhf_level,"."))
    } else {
      trade_m15 = 0
      print(paste0("do not accept entry, the vhf direction is ", vhfd, " and the level is ", vhf_level,"."))
    }
  } else if(trade_m15 == 4){
    if(vhfd == "trending" & vhf_level == "strong_trend"){
      trade_m15 = trade_m15
      print(paste0("accept entry, the vhf direction is ", vhfd, " and the level is ", vhf_level,"."))
    } else {
      trade_m15 = 0
      print(paste0("do not accept entry, the vhf direction is ", vhfd, " and the level is ", vhf_level,"."))
    }
  } else {
    print("do not run vhf check, there is no trade signal to confirm.")
  }
}
#######################################################################################
#######################################################################################
#######################################################################################
pvmv <- list.files(path = "outputs/pv_med_value/.", recursive = TRUE, 
                   pattern = "\\.csv$", 
                   full.names = TRUE)
if(length(pvmv) == 1){
  pv_med_value <- read_csv("outputs/pv_med_value/pvmv.csv")
  print(paste0("the position is in a ", pv_med_value$x))
} else {
  pv_med_value = data.frame('x' = c("low_potential"))
}
if(length(trades) == 0){
  if(trade_m15 == 1){
    source("scripts/set_stoploss.r")
    source("scripts/factor.r")
    source("scripts/update_values.r")
    source_python("scripts/refresh_excel.py") 
  } else if(trade_m15 == 4){
    source("scripts/set_stoploss.r")
    source("scripts/factor.r")
    source("scripts/update_values.r")
    source_python("scripts/refresh_excel.py") 
  } else {
    sl = 100
    factor_num = 4
    print("default sl and factor bc no position is opening. do not update trade files.")
  }
} else if(length(trades)>=1){
  print(paste0("the sl setting is ", sl))
  print(paste0("the sl factor value is ", factor_num))
  marker_one = sl/factor_num
  print(paste0("the first marker value is ", marker_one))
  reverse_marker_one = marker_one/2
  print(paste0("the first reverse marker value is ", reverse_marker_one))
}
#######################################################################################
#######################################################################################
#######################################################################################
name1 = paste0("trades/","results_E5ZZ_m15", lastdate,"_1.csv")
name2 = paste0("trades/","results_E5ZZ_m15", lastdate,"_4.csv")
if(trade_m15 == 1){
  write.csv(results_E5ZZ_m15,name1, row.names = FALSE)
} else if(trade_m15 == 4){
  write.csv(results_E5ZZ_m15,name2, row.names = FALSE)
}
#######################################################################################
#######################################################################################
#######################################################################################
trades <- list.files(path = "trades/.", recursive = TRUE, 
                     pattern = "\\.csv$", 
                     full.names = TRUE)
n_last <- 5                               
ext = substr(trades, nchar(trades) - n_last + 1, nchar(trades)) 
require(stringr)
require(data.table)
ext = str_remove(ext,".csv")
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
source("scripts/next_pos.r")
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
tbt <- list.files(path = "outputs/trade_block_tier/.", recursive = TRUE, 
                  pattern = "\\.csv$", 
                  full.names = TRUE)
if(length(tbt) == 1){
  trade_block_tier <- read_csv("outputs/trade_block_tier/tbt.csv")
}
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
if(length(trade_m15) >= 1){
  source("scripts/capture_trades.r")
  # Activate Trading Decisions (Accept the Predicted Signal)
  if(nrow(ptrades) == 0){
    trade_m15 = trade_m15
    assign("trade_m15",trade_m15, envir = globalenv())
    profit_sum = 0 
    assign("profit_sum",profit_sum, envir = globalenv())
    print(paste0("no positions open."))
    # Activate Trading Decisions (Do Not Accept the Predicted Signal)  ----
  } else if(nrow(ptrades) > 0){
    ot = c(ptrades[,1])
    currentp = c(current_cp)
    n = length(ot)
    if(n == 1){
      currentp = currentp
    } else if(n > 1){
      currentp = rep(currentp,n)
    }
    if(last(ext,1) == 1){
      diffp = c(mapply('-', currentp, ot, SIMPLIFY = FALSE))
      diffp = do.call(c, diffp)
    } else if(last(ext,1) == 4){
      diffp = c(mapply('-', ot, currentp, SIMPLIFY = FALSE))
      diffp = do.call(c, diffp)
    }
    profit_sum = sum(diffp)
    assign("profit_sum",profit_sum, envir = globalenv())
    pvmv <- list.files(path = "outputs/pv_med_value/.", recursive = TRUE, 
                       pattern = "\\.csv$", 
                       full.names = TRUE)
    if(length(pvmv) == 1){
      pv_med_value <- read_csv("outputs/pv_med_value/pvmv.csv")
    }
    source("scripts/safe_net_values.r")
    source("scripts/safe_nets/sn_string_profit.r")
  }
} 
if(profit_sum < 0){
  print(paste0("profit sum negative ", round(profit_sum,6)))
} else if(profit_sum > 0){
  print(paste0("profit sum positive ", round(profit_sum,6)))
}
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
if(length(trades)==1){
  if(profit_sum < 0){
    if(last(ext,1) == 1){
      if(correction == -1 & bar_signal == -1 & market == "bearish"){
        trade_m15 = 98
        print("close buy/s bc at least bsts, bar signal, and pivot point market direction disagree.")
      } else {
        print("no indication of a market reversal, keep trading until closure signal.")
      }
    } else if(last(ext,1) == 4){
      if(correction == 1 & bar_signal == 1 & market == "bullish"){
        trade_m15 = 99
        print("close sell/s bc at least bsts, bar signal, and pivot point market direction disagree.")
      } else {
        print("no indication of a market reversal, keep trading until closure signal.")
      }
    }
  } else {
    print("no indication of a market reversal, keep trading until closure signal.")
  }
}
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
source("scripts/position_checks/pos_checks_list.r")
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
if(length(pvmv) == 1){
  pv_med_value <- read_csv("outputs/pv_med_value/pvmv.csv")
  print(paste0("the position is in a ", pv_med_value$x))
}
if(length(trade_status)>=1){
  ts <- read_csv("outputs/trade_status/ts.csv")
  if(ts$x >0){
    past_trade = "positive"
  } else if(ts$x < 0){
    past_trade = "negative"
  } else {
    past_trade = "neutral"
  }
} else if(length(trade_status) == 0){
  past_trade = "neutral"
}
print(paste0("past trade status is ", past_trade))
if(past_trade == "neutral"){
  source("scripts/pos_marker_data.r")
} else if(past_trade == "positive"){
  source("scripts/pos_marker_data.r")
} else if(past_trade == "negative"){
  source("scripts/pos_marker_data2.r")
}
if(length(trades)>=1){
  if(pv_med_value$x == "low_potential"){
    source("scripts/pos_checks_t1_t3.r")
  }
  if(pv_med_value$x == "medium_potential"){
    source("scripts/pos_checks_t4_t5.r")
  }
  if(pv_med_value$x == "high_potential"){
    source("scripts/pos_checks_t6_t8.r")
  }
}
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
if(length(trades) >=1){
  if(length(trade_types) == 1){
    trade_type <- read_csv("outputs/trade_type/trade_type.csv")
    if(trade_type == "reversal" & last(ext,1) == 1 & trade_m15 == 98 & profit_sum < 0){
      loss_type = "loss"
      write.csv(loss_type,"outputs/trader_type/band/trader_type.csv", row.names = FALSE)
    }
    trade_type <- read_csv("outputs/trade_type/trade_type.csv")
    if(trade_type == "reversal" & last(ext,1) == 4 & trade_m15 == 99 & profit_sum < 0){
      loss_type = "loss"
      write.csv(loss_type,"outputs/trader_type/band/trader_type.csv", row.names = FALSE)
    }
    if(trade_type == "double" & last(ext,1) == 1 & trade_m15 == 98 & profit_sum < 0){
      loss_type = "loss"
      write.csv(loss_type,"outputs/trader_type/band/trader_type.csv", row.names = FALSE)
    }
    trade_type <- read_csv("outputs/trade_type/trade_type.csv")
    if(trade_type == "double" & last(ext,1) == 4 & trade_m15 == 99 & profit_sum < 0){
      loss_type = "loss"
      write.csv(loss_type,"outputs/trader_type/band/trader_type.csv", row.names = FALSE)
    }
    if(trade_type == "trend" & last(ext,1) == 1 & trade_m15 == 98 & profit_sum < 0){
      loss_type = "loss"
      write.csv(loss_type,"outputs/trader_type/trend/trader_type.csv", row.names = FALSE)
    }
    trade_type <- read_csv("outputs/trade_type/trade_type.csv")
    if(trade_type == "trend" & last(ext,1) == 4 & trade_m15 == 99 & profit_sum < 0){
      loss_type = "loss"
      write.csv(loss_type,"outputs/trader_type/trend/trader_type.csv", row.names = FALSE)
    }
  }
}
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
if(length(trader_type_band) == 1 & length(trader_type_trend) == 1){
  sapply(trader_type_band, unlink)
  sapply(trader_type_trend, unlink)
}
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
if(length(trades)>=1){
  if(past_trade == "neutral"){
    source("scripts/equity_closure.r")
  } else if(past_trade == "positive"){
    source("scripts/equity_closure.r")
  } else if(past_trade == "negative"){
    source("scripts/equity_closure2.r")
  }
}
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
if(trade_m15 != 0 & length(trades) >= 1){
  trades <- list.files(path = "trades/.", recursive = TRUE, 
                       pattern = "\\.csv$", 
                       full.names = TRUE)
  n_last <- 5                               
  ext = substr(trades, nchar(trades) - n_last + 1, nchar(trades)) 
  require(stringr)
  require(data.table)
  ext = str_remove(ext,".csv")
  
  if(last(ext,1) == "4"){
    cs = str_detect(ext, "1", negate = FALSE)
    cs_status = str_detect(cs, "TRUE", negate = FALSE)
    if(first(cs_status,1) == "TRUE"){
      sapply(trades[-length(trades)], unlink)
    } else if(last(cs_status,1) == "FALSE"){
      print("no reversal signal, check short position files for close.")
    }
  } else if(last(ext,1) == "1"){
    cb = str_detect(ext, "4", negate = FALSE)
    cb_status = str_detect(cb, "TRUE", negate = FALSE)
    if(first(cb_status,1) == "TRUE"){
      sapply(trades[-length(trades)], unlink)
    } else if(last(cb_status,1) == "FALSE"){
      print("no reversal signal, check buy position files for close.")
    }
  }
  if(trade_m15 == "98"){
    source("scripts/remove_files.r")
    print(paste0("forced close buy/s, remove recorded open position files..."))
    write.csv(percent_change,"outputs/trade_status/ts.csv", row.names = FALSE)
    # Prior Trade Info
    pt_status = ifelse(profit_sum >= 0, "positive", "negative")
    pti_sl = sl
    pti_close = current_cp
    pti = data.frame("date" = lastdate,"signal" = last(ext,1), "pt_status" = pt_status, "pip sum" = profit_sum,"set_sl" = pti_sl, "close_price" = pti_close, last(pivots,1))
    write.csv(pti,paste0("outputs/past_trade_status/",lastdate,"ts.csv"), row.names = FALSE)
    print(paste0("save trade results..."))
  } 
  if(trade_m15 == "99"){
    source("scripts/remove_files.r")
    print(paste0("forced close sell/s, remove recorded open position files..."))
    write.csv(percent_change,"outputs/trade_status/ts.csv", row.names = FALSE)
    # Prior Trade Info
    pt_status = ifelse(profit_sum >= 0, "positive", "negative")
    pti_sl = sl
    pti_close = current_cp
    pti = data.frame("date" = lastdate,"signal" = last(ext,1), "pt_status" = pt_status, "pip sum" = profit_sum,"set_sl" = pti_sl, "close_price" = pti_close, last(pivots,1))
    write.csv(pti,paste0("outputs/past_trade_status/",lastdate,"ts.csv"), row.names = FALSE)
    print(paste0("save trade results..."))
  } 
}
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
results_E5ZZ_m15$trade_m15 = trade_m15
if(o_trade_m15 != trade_m15){ 
  print(paste0("new trade_m15 signal ", trade_m15))
} else if(o_trade_m15 == trade_m15){
  print(paste0("original trade_m15 signal ", trade_m15))
}
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
# Save the result file to transfer to API for signal to MT% ----
print(paste0("save the prediction file... "))
write.csv(results_E5ZZ_m15,"outputs/results_E5ZZ_m15.csv", row.names = FALSE)
rm(trade_m15)
rm(o_trade_m15)
rm(pos_positive_count)
rm(pos_negative_count)
rm(pos_positive)
rm(pos_negative)
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
fnt_list <- list.files(path = "rl_agent_crowder/deploy_outputs/failed_neg_theta/.", recursive = TRUE, 
                       pattern = "\\.csv$", 
                       full.names = TRUE)
if(length(fnt_list)>=1){
  sapply(fnt_list, unlink)
}
fnp_list <- list.files(path = "rl_agent_crowder/deploy_outputs/failed_neg_positions/.", recursive = TRUE, 
                       pattern = "\\.csv$", 
                       full.names = TRUE)
if(length(fnp_list)>=1){
  sapply(fnp_list, unlink)
}
fpt_list <- list.files(path = "rl_agent_crowder/deploy_outputs/failed_pos_theta/.", recursive = TRUE, 
                       pattern = "\\.csv$", 
                       full.names = TRUE)
if(length(fpt_list)>=1){
  sapply(fpt_list, unlink)
}
fpp_list <- list.files(path = "rl_agent_crowder/deploy_outputs/failed_pos_positions/.", recursive = TRUE, 
                       pattern = "\\.csv$", 
                       full.names = TRUE)
if(length(fpp_list)>=1){
  sapply(fpp_list, unlink)
}
list <- list.files(path = "rl_agent_crowder/train_outputs/pos_ratio/.", recursive = TRUE, 
                   pattern = "\\.csv$", 
                   full.names = TRUE)
if(length(list)>=10){
  sapply(list, unlink)
}
list <- list.files(path = "rl_agent_crowder/train_outputs/sharpe_ratio/.", recursive = TRUE, 
                   pattern = "\\.csv$", 
                   full.names = TRUE)
if(length(list)>=10){
  sapply(list, unlink)
}
list <- list.files(path = "rl_agent_crowder/train_outputs/theta/.", recursive = TRUE, 
                   pattern = "\\.csv$", 
                   full.names = TRUE)
if(length(list)>=10){
  sapply(list, unlink)
}
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################