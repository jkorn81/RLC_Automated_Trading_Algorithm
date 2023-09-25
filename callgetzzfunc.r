
#' Log system time, request method and HTTP user agent of the incoming request
#' @filter logger
function(req){
  cat("System time:", as.character(Sys.time()), "\n",
      "Request method:", req$REQUEST_METHOD, req$PATH_INFO, "\n",
      "HTTP user agent:", req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n")
  plumber::forward()
}

library(jsonlite)

#' Publish getzzpredict(sub)
#' @param req a json request
#' @post /pred
function(req, res){
  rdata <- fromJSON(req$postBody, simplifyVector = TRUE, flatten = TRUE)
  df_rates <- as.data.frame(rdata)
  colnames(df_rates) <- c("Open","High","Low","Close")
  assign("df_rates",df_rates, envir = globalenv())
  ##getzzpredict::getzzpredict(df_rates)
  ##df_save <- cbind(last(df_rates,1),last(df_total,1))
  ##assign("df_save",df_save, envir = globalenv())
  res$body <- results
        }

#' Publish E4(sub)
#' @param req a json request
#' @post /pred2
function(req, res){
  rates_p2 <- fromJSON(req$postBody, simplifyVector = TRUE, flatten = TRUE) 
  df_rates_p2 <- as.data.frame(rates_p2)
  colnames(df_rates_p2) <- c("Account","Balance","Position","Date","Open","High","Low","Close")
  assign("df_rates_p2",df_rates_p2, envir = globalenv())
  write.csv(df_rates_p2,"C:/Users/Jonathan Korn/Desktop/rl_mps_secure/rl_agent_crowder/data/df_rates_p2.csv", row.names = FALSE)
  print("start rl crowding system")
  #source("C:/Users/Jonathan Korn/Desktop/rl_mps_secure/tc.caret.para.deploy.r")
  source("C:/Users/Jonathan Korn/Desktop/rl_mps_secure/tc.caret.para.deploy_state_data.r")
  print("complete rl crowding sytsem")
  res$body <- results_E5ZZ_m15
}

#' Publish gettzzpredict(sub)
#' @param req a json request
#' @post /pre3
function(req, res){
  rates_p3 <- fromJSON(req$postBody, simplifyVector = TRUE, flatten = TRUE)
  df_rates_p3 <- as.data.frame(rates_p3)
  colnames(df_rates_p3) <- c("Open","High","Low","Close")
  assign("df_rates_p3",df_rates_p3, envir = globalenv())
  getzzpredict::getzzpredict(df_rates_p3)
  ##df_combined_ts <- cbind(last(df_ratests,1),last(df_total,1))
  ##df_save_ts <- df_combined_ts
  ##df_save_ts <- rbind(df_save_ts,df_combined_ts)
  ##assign("df_save_ts",df_save_ts, envir = globalenv())
  ######return results
  res$body <- df_total
}