### Getzzpredict Server

setwd("C:/Users/Jonathan Korn/Desktop/rl_mps_secure")

## Load plumbr
library(plumber)

## Launch endpoint
pred_api <- pr("C:/Users/Jonathan Korn/Desktop/rl_mps_secure/callgetzzfunc.r")
pred_api$run(host = '127.0.0.1', port = 8001)



