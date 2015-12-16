#Quandl Data YAHOO/INDEX_GSPC
library("Quandl")
setwd("~/Quant Trading/sp_daily")
spx <- Quandl("YAHOO/INDEX_GSPC", end_date="2015-11-30",type = "xts")
saveRDS(spx,file="spx.rds")