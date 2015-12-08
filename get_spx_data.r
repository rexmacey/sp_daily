#Quandl Data YAHOO/INDEX_GSPC
library("Quandl")
setwd("C:/Users/Rex/Documents/Quant Trading/GRAT")
spx <- Quandl("YAHOO/INDEX_GSPC")
saveRDS(spx,file="spx.rds")
spx.xts<-xts(spx$Close,order.by=spx$Date)
saveRDS(spx.xts,file="spx_xts.rds")

