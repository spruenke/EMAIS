setwd("D:/Dropbox/Studium/B.Sc. Economics/Bachelorarbeit/b-thesis/data_new")
#setwd("/media/erin/DATA/Dropbox/Studium/B.Sc. Economics/Bachelorarbeit/b-thesis/data_new") #Ubuntu path

### cryptocurrencies (coingecko)
rm(list = ls())

tck.cc = c("btc", "eth", "ltc", "usdt", "xlm", "xrp") # by the 21.04.2020 these cover roughly 82 % of the CC market by market cap in USD

data.list = lapply(tck.cc, FUN = function(x){
  read.csv(paste0(x, "-usd-max.csv"))
})

data.list.new = lapply(data.list, FUN = function(x){
  x[-which(as.Date(x[,1], format = "%Y-%m-%d") < as.Date("2016-02-11")),]
})
data.list.new = lapply(data.list.new, FUN = function(x){
  x[-which(as.Date(x[,1], format = "%Y-%m-%d") > as.Date("2018-12-31")),]
})

data.list.dates = lapply(data.list, FUN = function(x){
  as.Date(x[,1], format = "%Y-%m-%d")
})
data.list.new.dates = lapply(data.list.dates, FUN = function(x){
  x[-which(x < as.Date("2016-02-11"))]
  
})
data.list.new.dates = lapply(data.list.new.dates, FUN = function(x){
  x[-which(x > as.Date("2018-12-31"))]
})
lapply(data.list.new.dates, FUN = function(x){
  which(data.list.new.dates[[4]]%in%x == F)
})
abc = which(data.list.new.dates[[1]]%in%data.list.new.dates[[4]] == F) ## 
#data.list.new[[4]] = rbind(data.list.new[[4]][1:1203,], rep(NA, 4), data.list.new[[4]][1204:nrow(data.list.new[[4]]),])

dates.cryptos = data.list.new.dates[[1]]

data.list.2 = lapply(data.list.new, "[", 2) #price
data.list.2.v = lapply(data.list.new, "[", 4) #volume

cc.prices = do.call(cbind, data.list.2)
cc.vol = do.call(cbind, data.list.2.v)



rm(data.list, data.list.2, data.list.2.v, data.list.dates, data.list.new, data.list.new.dates, abc)
colnames(cc.prices) = tck.cc
colnames(cc.vol) = tck.cc

##### stock prices ##########################################################################################################
tck.stock = c("CSI500Smallcap", "S&P600") #omit "DAX", "FTSE", "NASDAC", "Nikkei"

price.sdax = read.csv2("sdax_pr.csv")
vol.sdax = read.csv("sdax_vol.csv")

#fill in NA in sdax-data
for (i in 1:ncol(price.sdax)){
  price.sdax[,i] = gsub("#N/A N/A", NA, price.sdax[,i])
  price.sdax[,i] = gsub(",", ".", price.sdax[,i])
  vol.sdax[,i] = gsub("#N/A N/A", NA, vol.sdax[,i])
  vol.sdax[,i] = gsub(",", ".", vol.sdax[,i])
}

dates.sdax = as.Date(price.sdax[,1], format = "%d.%m.%Y")
dates.sdax.vol = as.Date(vol.sdax[,1], format = "%m/%d/%Y")
all(dates.sdax%in%dates.sdax.vol)
all(dates.sdax.vol%in%dates.sdax)

dates.sdax = dates.sdax[dates.sdax >= "2016-02-11"]
price.sdax = price.sdax[as.Date(price.sdax[,1], format = "%d.%m.%Y")%in%dates.sdax,-1]
vol.sdax   = vol.sdax[as.Date(vol.sdax[,1], format = "%m/%d/%Y")%in%dates.sdax,-1]


stock.list = lapply(tck.stock, FUN = function(x){
  read.csv(paste0(x, "_P_USD.csv"))
})

stock.prices = lapply(stock.list, FUN = function(x){
  x[,-1]
})

topix.p = read.csv2("TOPIX_P_USD.csv")

stock.prices.2 = do.call(cbind, stock.prices)
stock.prices.2 = cbind(stock.prices.2, topix.p)

stock.list.2 = lapply(tck.stock, FUN = function(x){
  read.csv(paste0(x, "_VA_USD.csv"))
})

stock.vol = lapply(stock.list.2, FUN = function(x){
  x[,-1]
})

topix.v = read.csv2("TOPIX_VA_USD.csv")

stock.vol.2 = do.call(cbind, stock.vol)
stock.vol.2 = cbind(stock.vol.2, topix.v)
dates = as.Date(stock.list[[1]][,1], format = "%m/%d/%Y")
dates.2 = dates[dates >= as.Date("2016-02-11")]
dates.2 = dates.2[dates.2 <= as.Date("2018-12-31")]
ddd = which(dates.2%in%dates.sdax == T)
all(dates.2%in%dates.sdax) # TRUE
all(dates.sdax%in%dates.2) # TRUE
all(dates.2%in%dates.cryptos) # TRUE
ln.dif = length(dates) - length(dates.2)
dde = which(dates%in%dates.2 == T)
stock.prices.2 = stock.prices.2[dde,]
stock.vol.2 = stock.vol.2[dde,]

dates.miss.p = apply(stock.prices.2, 2, FUN = function(x){
  length(which(is.na(x) == T))
})
dates.miss.s = apply(price.sdax, 2, FUN = function(x){
  length(which(is.na(x) == T))
})
any.nr = which(dates.miss.p > 0) # which stocks have NA values in prices
any.nr.2 = which(dates.miss.s > 0)

stock.prices.2 = stock.prices.2[,-any.nr]
stock.vol.2 = stock.vol.2[,-any.nr]

price.sdax = price.sdax[,-any.nr.2]
vol.sdax = vol.sdax[,-any.nr.2]


dates.miss.v = apply(stock.vol.2, 2, FUN = function(x){
  length(which(is.na(x) == T))
})
dates.miss.vs = apply(vol.sdax, 2, FUN = function(x){
  length(which(is.na(x) == T))
})

any.vr = which(dates.miss.v > 100) # which stocks have more than 100 NA values in volume
any.vr.2 = which(dates.miss.vs > 100)

if(any.vr != 0){
  stock.prices.2 = stock.prices.2[,-any.vr]
  stock.vol.2 = stock.vol.2[, -any.vr]
}
if(any.vr.2 != 0){
  price.sdax = price.sdax[,-any.vr]
  vol.sdax = vol.sdax[, -any.vr]
}
price.sdax = as.data.frame(apply(price.sdax, 2, as.numeric))
vol.sdax   = as.data.frame(apply(vol.sdax, 2, as.numeric)) 

#### convert sdax to USD
fx = read.csv2("EURUSD.csv")
fx.dates = as.Date(fx[,1], format = "%d.%m.%Y")
fx = fx[,2]
da = which(fx.dates%in%dates.2 == T)
fx = fx[da]
fx.dates = fx.dates[da]
all(fx.dates%in%dates.2)
all(dates.2%in%fx.dates)

price.sdax = price.sdax * fx
vol.sdax   = vol.sdax * fx * 1000

stock.prices = cbind(price.sdax, stock.prices.2) # price.sdax #
stock.vol = cbind(vol.sdax, stock.vol.2) # vol.sdax #
dates.stocks = dates.2
rm(fx, fx.dates, da, price.sdax, vol.sdax, stock.list, stock.list.2, stock.prices.2, stock.vol.2, dates, dates.2, dates.miss.p, dates.miss.v, ln.dif, any.nr, any.vr, any.nr.2, any.vr.2, dates.miss.s, dates.miss.vs, dates.sdax, dates.sdax.vol, ddd, dde, i, topix.p, topix.v)

### sample 70 stocks in a more or less good proportion
set.seed(31416)
a = sample(c(1:62), 19)
b = sample(c(63:563), 17)
d = sample(c(564:1164), 17)
e = sample(c(1165:ncol(stock.prices)), 17)
sample.stocks = c(a,b,d,e)
stock.prices = stock.prices[,sample.stocks]
stock.vol = stock.vol[,sample.stocks]
rm(a,b,d,e,sample.stocks)

######################### ADJUST CC AND STOCK DATA ################################################################

venn = which(dates.cryptos%in%dates.stocks == T)
dates.cryptos.2 = dates.cryptos[venn]

all(dates.cryptos.2 == dates.stocks) # evaluates to TRUE, so we have equal dates
dates.cryptos = dates.cryptos.2

# adjust prices and volume to new dates
cc.prices = cc.prices[venn,]
cc.vol = cc.vol[venn,]

rm(dates.cryptos.2, venn)


#################### COMMODITIES ################################################################################################

commodities.prices = read.csv("Commodities_P_USD.csv")
dates = as.Date(commodities.prices[,1], format = "%m/%d/%Y")
commodities.prices = commodities.prices[,-1]

dd = which(dates%in%dates.stocks == T)

dates.commodities = dates[dd]
all(dates.commodities == dates.stocks)

commodities.prices = commodities.prices[dd,-3]

rm(dates, dd)

##################### PUT IT ALL TOGETHER ##########################################################################################

prices = cbind(stock.prices, commodities.prices, cc.prices)
volume = cbind(stock.vol, matrix(NA, nrow = nrow(commodities.prices), ncol = ncol(commodities.prices)), cc.vol)
a = which(complete.cases(prices) == F) # there is one observation to be NA, we will delete the whole observation

dates = dates.stocks
if(a != 0){
  prices = prices[-a,]
  volume = volume[-a,]
  dates = dates.stocks[-a]
} else {
  dates = dates.stocks
}




rm(a, tck.cc, tck.stock, cc.prices, cc.vol, commodities.prices, stock.prices, stock.vol, dates.commodities, dates.cryptos, dates.stocks)

### RETURNS ###
prices = data.matrix(prices)
volume = data.matrix(volume)
#prices = prices[,-128]
#volume = volume[,-128]
#prices = prices[,-c(130,131,132,133,456)]
#volume = volume[,-c(130,131,132,133,456)]
returns = apply(prices, 2, FUN = function(x){
  x[-1]/x[-length(x)] - 1
})

save(prices, volume, returns, dates, file = "final_set.RData")