rm(list = ls())
graphics.off()

#Set proper working directory
dir = "D:/Dropbox/Studium/B.Sc. Economics/Bachelorarbeit/EMAIS_git/EMAIS_DataManip"
setwd(dir)

##########################
#### Cryptocurrencies ####
##########################

    tck = c("BTC-USD", "XRP-USD", "ETH-USD", "LTC-USD", "BCH-USD", "BNB-USD", "USDT-USD", "EOS-USD", "TRX-USD", "XLM-USD") #tickers
    ccc = c("Bitcoin", "Ripple", "Ethereum", "Litecoin", "Bitcoin Cash", "Binance Coin", "Tether", "Eos", "Tronix", "Stellar") #names of the cryptos
    
    tck.2 = tolower(tck)
    tck.2 = paste0(tck.2, "-max.csv")
    
    data.list = lapply(tck.2, read.csv)
    
    
    len.2 = sapply(data.list, nrow)
    max.len.2 = max(len.2)
    
    dates.2 = as.Date(data.list[[1]][,1], format = "%Y-%m-%d")
    data.list.2 = lapply(data.list, "[", 2) #price
    data.list.2.v = lapply(data.list, "[", 4) #volume
    
    
    data.list.3 = lapply(data.list.2, FUN = function(x){
      c(rep(NA, (max.len.2 - nrow(x))), x[,1])
    })
    
    data.list.3.v = lapply(data.list.2.v, FUN = function(x){
      c(rep(NA, (max.len.2 - nrow(x))), x[,1])
    })
    crypto.cg = as.data.frame(sapply(data.list.3, cbind))
    crypto.cg.v = as.data.frame(sapply(data.list.3.v, cbind))
    colnames(crypto.cg) = ccc
    rownames(crypto.cg) = dates.2
    colnames(crypto.cg.v) = ccc
    rownames(crypto.cg.v) = dates.2
    
    save(crypto.cg, file = "crypto_cg.RData")
    save(crypto.cg.v, file = "crypto_cg_vol.RData")
    
    #### Load SDAX data ####
    sdax = read.csv2("sdax_pr.csv")
    sdax.vol = read.csv("sdax_vol.csv")

#####################
#### Adjustments ####
#####################

    sdax.vol = sdax.vol[-c(1:5),]
    
    sdax[,1] = as.Date(as.character(sdax[,1]), format = "%d.%m.%Y")
    sdax.vol[,1] = as.Date(as.character(sdax.vol[,1]), format = "%m/%d/%Y")
    sdax.vol[,2:ncol(sdax.vol)] = apply(sdax.vol[,2:ncol(sdax.vol)],2,as.numeric)
    #crypto data starts from 2013-01-01
      sdax = sdax[which(sdax[,1]=="2013-01-01"):nrow(sdax),]
      sdax.vol = sdax.vol[which(sdax.vol[,1]=="2013-01-01"):nrow(sdax.vol),]
    
    #lengths of datasets "sdax" and "crypto" differ because sdax constituents are not traded on weekends
    #adjust length (i.e.: delete observations of crypto which are not in sdax)
    
      abc.2 = which(rownames(crypto.cg)%in%as.character(sdax[,1])==TRUE)
      crypto.cg.1 = crypto.cg[abc.2,]
      crypto.cg.v.1 = crypto.cg.v[abc.2,]
    
    #fill in NA in sdax-data
      for (i in 1:ncol(sdax)){
        sdax[,i] = gsub("#N/A N/A", NA, sdax[,i])
        sdax[,i] = gsub(",", ".", sdax[,i])
      }
    
    
      dates.sdax = sdax[,1]
      sdax = sdax[,-1]
      sdax.vol = sdax.vol[,-1]
    
      sdax = as.data.frame(apply(sdax,2,as.numeric))
    
      complete.cases(t(sdax))
    
    #Use 6 Cryptos starting in August 2015
    #Use coingecko as final source (for some reason, yahoo finance contains outliers at Tether)
    
    #when is data available for 6 cryptos?
      f.na = apply(crypto.cg.1, 1, FUN = function(x){length(which(!is.na(x))==TRUE)}) #vector of how many not-NA values are in each row
      f.date = rownames(crypto.cg.1)[min(which(f.na==6))] #date of when 6 cryptos are available
      
      sdax.new = sdax[which(dates.sdax==f.date):nrow(sdax),] #adjust sdax data
      sdax.vol.new = sdax.vol[which(dates.sdax==f.date):nrow(sdax),] #adjust sdax volume in the same way
      
      crypto.new = crypto.cg.1[min(which(f.na==6)):nrow(crypto.cg.1),] #adjust crypto data
      crypto.v.new = crypto.cg.v.1[min(which(f.na==6)):nrow(crypto.cg.1),] #use volume data adjusted by the method before (! price data is referenced by intention so the data matches !)
      crypto.v.new = crypto.v.new[,which(complete.cases(t(crypto.new))==T)] #use only data of those which are complete in price data
      crypto.new = crypto.new[,which(complete.cases(t(crypto.new))==T)] #use only complete columns of crypto
      
      
      sdax.new = sdax.new[,which(complete.cases(t(sdax.new))==T)] #use complete cases for adjusted sdax data
      sdax.vol.new = sdax.vol.new[,which(complete.cases(t(sdax.new))==T)] #same for volume data
      colnames(sdax.vol.new) = colnames(sdax.new)
    
    
    #Alternative Assets Data from Eikon 04.03.2019
    #Gold, Palladium, Silver, Diamonds, Wheat and Corn
    
      alt = read.csv2("Alternative.csv", header = T)
      alt[,1] = as.Date(alt[,1], format = "%d.%m.%Y")
      alt[,2:10] = apply(alt[,2:10], 2, as.numeric)    
      
      s.date = which(alt[,1]==row.names(crypto.new)[1])  #adjust dates
      alt.new = alt[s.date:nrow(alt),] #check dates
      all(alt.new[,1]==row.names(crypto.new))
    #is true, thus remove dates from alt.new
      alt.new = alt.new[,-1]
      save(sdax.new, sdax.vol.new, crypto.new, crypto.v.new, alt.new, file = "final.RData")

########################################
#### Returns and final preparations ####
########################################

    na.vec = rep(NA,  nrow(sdax.new))
    dat = as.data.frame(cbind(sdax.new, crypto.new, alt.new))
    dat.vol = as.data.frame(cbind(sdax.vol.new, crypto.v.new))
    for(i in 1:9){
      dat.vol = cbind(dat.vol, na.vec)
    }
    dat.vol = as.data.frame(dat.vol)
    row.names(dat) = row.names(crypto.new)
    row.names(dat.vol) = row.names(crypto.new)
    returns = as.data.frame(apply(dat, 2, FUN = function(x){
      diff(x)/x[-length(x)]
    }))
    row.names(returns) = row.names(dat)[-1]
    dat.vol = dat.vol[-1,]
    
    save(returns, dat.vol, file = "final_set.RData")
