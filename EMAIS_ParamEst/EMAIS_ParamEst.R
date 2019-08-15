rm(list = ls())
graphics.off()

#Set proper working directory of this script:
dir = "D:/Dropbox/Studium/B.Sc. Economics/Bachelorarbeit/b-thesis/Quantlet/parameter_estimation"
setwd(dir)
load("final_set.RData")
source("functions.R")

M = 120 #Window Size

dates.tck = row.names(dat.vol)

#arithmetic mean
    a.m = matrix(nrow = nrow(returns)-M, ncol = ncol(returns))
    for(i in 1:(nrow(returns)-M)){
      a.m[i,] = par.est(returns[i:nrow(returns),], M = M, method = "ar")$mu
    }
    if(any(is.na(a.m))){warning("a.m object contains NA, maybe something wrong with window size or data?")}

#geometric mean
    g.m = matrix(nrow = nrow(returns)-M, ncol = ncol(returns))
    for(i in 1:(nrow(returns)-M)){
      g.m[i,] = par.est(returns[i:nrow(returns),], M = M, method = "gm")$mu
    }
    if(any(is.na(a.m))){warning("g.m object contains NA, maybe something wrong with window size or data?")}

#covariance matrix (classic)
    sig.n = list()
    for(i in 1:(nrow(returns)-M)){
      sig.n[[i]] = par.est(returns[i:nrow(returns),], M = M)$var
    }
    if(any(is.na(sig.n))){warning("sig.n object contains NA, maybe something wrong with window size or data?")}

#Bayes-Stein estimators
    bs.m = matrix(nrow = nrow(returns)-M, ncol = ncol(returns))
    sig.bs = list()
    for(i in 1:(nrow(returns)-M)){
      bs.m[i,] = par.est(returns[i:nrow(returns),], M = M, method = "bs")$mu
      sig.bs[[i]] = par.est(returns[i:nrow(returns),], M = M, method = "bs")$var
    }
    if(any(is.na(bs.m))){warning("bs.m object contains NA, maybe something wrong with window size or data?")}
    if(any(is.na(sig.bs))){warning("sig.bs object contains NA, maybe something wrong with window size or data?")}
    

abc.test = c()
for(i in 1:765){abc.test[i] = any(sig.bs[[i]] == sig.n[[i]])}

#if below gives TRUE then BS estimator of variance is different from normal Variance estimator
all(abc.test == FALSE)

#trading volume estimation
    TV.hat = matrix(nrow = nrow(dat.vol)-M, ncol = ncol(dat.vol))
    for(i in 1:(nrow(dat.vol)-M)){
      TV.hat[i,] = tv.est(dat.vol, M, method = "median")$TV
    }
    if(any(is.na(TV.hat))==T){warning("TV.hat object contains NA, maybe something wrong with window size or data?")}

save(file = "parameters.RData", list = c("a.m", "g.m", "sig.n", "bs.m", "sig.bs", "TV.hat"))