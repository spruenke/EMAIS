### Care: The calculation time of this script may amount to several hours!

rm(list = ls())
graphics.off()

#set proper working direcotry
dir = "D:/Dropbox/Studium/B.Sc. Economics/Bachelorarbeit/EMAIS_git/EMAIS_Optim"
setwd(dir)
load("final_set.RData")
load("parameters.RData")

#get Rcpp
lib = c("Rcpp")
if(!(lib %in% installed.packages())){install.packages(lib)}
lapply(lib, library, character.only = T)

#load optimization functions
opt.rout = list.files("./opt", full.names = T)
lapply(opt.rout, source)
sourceCpp("./opt/src/colCvarCPP.cpp")
#choose a gamma for MV (risk aversion parameter) - defaults to 1
gam = 5
#choose target return
mu.p = 0.00026 #(remember, that daily returns are far lower than annual returns), approx 10% p.a.
#choose max-ratio f
f = 0.3
#choose VaR alpha
alpha = 0.05
#choose beginning wealth to start with
start_wealth = 1e6
#percentage cost of trading
fee = 0##   rep(c(0.0005, 0.002), c(ncol(a.m)-6,6)) #0.0005
#rebalancing period
rb = 1
ind = seq(1, nrow(a.m), rb)

############################
### Prepare optimization ###
############################

    #spaceholder matrix for result-dataframes and name-strings
      spc.hld = matrix(nrow = nrow(a.m), ncol = ncol(a.m))
      names   = c("w.ew", "w.mvcmv", "w.mvgmv", "w.mvsr", "w.gmmf", "w.gmmsc", "w.licmv", "w.licvar", "w.cvar")
      est.string = c(".am", ".gm", ".bs")
      est.string.2 = c(" with Arithmetic Mean", " with Geometric Mean", " with Bayes-Stein")
      est.string.3 = c(" (AM)", " (GM)", " (BS)")
      fin.names = expand.grid(names, est.string)
      fin.names = paste0(fin.names[,1], fin.names[,2])
      fin.names.1 = c("Naive", "Constrained Minimum Variance", "Global Minimum Variance", "Sharpe Ratio", "F-Geometric Mean Maximization", "C-Geometric Mean Maximization", "LIBRO-MPT", "LIBRO-CVaR", "CVaR")
      fin.names.2 = expand.grid(fin.names.1, est.string.2)
      fin.names.2 = paste0(fin.names.2[,1], fin.names.2[,2])
      strat.expl = data.frame(fin.names, fin.names.2)
      colnames(strat.expl) = c("Abbreviation", "Description")
      
      names.new = expand.grid(fin.names.1, est.string.3)
      names.new = paste0(names.new[,1], names.new[,2])
    #create dataframes for results
      for(i in 1:length(fin.names)){
        assign(fin.names[i], spc.hld)
      }
      
      #need returns in numeric form for matrix operations; Furthermore we need to cut the first M observations, as these were used to estimate (weights start at time M+1)
      returns.num = as.matrix(returns[(M+1):nrow(returns),])
      prices.num  = as.matrix(prices[(M+1):nrow(prices), ])
      returns.num = returns.num[ind,]
      prices.num = prices.num[ind,]
      dim(returns.num)

      #prepare a matrix for portfolio wealth P.w (752-M+1 x 25), only 25 strategies since equally weighted is always the same, no need to count it three times;
      #        the extra row is to save the starting wealth
      P = matrix(rep(start_wealth, (length(ind)+1) * 25), ncol = 25)
      
      ### INITIALIZE FIRST TIME POINT
      i = 1
      
      w.ew.am[i,]     = ew(a.m[i,], sig.n[[i]], wealth = P[i,1], prices = prices.num[i,])$weights
      P[i+1,1]      = (w.ew.am[i,]%*%returns.num[i,] + 1) * P[i,1] - sum(abs(w.ew.am[i,]) * P[i,1] * fee)   # Calculate Portfolioreturn (+1 = gross return) times the current portfolio value - costs
      
      w.mvcmv.am[i,]  = mv.opt(a.m[i,], sig.n[[i]], gamma = gam, method = "cmv", mu.p = mu.p, wealth = P[i,2], prices = prices.num[i,])$weights
      P[i+1,2]      = (w.mvcmv.am[i,]%*%returns.num[i,] + 1) * P[i,2]- sum(abs(w.mvcmv.am[i,]) * P[i,2] * fee)
      
      w.mvgmv.am[i,]  = mv.opt((a.m[i,]), sig.n[[i]], method = "gmv", wealth = P[i,3], prices = prices.num[i,])$weights
      P[i+1,3]      = (w.mvgmv.am[i,]%*%returns.num[i,] + 1) * P[i,3]- sum(abs(w.mvgmv.am[i,]) * P[i,3] * fee)
      
      w.mvsr.am[i,]   = mv.opt((a.m[i,]), sig.n[[i]], method = "sr", wealth = P[i,4], prices = prices.num[i,])$weights
      P[i+1,4]      = (w.mvsr.am[i,]%*%returns.num[i,] + 1) * P[i,4]- sum(abs(w.mvsr.am[i,]) * P[i,4] * fee)
      
      w.gmmf.am[i,]   = gmm.opt((a.m[i,]), sig.n[[i]], method = "f", wealth = P[i,5], prices = prices.num[i,])$weights
      P[i+1,5]      = (w.gmmf.am[i,]%*%returns.num[i,] + 1) * P[i,5]- sum(abs(w.gmmf.am[i,]) * P[i,5] * fee)
      
      w.gmmsc.am[i,]  = gmm.opt((a.m[i,]), sig.n[[i]], method = "sc", wealth = P[i,6], prices = prices.num[i,])$weights
      P[i+1,6]      = (w.gmmsc.am[i,]%*%returns.num[i,] + 1) * P[i,6]- sum(abs(w.gmmsc.am[i,]) * P[i,6] * fee)
      
      w.licmv.am[i,]  = li.opt((a.m[i,]), sig.n[[i]], TV = TV.hat[i,], method = "cmv", r = mu.p, f = f, wealth = P[i,7], prices = prices.num[i,])$weights
      P[i+1,7]      = (w.licmv.am[i,]%*%returns.num[i,] + 1) * P[i,7]- sum(abs(w.licmv.am[i,]) * P[i,7] * fee)
      
      w.licvar.am[i,] = li.opt((a.m[i,]), sig.n[[i]], TV = TV.hat[i,], method = "cvar", r = mu.p, f = f, n = 20000, window = M, alpha = alpha, data = returns[ind[i]:nrow(returns),], wealth = P[i,8], prices = prices.num[i,])$weights
      P[i+1,8]      = (w.licvar.am[i,]%*%returns.num[i,] + 1) * P[i,8]- sum(abs(w.licvar.am[i,]) * P[i,8] * fee)
      
      w.cvar.am[i,]   = cvar.opt(a.m[i,], sig.n[[i]], alpha = alpha, n = 20000, M = M, data = returns[ind[i]:nrow(returns),], wealth = P[i,9], prices = prices.num[i,])$weights
      P[i+1,9]      = (w.cvar.am[i,]%*%returns.num[i,] + 1) * P[i,9]- sum(abs(w.cvar.am[i,]) * P[i,9] * fee)
      
      w.mvcmv.gm[i,]  = mv.opt((g.m[i,]), sig.n[[i]], gamma = gam, method = "cmv", mu.p = mu.p, wealth = P[i,10], prices = prices.num[i,])$weights
      P[i+1,10]     = (w.mvcmv.gm[i,]%*%returns.num[i,] + 1) * P[i,10]- sum(abs(w.mvcmv.gm[i,]) * P[i,10] * fee)
      
      w.mvgmv.gm[i,]  = mv.opt((g.m[i,]), sig.n[[i]], method = "gmv", wealth = P[i,11], prices = prices.num[i,])$weights
      P[i+1, 11]    = (w.mvgmv.gm[i,]%*%returns.num[i,] + 1) * P[i,11]- sum(abs(w.mvgmv.gm[i,]) * P[i,11] * fee)
      
      w.mvsr.gm[i,]   = mv.opt((g.m[i,]), sig.n[[i]], method = "sr", wealth = P[i,12], prices = prices.num[i,])$weights
      P[i+1, 12]    = (w.mvsr.gm[i,]%*%returns.num[i,] + 1) * P[i,12]- sum(abs(w.mvsr.gm[i,]) * P[i,12] * fee)
      
      w.gmmf.gm[i,]   = gmm.opt((g.m[i,]), sig.n[[i]], method = "f", wealth = P[i,13], prices = prices.num[i,])$weights
      P[i+1, 13]    = (w.gmmf.gm[i,]%*%returns.num[i,] + 1) * P[i,13]- sum(abs(w.gmmf.gm[i,]) * P[i,13] * fee)
      
      w.gmmsc.gm[i,]  = gmm.opt((g.m[i,]), sig.n[[i]], method = "sc", wealth = P[i,14], prices = prices.num[i,])$weights
      P[i+1, 14]    = (w.gmmsc.gm[i,]%*%returns.num[i,] + 1) * P[i,14]- sum(abs(w.gmmsc.gm[i,]) * P[i,14] * fee)
      
      w.licmv.gm[i,]  = li.opt((g.m[i,]), sig.n[[i]], TV = TV.hat[i,], method = "cmv", r = mu.p, f = f, wealth = P[i,15], prices = prices.num[i,])$weights
      P[i+1,15]     = (w.licmv.gm[i,]%*%returns.num[i,] + 1) * P[i,15]- sum(abs(w.licmv.gm[i,]) * P[i,15] * fee)
      
      w.licvar.gm[i,] = li.opt((g.m[i,]), sig.n[[i]], TV = TV.hat[i,], method = "cvar", r = mu.p, f = f, n = 20000, window = M, alpha = alpha, data = returns[ind[i]:nrow(returns),], wealth = P[i,16], prices = prices.num[i,])$weights
      P[i+1,16]     = (w.licvar.gm[i,]%*%returns.num[i,] + 1) * P[i,16]- sum(abs(w.licvar.gm[i,]) * P[i,16] * fee)
      
      w.cvar.gm[i,]   = cvar.opt(g.m[i,], sig.n[[i]], alpha = alpha, n = 20000, M = M, data = returns[ind[i]:nrow(returns),], wealth = P[i,17], prices = prices.num[i,])$weights
      P[i+1,17]     = (w.cvar.gm[i,]%*%returns.num[i,] + 1) * P[i,17]- sum(abs(w.cvar.gm[i,]) * P[i,17] * fee)
      
      w.mvcmv.bs[i,]  = mv.opt((bs.m[i,]), sig.bs[[i]], gamma = gam, method = "cmv", mu.p = mu.p, wealth = P[i,18], prices = prices.num[i,])$weights
      P[i+1,18]     = (w.mvcmv.bs[i,]%*%returns.num[i,] + 1) * P[i,18] - sum(abs(w.mvcmv.bs[i,]) * P[i,18] * fee)
      
      w.mvgmv.bs[i,]  = mv.opt((bs.m[i,]), sig.bs[[i]], method = "gmv", wealth = P[i,19], prices = prices.num[i,])$weights
      P[i+1,19]     = (w.mvgmv.bs[i,]%*%returns.num[i,] + 1) * P[i,19]- sum(abs(w.mvgmv.bs[i,]) * P[i,19] * fee)
      
      w.mvsr.bs[i,]   = mv.opt((bs.m[i,]), sig.bs[[i]], method = "sr", wealth = P[i,20], prices = prices.num[i,])$weights
      P[i+1,20]     = (w.mvsr.bs[i,]%*%returns.num[i,] + 1) * P[i,20]- sum(abs(w.mvsr.bs[i,]) * P[i,20] * fee)
      
      w.gmmf.bs[i,]   = gmm.opt((bs.m[i,]), sig.bs[[i]], method = "f", wealth = P[i,21], prices = prices.num[i,])$weights
      P[i+1,21]     = (w.gmmf.bs[i,]%*%returns.num[i,] + 1) * P[i,21]- sum(abs(w.gmmf.bs[i,]) * P[i,21] * fee)
      
      w.gmmsc.bs[i,]  = gmm.opt((bs.m[i,]), sig.bs[[i]], method = "sc", wealth = P[i,22], prices = prices.num[i,])$weights
      P[i+1,22]     = (w.gmmsc.bs[i,]%*%returns.num[i,] + 1) * P[i,22]- sum(abs(w.gmmsc.bs[i,]) * P[i,22] * fee)
      
      w.licmv.bs[i,]  = li.opt((bs.m[i,]), sig.bs[[i]], TV = TV.hat[i,], method = "cmv", r = mu.p, f = f, wealth = P[i,23], prices = prices.num[i,])$weights
      P[i+1,23]     = (w.licmv.bs[i,]%*%returns.num[i,] + 1) * P[i,23]- sum(abs(w.licmv.bs[i,]) * P[i,23] * fee)
      
      w.licvar.bs[i,] = li.opt((bs.m[i,]), sig.bs[[i]], TV = TV.hat[i,], method = "cvar", r = mu.p, f = f, n = 20000, window = M, alpha = alpha, data = returns[ind[i]:nrow(returns),], wealth = P[i,24], prices = prices.num[i,])$weights
      P[i+1,24]     = (w.licvar.bs[i,]%*%returns.num[i,] + 1) * P[i,24] - sum(abs(w.licvar.bs[i,]) * P[i,24] * fee)
      
      w.cvar.bs[i,]  = cvar.opt(bs.m[i,], sig.bs[[i]], alpha = alpha, n = 20000, M = M, data = returns[ind[i]:nrow(returns),], wealth = P[i,25], prices = prices.num[i,])$weights
      P[i+1, 25]   = (w.cvar.bs[i,]%*%returns.num[i,] + 1) * P[i,25]- sum(abs(w.cvar.bs[i,]) * P[i,25] * fee)
      ### END FIRST TIME POINT
      
      #################################################
      ### 1. Optimize for arithemtic mean estimator ###
      #################################################
      start.time.am = Sys.time()
      for(i in 2:length(ind)){
        w.ew.am[i,]     = ew(a.m[i,], sig.n[[i]], wealth = P[i,1], prices = prices.num[i,])$weights
        P[i+1,1]      = (w.ew.am[i,]%*%returns.num[i,] + 1) * P[i,1] - sum(abs(w.ew.am[i,]-w.ew.am[i-1,]) * P[i,1] * fee)   # Calculate Portfolioreturn (+1 = gross return) times the current portfolio value - costs
        
        w.mvcmv.am[i,]  = mv.opt(a.m[i,], sig.n[[i]], gamma = gam, method = "cmv", mu.p = mu.p, wealth = P[i,2], prices = prices.num[i,])$weights
        P[i+1,2]      = (w.mvcmv.am[i,]%*%returns.num[i,] + 1) * P[i,2]- sum(abs(w.mvcmv.am[i,]-w.mvcmv.am[i-1,]) * P[i,2] * fee)
        
        w.mvgmv.am[i,]  = mv.opt((a.m[i,]), sig.n[[i]], method = "gmv", wealth = P[i,3], prices = prices.num[i,])$weights
        P[i+1,3]      = (w.mvgmv.am[i,]%*%returns.num[i,] + 1) * P[i,3]- sum(abs(w.mvgmv.am[i,]-w.mvgmv.am[i-1,]) * P[i,3] * fee)
        
        w.mvsr.am[i,]   = mv.opt((a.m[i,]), sig.n[[i]], method = "sr", wealth = P[i,4], prices = prices.num[i,])$weights
        P[i+1,4]      = (w.mvsr.am[i,]%*%returns.num[i,] + 1) * P[i,4]- sum(abs(w.mvsr.am[i,]-w.mvsr.am[i-1,]) * P[i,4] * fee)
        
        w.gmmf.am[i,]   = gmm.opt((a.m[i,]), sig.n[[i]], method = "f", wealth = P[i,5], prices = prices.num[i,])$weights
        P[i+1,5]      = (w.gmmf.am[i,]%*%returns.num[i,] + 1) * P[i,5]- sum(abs(w.gmmf.am[i,]-w.gmmf.am[i-1,]) * P[i,5] * fee)
        
        w.gmmsc.am[i,]  = gmm.opt((a.m[i,]), sig.n[[i]], method = "sc", wealth = P[i,6], prices = prices.num[i,])$weights
        P[i+1,6]      = (w.gmmsc.am[i,]%*%returns.num[i,] + 1) * P[i,6]- sum(abs(w.gmmsc.am[i,]-w.gmmsc.am[i-1,]) * P[i,6] * fee)
        
        w.licmv.am[i,]  = li.opt((a.m[i,]), sig.n[[i]], TV = TV.hat[i,], method = "cmv", r = mu.p, f = f, wealth = P[i,7], prices = prices.num[i,])$weights
        P[i+1,7]      = (w.licmv.am[i,]%*%returns.num[i,] + 1) * P[i,7]- sum(abs(w.licmv.am[i,]-w.licmv.am[i-1,]) * P[i,7] * fee)
        
        w.licvar.am[i,] = li.opt((a.m[i,]), sig.n[[i]], TV = TV.hat[i,], method = "cvar", r = mu.p, f = f, n = 20000, window = M, alpha = alpha, data = returns[ind[i]:nrow(returns),], wealth = P[i,8], prices = prices.num[i,])$weights
        P[i+1,8]      = (w.licvar.am[i,]%*%returns.num[i,] + 1) * P[i,8]- sum(abs(w.licvar.am[i,]-w.licvar.am[i-1,]) * P[i,8] * fee)
        
        w.cvar.am[i,]   = cvar.opt(a.m[i,], sig.n[[i]], alpha = alpha, n = 20000, M = M, data = returns[ind[i]:nrow(returns),], wealth = P[i,9], prices = prices.num[i,])$weights
        P[i+1,9]      = (w.cvar.am[i,]%*%returns.num[i,] + 1) * P[i,9]- sum(abs(w.cvar.am[i,]-w.cvar.am[i-1,]) * P[i,9] * fee)
      }  
      end.time.am = Sys.time()
      runtime.am = end.time.am-start.time.am
      print(runtime.am)
      ################################################
      ### 2. Optimize for geometric mean estimator ###
      ################################################
      
      start.time.gm = Sys.time()
      for(i in 2:length(ind)){
        
        w.mvcmv.gm[i,]  = mv.opt((g.m[i,]), sig.n[[i]], gamma = gam, method = "cmv", mu.p = mu.p, wealth = P[i,10], prices = prices.num[i,])$weights
        P[i+1,10]     = (w.mvcmv.gm[i,]%*%returns.num[i,] + 1) * P[i,10]- sum(abs(w.mvcmv.gm[i,]-w.mvcmv.gm[i-1,]) * P[i,10] * fee)
        
        w.mvgmv.gm[i,]  = mv.opt((g.m[i,]), sig.n[[i]], method = "gmv", wealth = P[i,11], prices = prices.num[i,])$weights
        P[i+1, 11]    = (w.mvgmv.gm[i,]%*%returns.num[i,] + 1) * P[i,11]- sum(abs(w.mvgmv.gm[i,]-w.mvgmv.gm[i-1,]) * P[i,11] * fee)
        
        w.mvsr.gm[i,]   = mv.opt((g.m[i,]), sig.n[[i]], method = "sr", wealth = P[i,12], prices = prices.num[i,])$weights
        P[i+1, 12]    = (w.mvsr.gm[i,]%*%returns.num[i,] + 1) * P[i,12]- sum(abs(w.mvsr.gm[i,]-w.mvsr.gm[i-1,]) * P[i,12] * fee)
        
        w.gmmf.gm[i,]   = gmm.opt((g.m[i,]), sig.n[[i]], method = "f", wealth = P[i,13], prices = prices.num[i,])$weights
        P[i+1, 13]    = (w.gmmf.gm[i,]%*%returns.num[i,] + 1) * P[i,13]- sum(abs(w.gmmf.gm[i,]-w.gmmf.gm[i-1,]) * P[i,13] * fee)
        
        w.gmmsc.gm[i,]  = gmm.opt((g.m[i,]), sig.n[[i]], method = "sc", wealth = P[i,14], prices = prices.num[i,])$weights
        P[i+1, 14]    = (w.gmmsc.gm[i,]%*%returns.num[i,] + 1) * P[i,14]- sum(abs(w.gmmsc.gm[i,]-w.gmmsc.gm[i-1,]) * P[i,14] * fee)
        
        w.licmv.gm[i,]  = li.opt((g.m[i,]), sig.n[[i]], TV = TV.hat[i,], method = "cmv", r = mu.p, f = f, wealth = P[i,15], prices = prices.num[i,])$weights
        P[i+1,15]     = (w.licmv.gm[i,]%*%returns.num[i,] + 1) * P[i,15]- sum(abs(w.licmv.gm[i,]-w.licmv.gm[i-1,]) * P[i,15] * fee)
        
        w.licvar.gm[i,] = li.opt((g.m[i,]), sig.n[[i]], TV = TV.hat[i,], method = "cvar", r = mu.p, f = f, n = 20000, window = M, alpha = alpha, data = returns[ind[i]:nrow(returns),], wealth = P[i,16], prices = prices.num[i,])$weights
        P[i+1,16]     = (w.licvar.gm[i,]%*%returns.num[i,] + 1) * P[i,16]- sum(abs(w.licvar.gm[i,]-w.licvar.gm[i-1,]) * P[i,16] * fee)
        
        w.cvar.gm[i,]   = cvar.opt(g.m[i,], sig.n[[i]], alpha = alpha, n = 20000, M = M, data = returns[ind[i]:nrow(returns),], wealth = P[i,17], prices = prices.num[i,])$weights
        P[i+1,17]     = (w.cvar.gm[i,]%*%returns.num[i,] + 1) * P[i,17]- sum(abs(w.cvar.gm[i,]-w.cvar.gm[i-1,]) * P[i,17] * fee)
      }  
      end.time.gm = Sys.time()
      runtime.gm = end.time.gm-start.time.gm
      print(runtime.gm)
      #############################################
      ### 3. Optimize for Bayes-Stein estimator ###
      #############################################
      
      start.time.bs = Sys.time()
      for(i in 2:length(ind)){
        
        w.mvcmv.bs[i,]  = mv.opt((bs.m[i,]), sig.bs[[i]], gamma = gam, method = "cmv", mu.p = mu.p, wealth = P[i,18], prices = prices.num[i,])$weights
        P[i+1,18]     = (w.mvcmv.bs[i,]%*%returns.num[i,] + 1) * P[i,18] - sum(abs(w.mvcmv.bs[i,]-w.mvcmv.bs[i-1,]) *  P[i,18] * fee)
        
        w.mvgmv.bs[i,]  = mv.opt((bs.m[i,]), sig.bs[[i]], method = "gmv", wealth = P[i,19], prices = prices.num[i,])$weights
        P[i+1,19]     = (w.mvgmv.bs[i,]%*%returns.num[i,] + 1) * P[i,19]- sum(abs(w.mvgmv.bs[i,]-w.mvgmv.bs[i-1,]) * P[i,19] * fee)
        
        w.mvsr.bs[i,]   = mv.opt((bs.m[i,]), sig.bs[[i]], method = "sr", wealth = P[i,20], prices = prices.num[i,])$weights
        P[i+1,20]     = (w.mvsr.bs[i,]%*%returns.num[i,] + 1) * P[i,20]- sum(abs(w.mvsr.bs[i,]-w.mvsr.bs[i-1,]) * P[i,20] * fee)
        
        w.gmmf.bs[i,]   = gmm.opt((bs.m[i,]), sig.bs[[i]], method = "f", wealth = P[i,21], prices = prices.num[i,])$weights
        P[i+1,21]     = (w.gmmf.bs[i,]%*%returns.num[i,] + 1) * P[i,21]- sum(abs(w.gmmf.bs[i,]-w.gmmf.bs[i-1,]) * P[i,21] * fee)
        
        w.gmmsc.bs[i,]  = gmm.opt((bs.m[i,]), sig.bs[[i]], method = "sc", wealth = P[i,22], prices = prices.num[i,])$weights
        P[i+1,22]     = (w.gmmsc.bs[i,]%*%returns.num[i,] + 1) * P[i,22]- sum(abs(w.gmmsc.bs[i,]-w.gmmsc.bs[i-1,]) * P[i,22] * fee)
        
        w.licmv.bs[i,]  = li.opt((bs.m[i,]), sig.bs[[i]], TV = TV.hat[i,], method = "cmv", r = mu.p, f = f, wealth = P[i,23], prices = prices.num[i,])$weights
        P[i+1,23]     = (w.licmv.bs[i,]%*%returns.num[i,] + 1) * P[i,23]- sum(abs(w.licmv.bs[i,]-w.licmv.bs[i-1,]) * P[i,23] * fee)
        
        w.licvar.bs[i,] = li.opt((bs.m[i,]), sig.bs[[i]], TV = TV.hat[i,], method = "cvar", r = mu.p, f = f, n = 20000, window = M, alpha = alpha, data = returns[ind[i]:nrow(returns),], wealth = P[i,24], prices = prices.num[i,])$weights
        P[i+1,24]     = (w.licvar.bs[i,]%*%returns.num[i,] + 1) * P[i,24] - sum(abs(w.licvar.bs[i,]-w.licvar.bs[i-1,]) * P[i,24] * fee)
        
        w.cvar.bs[i,]  = cvar.opt(bs.m[i,], sig.bs[[i]], alpha = alpha, n = 20000, M = M, data = returns[ind[i]:nrow(returns),], wealth = P[i,25], prices = prices.num[i,])$weights
        P[i+1, 25]   = (w.cvar.bs[i,]%*%returns.num[i,] + 1) * P[i,25]- sum(abs(w.cvar.bs[i,]-w.cvar.bs[i-1,]) * P[i,25] * fee)
      } 
      end.time.bs = Sys.time()
      runtime.bs = end.time.bs-start.time.bs
      print(runtime.bs)
      
      runtime = sum(runtime.am, runtime.gm, runtime.bs)
      print(runtime)

### For convenience, wrap the weights into a large list and convert them to numeric
#Care, that this is a capital letter W
    W = lapply(fin.names, get)
    
    names(W) = fin.names
    #all sum of weights should be one, let's check
      test.one = sapply(W, FUN = function(x){
        sum(x)
      })
      
    
    if(!all(round(test.one)==765)){warning("Care - Some Portfolio Weights do not sum up to one! These are: ", paste0(fin.names[-which(round(test.one)==765)], " "))}
    ### if there is no output, proceed
      tck = colnames(returns)
      W = lapply(W, FUN = as.matrix)
      for(i in 1:length(W)){
        colnames(W[[i]]) = tck
      }
    #need returns in numeric form for matrix operations; Furthermore we need to cut the first M observations, as these were used to estimate (weights start at time M+1)
      returns.num = as.matrix(returns[(M+1):nrow(returns),])
      dim(returns.num)
      #should be 885-M   75

#### Results ####

#Now we are interested in obtaining the true portfolio returns
#Care, that this is a capital letter P  
      P.r = list()
      for(i in 1:ncol(P)){
        P.r[[i]] = (P[-1,i] / P[-nrow(P),i]) - 1
        
      }
      names(P.r) = fin.names[-c(10,19)]
      #remove EW (GM) and EW (BS) as they are identical to EW (AM) due to non-parametric
      W = W[-c(10,19)]
      names.new = names.new[-c(10,19)]
      #Save data, so it can be used in other scripts, which may be outsourced for quantlets
      save(file = "./res/Results.RData", list = c("P", "P.r", "W", "returns.num", "names.new", "gam"))