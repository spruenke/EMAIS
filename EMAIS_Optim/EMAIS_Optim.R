### Care: The calculation time of this script may amount to several hours!

rm(list = ls())
graphics.off()

#set proper working direcotry
dir = "D:/Dropbox/Studium/B.Sc. Economics/Bachelorarbeit/EMAIS_git/EMAIS_Optim"
setwd(dir)
load("final_set.RData")
load("parameters.RData")

#load optimization functions
opt.rout = list.files("./opt", full.names = T)
lapply(opt.rout, source)

#choose a gamma for MV (risk aversion parameter) - defaults to 1
gam = 1
#choose target return
mu.p = 0.00026 #(remember, that daily returns are far lower than annual returns), approx 10% p.a.
#choose max-ratio f
f = 0.001
#choose VaR alpha
alpha = 0.05

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

#################################################
### 1. Optimize for arithemtic mean estimator ###
#################################################
    start.time.am = Sys.time()
    for(i in 1:nrow(a.m)){
      w.ew.am[i,]     = ew(a.m[i,], sig.n[[i]])$weights
      w.mvcmv.am[i,]  = mv.opt(a.m[i,], sig.n[[i]], gamma = gam, method = "cmv", mu.p = mu.p)$weights
      w.mvgmv.am[i,]  = mv.opt((a.m[i,]), sig.n[[i]], method = "gmv")$weights
      w.mvsr.am[i,]   = mv.opt((a.m[i,]), sig.n[[i]], method = "sr")$weights
      w.gmmf.am[i,]   = gmm.opt((a.m[i,]), sig.n[[i]], method = "f")$weights
      w.gmmsc.am[i,]  = gmm.opt((a.m[i,]), sig.n[[i]], method = "sc")$weights
      w.licmv.am[i,]  = li.opt((a.m[i,]), sig.n[[i]], TV = TV.hat[i,], method = "cmv", r = mu.p, f = f)$weights
      w.licvar.am[i,] = li.opt((a.m[i,]), sig.n[[i]], TV = TV.hat[i,], method = "cvar", r = mu.p, f = f, n = 15000, window = M, alpha = alpha, data = returns[i:nrow(returns),])$weights
      w.cvar.am[i,] = cvar.opt(a.m[i,], sig.n[[i]], alpha = alpha, n = 15000, M = M, data = returns[i:nrow(returns),])$weights
    }  
    end.time.am = Sys.time()
    runtime.am = end.time.am-start.time.am
    print(runtime.am)
    
################################################
### 2. Optimize for geometric mean estimator ###
################################################

    start.time.gm = Sys.time()
    for(i in 1:nrow(a.m)){
      w.ew.gm[i,]     = ew((g.m[i,]), sig.n[[i]])$weights
      w.mvcmv.gm[i,]  = mv.opt((g.m[i,]), sig.n[[i]], gamma = gam, method = "cmv", mu.p = mu.p)$weights
      w.mvgmv.gm[i,]  = mv.opt((g.m[i,]), sig.n[[i]], method = "gmv")$weights
      w.mvsr.gm[i,]   = mv.opt((g.m[i,]), sig.n[[i]], method = "sr")$weights
      w.gmmf.gm[i,]   = gmm.opt((g.m[i,]), sig.n[[i]], method = "f")$weights
      w.gmmsc.gm[i,]  = gmm.opt((g.m[i,]), sig.n[[i]], method = "sc")$weights
      w.licmv.gm[i,]  = li.opt((g.m[i,]), sig.n[[i]], TV = TV.hat[i,], method = "cmv", r = mu.p, f = f)$weights
      w.licvar.gm[i,] = li.opt((g.m[i,]), sig.n[[i]], TV = TV.hat[i,], method = "cvar", r = mu.p, f = f, n = 15000, window = M, alpha = alpha, data = returns[i:nrow(returns),])$weights
      w.cvar.gm[i,]  = cvar.opt(g.m[i,], sig.n[[i]], alpha = alpha, n = 15000, M = M, data = returns[i:nrow(returns),])$weights
    }  
    end.time.gm = Sys.time()
    runtime.gm = end.time.gm-start.time.gm
    print(runtime.gm)
    
#############################################
### 3. Optimize for Bayes-Stein estimator ###
#############################################

    start.time.bs = Sys.time()
    for(i in 1:nrow(a.m)){
      w.ew.bs[i,]     = ew((bs.m[i,]), sig.n[[i]])$weights
      w.mvcmv.bs[i,]  = mv.opt((bs.m[i,]), sig.bs[[i]], gamma = gam, method = "cmv", mu.p = mu.p)$weights
      w.mvgmv.bs[i,]  = mv.opt((bs.m[i,]), sig.bs[[i]], method = "gmv")$weights
      w.mvsr.bs[i,]   = mv.opt((bs.m[i,]), sig.bs[[i]], method = "sr")$weights
      w.gmmf.bs[i,]   = gmm.opt((bs.m[i,]), sig.bs[[i]], method = "f")$weights
      w.gmmsc.bs[i,]  = gmm.opt((bs.m[i,]), sig.bs[[i]], method = "sc")$weights
      w.licmv.bs[i,]  = li.opt((bs.m[i,]), sig.bs[[i]], TV = TV.hat[i,], method = "cmv", r = mu.p, f = f)$weights
      w.licvar.bs[i,] = li.opt((bs.m[i,]), sig.bs[[i]], TV = TV.hat[i,], method = "cvar", r = mu.p, f = f, n = 15000, window = M, alpha = alpha, data = returns[i:nrow(returns),])$weights
      w.cvar.bs[i,]  = cvar.opt(bs.m[i,], sig.bs[[i]], alpha = alpha, n = 15000, M = M, data = returns[i:nrow(returns),])$weights
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
      P = lapply(W, FUN = function(x){
        diag(x%*%t(returns.num)) #this matrix operation calculates the weighted sum of returns, but only diagonal elements of the resulting matrix are of interest
      })
      names(P) = fin.names
    #remove EW (GM) and EW (BS) as they are identical to EW (AM) due to non-parametric
      P = P[-c(10,19)] 
      W = W[-c(10,19)]
      names.new = names.new[-c(10,19)]
    #Save data, so it can be used in other scripts, which may be outsourced for quantlets
      save(file = "Results.RData", list = c("P", "W", "returns.num", "names.new", "gam"))