### CVaR following Trimborn et. al (2018), Petukhina et. al (2018), ... (insert more sources!)
### load Rcpp library!!
### sourceCpp("src/colCvarCPP.cpp")

cvar.opt = function(mu, sigma, alpha = 0.05, n = 10000, data, M, wealth, prices){ #n is parameter for how many simulated weights will be drawn => the more the better but also the more time it will cost; data should be true historic returns but already considered with correct window M!
  w.sim = matrix(runif(length(mu) * n), ncol = n)
  w.sim = w.sim / colSums(w.sim)
  data.r = as.matrix(data[1:M,])
  p.r.sim = data.r %*% w.sim
  
  cvar = colCVar(p.r.sim, nrow(p.r.sim), ncol(p.r.sim), alpha)
  
  opt = which(cvar==max(cvar))
  opt.weights = w.sim[,opt]
  w.rel = opt.weights #scaling not necessary because done before
  method = "CVaR"
  
  #check for correct increments and change weights in a way that only integers of assets will be bought
  w.rel = floor(w.rel * wealth / prices) * prices / wealth
  
  return(list("weights" = w.rel, "expected return" = t(w.rel)%*%mu, "expected variance" = t(w.rel)%*%sigma%*%w.rel, "method" = method, "length" = length(mu)))
}
