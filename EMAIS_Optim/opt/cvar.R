### CVaR following Trimborn et. al (2018), Petukhina et. al (2018), ... (insert more sources!)

cvar.opt = function(mu, sigma, alpha = 0.05, n = 10000, data, M){ #n is parameter for how many simulated weights will be drawn => the more the better but also the more time it will cost; data should be true historic returns but already considered with correct window M!
  w.sim = list() #placeholder for simulated weights
  for(j in 1:n){
    w.sim[[j]] = runif(length(mu))
  }
  w.sim = lapply(w.sim, FUN = function(x){ #scale back such that the weights sum up to one
    x/sum(x)
  })
  data.r = as.matrix(data[1:M,])
  p.r.sim = lapply(w.sim, FUN = function(x){ #for each simulated set of weights the true historic portfolio returns
    data.r%*%x 
  })
  
  #cdf.sim = lapply(p.r.sim, FUN = ecdf) #empirical cumulative distribution function for each set
  var = c()
  for(j in 1:n){
    var[j] = quantile(p.r.sim[[j]], alpha) #get VaR for j-th set of weights
  }
  for (j in 1:n){
    cvar[j] = mean(p.r.sim[[j]][p.r.sim[[j]] <= var[j]])
  }
  opt = which(cvar==min(cvar))
  opt.weights = w.sim[[opt]]
  w.rel = opt.weights #scaling not necessary because done before
  method = "CVaR"

  return(list("weights" = w.rel, "expected return" = t(w.rel)%*%mu, "expected variance" = t(w.rel)%*%sigma%*%w.rel, "method" = method, "length" = length(mu)))
}
