### Naive Portfolio (Equally Weighted)

ew = function(mu, sigma, wealth, prices){
  w.rel = rep(1/length(mu), length(mu))
  #check for correct increments and change weights in a way that only integers of assets will be bought
  w.rel = floor(w.rel * wealth / prices) * prices / wealth
  
  return(list("weights" = w.rel, "expected return" = t(w.rel)%*%mu, "expected variance" = t(w.rel)%*%sigma%*%w.rel, "method" = "EW", "length" = length(mu)))
  
}
