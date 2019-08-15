### Naive Portfolio (Equally Weighted)

ew = function(mu, sigma){
  w.rel = rep(1/length(mu), length(mu))
  
  return(list("weights" = w.rel, "expected return" = t(w.rel)%*%mu, "expected variance" = t(w.rel)%*%sigma%*%w.rel, "method" = "EW", "length" = length(mu)))
}
