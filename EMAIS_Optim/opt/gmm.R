### Geometric Mean Maximization according to Javier Estrada (2010)
gmm.opt = function(mu, sigma, method = "f", wealth, prices){    
    one = rep(1, length(mu))
    init.v = rep((1/length(mu)), length(mu))

      L = function(x, mu, sigma){
        x.1 = x
       abc = -1*(exp(log(1+(t(x.1)%*%mu))-(t(x.1)%*%sigma%*%x.1)/(2*(1+(t(x.1)%*%mu))^2))-1)
       return(abc)
    }
    switch(method,
    f = { #unconstrained
    ppp = optim(init.v, L, mu = mu, sigma = sigma, method = "Nelder-Mead")
    w = ppp$par
    },
    sc = { #shortsale-constrained
      ppp = optim(init.v, L, mu = mu, sigma = sigma, method = "L-BFGS-B", lower = 0)
      w = ppp$par
    }
    )
    w.rel = w/sum(w)
    
    #check for correct increments and change weights in a way that only integers of assets will be bought
    w.rel = floor(w.rel * wealth / prices) * prices / wealth
    
    return(list("weights" = w.rel, "exprected return" = t(w.rel)%*%mu, "expected variance" = t(w.rel)%*%sigma%*%w.rel, "method" = paste0("GMM-", method), "length" = length(mu)))
}