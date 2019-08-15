### LIquidity Bounded Risk return Optimization (LIBRO)
lib = c("quadprog")
if(!(lib %in% installed.packages())){install.packages(lib)}
lapply(lib, library, character.only = T)
li.opt = function(mu, sigma, TV, f = 0.0001, M = 1, r = 0, method = "cmv", data = NULL, n = 10000, alpha = 0.05, window = 60){ #mu and sigma are estimators, TV is trading volume, 
                                                        #f is maximum ratio for positions (can be scalar or vector), 
                                                        #M is wealth, r is target return
                                                        #data are historic returns
  nn = length(mu)
  one = rep(1, length = nn) #vector of ones
  switch(method,
      cmv = {
          min.1 = diag(rep(-1, length = nn)) #negative Identity matrix
          A = cbind(one, mu, min.1) #matrix for quadprog constraints
          bvec = c(1, r, -(TV*f/M)) #target values for constraint
          d = rep(0, length = nn) #quadprog has some kind of part with d's in it which we want to ignore
          sol = solve.QP(Dmat = sigma, dvec = d, Amat = A, bvec = bvec) #meq is left zero because we can satisfy the "sum-to-one" constraint easily by division later
          w.rel = sol$solution
      },
      cvar = {
        #Pretty similar to cvar function itself
        #to fulfill constraint calculate a vector of boundaries (LIBRO approach)
        #use this vector to delete all combinations which do not meet these
        #minimize along the rest
        if(is.null(data)){stop("historic returns must be provided")}
        else{
            
            #upper bound for weights are trading volumes times f divided by M
            w.max = TV*f/M
            w.sim = list() #placeholder for simulated weights
            for(j in 1:n){
              w.sim[[j]] = runif(length(mu))
            }
            cond = lapply(w.sim, FUN = function(x) all(x<w.max)) #condition that weights are smaller then boundary
            w.sim = w.sim[unlist(cond)] #only keep those which fulfil the condition
            w.sim = lapply(w.sim, FUN = function(x){ #scale back such that the weights sum up to one
              x/sum(x)
            })
            data.r = as.matrix(data[1:window,])
            p.r.sim = lapply(w.sim, FUN = function(x){ #for each simulated set of weights the true historic portfolio returns
              data.r%*%x 
            })
            
            #cdf.sim = lapply(p.r.sim, FUN = ecdf) #empirical cumulative distribution function for each set
            var = c() #spaceholder vector for the VaR's
            for(j in 1:length(p.r.sim)){
              var[j] = quantile(p.r.sim[[j]], alpha) #evaluate distribution up to alpha (= VaR)
            }
            cvar = 1/(1-alpha)*(var - 1) #transformation can be easily done
            opt = which(cvar==min(cvar))
            opt.weights = w.sim[[opt]]
            w.rel = opt.weights #scaling not necessary because done before
        }
      }
  )
  w.rel = w.rel/sum(w.rel) #this way, weights sum up to one (rescaling, if necessary <=> if already met through constraint, this changes nothing!)
  return(list("weights" = w.rel, "expected return" = t(w.rel)%*%mu, "expected variance" = t(w.rel)%*%sigma%*%w.rel, method = paste0("LIBRO-", method), "length" = n))
}