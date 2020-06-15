### LIquidity Bounded Risk return Optimization (LIBRO)
lib = c("quadprog")
if(!(lib %in% installed.packages())){install.packages(lib)}
lapply(lib, library, character.only = T)
li.opt = function(mu, sigma, TV, f = 0.0001, M = 1, r = 0, method = "cmv", data = NULL, n = 10000, alpha = 0.05, window = 60, prices, wealth){ #mu and sigma are estimators, TV is trading volume, 
                                                        #f is maximum ratio for positions (can be scalar or vector), 
                                                        #M is wealth, r is target return
                                                        #data are historic returns
  nn = length(mu)
  one = rep(1, length = nn) #vector of ones
  M = wealth
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
          #upper bound for weights are trading volumes times f divided by M
          w.max = TV*f/M
          #w.sim = matrix(NA, ncol = n, nrow = length(mu))
          #for(z in 1:length(mu)){
          #    w.sim[z,] = runif(n, -0, w.max[z])
          #}
          w.sim  = matrix(runif(n * length(mu)), ncol = n)
          w.sim = w.sim / colSums(w.sim)
          
          #cond = apply(w.sim, 2,  FUN = function(x) all(x<w.max)) #condition that weights are smaller then boundary
          #cond = which(cond == T)
          #if(length(cond) == 0){
          #    w.sim = w.max / sum(w.max)
          #    w.sim = cbind(w.sim, w.sim, w.sim)
          #  } else {
          #w.sim = w.sim[,cond]
          #  }
          data.r = as.matrix(data[1:window,])
          p.r.sim = data.r%*%w.sim 
            
            cvar = colCVar(p.r.sim, nrow(p.r.sim), ncol(p.r.sim), alpha)
            
            opt = which(cvar==max(cvar))
            opt.weights = w.sim[,opt]
            opt.weights[which(opt.weights > w.max)] = w.max[which(opt.weights > w.max)]
            #if(ncol(opt.weights) > 1){
            #  opt.weights = opt.weights[,1]
            #}
            w.rel = opt.weights #scaling not necessary because done before
            method = "CVaR"
            w.rel = opt.weights #scaling not necessary because done before
        }
      }
  )
  w.rel = w.rel/sum(w.rel) #this way, weights sum up to one (rescaling, if necessary <=> if already met through constraint, this changes nothing!)
  
  #check for correct increments and change weights in a way that only integers of assets will be bought
  w.rel = floor(w.rel * wealth / prices) * prices / wealth
  return(list("weights" = w.rel, "expected return" = t(w.rel)%*%mu, "expected variance" = t(w.rel)%*%sigma%*%w.rel, method = paste0("LIBRO-", method), "length" = n))
}