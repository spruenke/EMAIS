#### Estimation of Parameters (Functions) ####

### Mean-Variance
### Based on Sample of dim T x N (i.e.: T Observations for N Assets)

par.est = function(x, M = 60, lam = 1, method = "ar"){ #x = matrix/dataframe of assets (T obs., N assets), M is window size for estimation
  data = x[1:M,]  
  switch(method,
         ar = { #arithmetic mean
           mu.hat = colMeans(data)
           sig.hat = cov(data)
         },
         gm = { #geometric mean
           g.r = 1+data #create gross return
           mu.hat = apply(g.r, 2, FUN = function(x) {((prod(x))^(1/M))-1})
           sig.hat = cov(data)
         },
         bs = { #bayes-stein shrinkage, following Jorion(1986)
           N = ncol(data)
           data = as.matrix(data)
           sig = (M-1)/(M-N-2) * cov(data)
           mu = colMeans(data)
           one = rep(1, nrow(sig))
           w.rel = as.vector(solve(sig)%*%one)/as.vector(t(one)%*%(solve(sig))%*%one) #weights in minimum variance (global) framework
           mu.min = as.vector(t(w.rel)%*%mu) #expected return of minimum variance portfolio
           phi = ((N+2)/((N+2)+M*t((mu-mu.min))%*%solve(sig)%*%(mu-mu.min)))
           if(phi <= 0){
             phi = 0} else if(phi >= 1){
               phi = 1
             } else {
               phi = phi
             }
           phi = as.vector(phi)
           mu.hat = (1-phi)*mu + phi*mu.min
           
           #factors for covariance matrix
           nom = as.vector(t(one)%*%one)
           denom = as.vector(t(one)%*%solve(sig)%*%one)
           sig.hat = sig*(1+(1/(M+lam))) + (lam/(M*(M+1+lam)))*(nom/denom)
         }
  )
  return(list("mu" = mu.hat, "var" = sig.hat, "method" = method, "window" = M))
}

### TV Estimation for portfolio optimization wich takes trading volume into account
### Based on Sample of dim T x N (i.e.: T Observations for N Assets)

tv.est = function(x, M = 60, method = "median"){
  data = x[1:M,]
  switch(method,
         median = { #classic LIBRO following Trimborn et al. (2018)
           tv.hat = apply(data, 2, median, na.rm = T)
         },
         mean = { #own approach, using mean instead of median
           tv.hat = apply(data, 2, mean, na.rm = T)
         }
         
  )
  tv.hat[which(is.na(tv.hat)==T)] = min(tv.hat, na.rm = T) #give NA values the least "possible" value of the data
  return(list("TV" = tv.hat, "method" = method, "window" = M))
}