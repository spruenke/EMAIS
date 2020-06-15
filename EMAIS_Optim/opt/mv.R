#Sample-based Mean-Variance Portfolio (Markowitz)
#See DeMiguel Paper
#See Modern Portfolio Theory and Investment Analysis


mv.opt = function(mu, sigma, gamma = 1, method = "cmv", mu.p = NULL, wealth, prices){
  
  #provide estimators for Target Return, Global Minimum Variance, Maximum Sharpe Ratio
  switch(method,
         cmv = { #optimal mean-variance portfolio, minimizing variance constrained by target return
             if(is.null(mu.p)){stop("For method cmv the target return mu.p must be provided")}
             else{  
               one = rep(1, length(mu))
               a = t(one)%*%(solve(sigma))%*%one
               b = t(one)%*%(solve(sigma))%*%mu
               d = t(mu)%*%(solve(sigma))%*%mu
               delta = a*d-b*b
               lambda.1 = (d-b*mu.p)/delta #the two lambdas and a, b and d can be obtained from applying the two constraints
               lambda.2 = (a*mu.p-b)/delta
               
               w.rel = (solve(sigma))%*%(lambda.1*one + lambda.2*mu) #lagrange yields this equation
               
               }   
         },
         gmv = { #optimal portfolio with global minimum variance => ignoring mean
           one = rep(1, nrow(sigma))
           #denom = t(one)%*%(solve(sigma))%*%one
           w.rel = (solve(sigma)%*%one)
           w.rel = w.rel/sum(w.rel)#one can derive this by using lagrangian - this is the same as using t(one)%*%solve(sigma)%*%one as denominator
           
         },
         sr = { #optimal portfolio, maximizing the sharpe ratio
              #this follows MPT and IA (Book)  
            
              w = solve(sigma)%*%mu
              w.rel = w/(sum((w)))
         }

         )
  
  #check for correct increments and change weights in a way that only integers of assets will be bought
  w.rel = floor(w.rel * wealth / prices) * prices / wealth
  
  return(list("weights" = w.rel, "expected return" = t(w.rel)%*%mu, "expected variance" = t(w.rel)%*%sigma%*%w.rel, "method" = method, "length" = length(mu)))
}