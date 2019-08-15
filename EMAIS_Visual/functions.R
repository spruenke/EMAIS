psi = function(ret, w, gamma = 1){
  ceq = mean(ret) - (gamma/2) * var(ret)
  sr = (mean(ret)/sd(ret))
  asr = sr * (1 + (skewness(ret)/6)*sr - ((kurtosis(ret)-3)/24)*(sr)^2) #-3 for excess kurtosis!
  wn = apply(w, 2, FUN = function(x){
    sum(abs(diff(x)))
  })
  to = 1/length(ret) * sum(wn)
  psi.e = c(ceq,sr,asr,to)
  names(psi.e) = c("CEQ", "SR", "ASR", "TO")
  return(psi.e)
}