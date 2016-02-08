kurtosis=function(x){
  m.4 = sum((x-mean(x, na.omit=TRUE))^4)/length(x)
  s.4 = (var(x))^2
  kurtosis = (m.4 / s.4) - 3
  
  SE=sqrt(24/length(x))
  #print(SE)
  p.value=1-pt((kurtosis/SE),length(x)-2)
  
  return(c(p.value=p.value,kurtosis=kurtosis))

}
