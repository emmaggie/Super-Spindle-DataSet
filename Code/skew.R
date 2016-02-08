skew=function(x){
  x=as.numeric(x)
  m.3 = sum((x-mean(x, na.omit=TRUE))^3)/length(x)
  s.3 = sqrt(var(x))^3
  skew=m.3/s.3
  #print(skew)
  SE=sqrt(6/length(x))
  #?print(SE)
  p.value=1-pt((skew/SE),length(x)-2)
  return(c(p.value=p.value,skew=skew))
}
