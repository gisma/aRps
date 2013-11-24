derivParam<-function(x,param=c("t","es","e","rh")){
    require(ncdf)
    p = get.var.ncdf( x, "P")
    pt = get.var.ncdf( x, "PT")  
   
    t =(pt/(1000/p))^0.287 #air temperature
    
    if (any(param=="es")||any(param=="rh")){
      es = 6.107 * (10^((7.5 * t)/(235 + t)))# water saturation pressure (E)
    }
    
    if (any(param=="e")||any(param=="rh")){
      qv = get.var.ncdf( x, "QV")
      e = (p * qv) / 622 # water partial pressure (e)
      if (any(param=="rh")){
        rh=(e/es)*1000 #relative air humidity
      }
    }
    
  result=list()
  for (i in 1:length(param)){
    result[[i]]=get(param[i])
  }
  names(result)=param
  return (result)  
}