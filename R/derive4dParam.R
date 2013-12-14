derive4dParam<-function(nc,param=c("tc","td","tk","es","e","rh","pr","ep","u","v","w","ws","wd")){
  #
  # Copyright 2013 Hanna Meyer, and Chris Reudenbach
  #
  # This file is part of the aRps library for R and related languages.
  #
  # aRps is free software; you can redistribute it and/or modify
  # it under the terms of the GNU General Public License as published by
  # the Free Software Foundation; either version 2 of the License, or
  # (at your option) any later version.
  #
  # aRps is distributed in the hope that it will be useful,
  # but WITHOUT ANY WARRANTY; without even the implied warranty of
  # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  # GNU General Public License for more details.
  #
  # You should have received a copy of the GNU General Public License
  # along with RadioSonde; if not, write to the Free Software
  # Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
  #  
  
  filename = '/home/creu/progs/opengrads/data/stol_d1_ARP.nc'
  nc <- open.ncdf( filename )
  # get pressure (Pa)
  #p = get.var.ncdf( nc, "P", start=c(1,1,2,1), count=c(5,5,1,1) )
  p = get.var.ncdf( nc, "P")
  # get potentialk Temperatur in K
  pt = get.var.ncdf( nc, "PT")
  # get Water Vapor Mixing Ratio (g/kg) 
  qv <- get.var.ncdf( nc, "QV")
  # get u wind vector (m/s)
  u <- get.var.ncdf( nc, "U")    
  # get V wind vector (m/s)
  v <- get.var.ncdf( nc, "V")    
  # get V wind vector (m/s)
  w <- get.var.ncdf( nc, "W")   
  # do some pseudo destaggering
  uxdim=dim(u)[1]
  uydim=dim(u)[2]
  vxdim=dim(v)[1]
  vydim=dim(v)[2]
  ldim=dim(u)[3]
  tdim=dim(u)[4]
  u=slice (u, i=1:uxdim-1 ,j=1:uydim,k=1:ldim,l=1:tdim)
  v=slice (v, i=1:vxdim ,j=1:vydim-1,k=1:ldim,l=1:tdim)
  # calculate windspeed (m/s)
  ws=sqrt(u^2+v^2)
  # calculate winddirection in degree
  wd=180+atan2(u,v)*57.295
  
  # calculate dry bulb temperature from potential temperature using exner function
  # first calculate Exner pressure (e_p)
  
  ep = (p / 100000.0) ^ (287.058 / 1005.0)
  # calculate dry bulb temperature using exner pressure
  # Air Temp in ° C
  tc = ep * pt -273.15
  # Air Temp in Kelvin (K)
  tk = ep * pt
 
  
  # Computes the saturation vapour pressure over water at temperature t (K).
  # es in hPas, t in K
  if (any(param=="es")){
  #  a0 = 23.832241 - 5.02808 * log10(tk)
  #  a1 = 0.00000013816 * 10 ^ (11.344 - 0.0303998 * tk)
  #  a2 = 0.0081328 * 10 ^ (3.49149 - 1302.8844 / tk)
  #  es = (10 ^ (a0 - a1 + a2 - 2949.076 / tk))
    
  # alternative calculation 
  # a,b params for water, water dewpoint and ice must be set
  #  T > 0 above water
  a <- replace(tc, tc >= 0, 7.5)
  b <- replace(tc, tc >= 0,  237.3)
  # T < 0 above ice Eis (freezing point)
  a <- replace(a, tc <0, 9.5)
  b <- replace(b, tc <0,  285.5)
  # Saturation vapor pressure alternative calculatio (hPa)
  es = 6.1078 * 10^((a*tc)/(b+tc))
  
  }
  
  if (any(param=="e")){
    # Water vapor specific humidity qv (g/kg)
    # water vapour partial pressure e (Pa)
    # pressure p (Pa)
    # Ra specific gas constant air 287.06
    # Rw specifs constant water vapor  461.6
    # e = p/(Ra/Rw)*qv (Pa)
    # e = e/100 (hPa)
    e = (p/0.622 * qv)/100
  }

  if (any(param=="td")){
  #  Dew-point temprature (Td in C)  from vapor pressure (e) 
  # T> 0 above water
    a <- replace(tc, tc >= 0, 7.6)
    b <- replace(tc, tc >= 0,  240.7)
    # T < 0 above ice Eis (freezing point)
    a <- replace(a, tc <0, 9.5)
    b <- replace(b, tc <0,  285.5)
  v = log10(e/6.1078)
  # Dewpoint Temperature C
  td = b*v/(a-v)  
  
  #alterntive calculation dewpoint C
  #td = (243.5 * log(e/6.112)) / (17.67 - log(e/6.112))
  }
  
  if (any(param=="rh")){
      # calculate relative air humidity
        rh=(e/es)*100 
      }
  if (any(param=="pr")){
    # convert pressure from Pa to hPa
    pr=p/100.0
  }
  
  
  result=list()
  for (i in 1:length(param)){
    result[[i]]=get(param[i])
  }
  names(result)=param
  return (result)  
}
