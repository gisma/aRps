derivParam<-function(nc,param=c("t","es","e","rh","td","p","rn")){
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
  
  #filename = '/home/creu/progs/opengrads/data/stol_d1_ARP.nc'
  #nc <- open.ncdf( filename )
  # get pressure (Pa)
  #p = get.var.ncdf( nc, "P", start=c(1,1,2,1), count=c(5,5,1,1) )
  p = get.var.ncdf( nc, "P")
  # get potentialk Temperatur in K
  pt = get.var.ncdf( nc, "PT" )
  # get Water Vapor Mixing Ratio (g/kg) 
  qv <- get.var.ncdf( nc, "QV" )
  # get rain in mm
  rn = get.var.ncdf( nc, "RAING" )
    
  # calculate dry bulb temperature from potential temperature using exner function
  # first calculate Exner pressure (e_p)
  e_p = (p / 100000) ^ (287.058 / 1005)
  # calculate dry bulb temperature using exner pressure
  # Air Temp in ° C
  tc = e_p * pt -273.15
  # Air Temp in Kelvin (K)
  tk = e_p * pt

  # Computes the saturation vapour pressure over water at temperature t (K).
  # es in hPas, t in K
  if (any(param=="es")||any(param=="rh")){
    a0 = 23.832241 - 5.02808 * log10(tk)
    a1 = 0.00000013816 * 10 ^ (11.344 - 0.0303998 * tk)
    a2 = 0.0081328 * 10 ^ (3.49149 - 1302.8844 / tk)
    es = (10 ^ (a0 - a1 + a2 - 2949.076 / tk))
    
  # alternative calculation slightly less accurate but faster
  # es= 6.107 * 10 ^ (7.5 * tc / (235 + tc))
  
  # alternative calculation slightly less accurate but faster    
  #  T > 0 above water
  # a =  7.5
  # b = 237.3
  # T < 0 above water (dewpoint)
  #  a = 7.6
  # b = 240.7 
  # T < 0 above ice Eis (freezing point)
  # a = 9.5
  # b = 265.5 
  # Saturation vapor pressure alternative calculatio (hPa)
  # es = 6.1078 * 10^((a*tc)/(b+tc))
  
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
  # T> 0 above water is calculated in two steps 
  a =  7.5
  b = 237.3
  #für T < 0 über Wasser (Taupunkt)
  #  a = 7.6
  #b = 240.7 
  #für T < 0 über Eis (Frostpunkt)
  # a = 9.5
  #  b = 265.5 
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
  
  result=list()
  for (i in 1:length(param)){
    result[[i]]=get(param[i])
  }
  names(result)=param
  return (result)  
}
