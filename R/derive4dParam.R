derive4dParam<-function(filename,param=c("tc","td","es","e","rh","pr","u","v","ws","wd")){
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
  
  ### derive4dParams extract the essential atmospheric variables from 
  ### the ARPS netCDF file.
  ### additionally it calculates the parameter dewpoint, water vapor pressure, 
  ### saturation water vapor pressure, dry bulb air temperature, relative humidity
  ### wind speed, wind direction
  ### it converts pressure from Pa to hPa and provides dry bulb air temparature also in Kelvin
  
  #filename = '/home/creu/progs/opengrads/data/stol_d1_ARP.nc'
  #filename= '/home/creu/Daten/ARPS/marburg_d1.nc'
  nc <- open.ncdf( filename )
  # get pressure (Pa)
  #p = get.var.ncdf( nc, "P", start=c(1,1,2,1), count=c(5,5,1,1) )
  
  wind=wind(nc)
  u=wind[[3]]
  writeLines('u vector done')
  v=wind[[4]]
  writeLines('v vector done')
  ws=wind[[1]]
  writeLines('windspeed done')
  wd=wind[[2]]
  writeLines('wind dir.  done')
  rm(wind)
  # Air Temp in ° C
  tc = tcelsius(nc)
  writeLines('temperature done')
  #var2nc(filename,'u',u,new=TRUE)
  
  # Computes the saturation vapour pressure over water at temperature t (K).
  if (any(param=="es")){
    es = satwatervapor(nc)
    writeLines('saturation vapour pressure done')
  }
  
  # water vapour partial pressure e (Pa)
  if (any(param=="e")){
    e =partwatervapor(nc)
    writeLines('partial vapour pressure done')
  }
  
  #  Dew-point temprature (Td in C)  from vapor pressure (e) 
  if (any(param=="td")){
    #  Dew-point temprature (Td in C)  from vapor pressure (e) 
    td =dewpoint(nc)  
    writeLines('dewpoint done')
  }
  # calculate relative air humidity
  if (any(param=="rh")){
    rh=relhum(nc)
    writeLines('rel. humidity done')
  }
  # convert pressure from Pa to hPa
  if (any(param=="pr")){
    pr=airpressure(nc)
    writeLines('air pressure done')
  }
  
  
  result=list()
  for (i in 1:length(param)){
    result[[i]]=get(param[i])
  }
  names(result)=param
  return (result)  
}
