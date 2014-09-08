#'@title Extract/calculate and georeferences meteo params from an ARPS netcdf file
#'@description  The information of the projection is calculated using the informations as derived by the gdalinfo call of the ARPS netcdf file. Currently only "lambert_conformal_conic" and 'latlong' is supported.
#'\code{\link{getRefInfo}} provides all missing projection values of an ARPS netCDF file

#'@source 
#'\tabular{ll}{
#'Package: \tab aRps\cr
#'Type: \tab Package\cr
#'Version: \tab 0.2\cr
#'Date: \tab \Sexpr[echo=TRUE]{paste0(getYear(Sys.Date()),"-",getMonth(Sys.Date()),"-",getDay(Sys.Date()))}\cr
#'License: \tab GPL (>= 2)\cr
#'LazyLoad: \tab yes\cr
#'}

#'@name makenc3D
#'@aliases makenc3D

#'@usage makenc3D(file,param)
#'@author Chris Reudenbach and Hanna Meyer
#'@references \url{http://giswerk.org/doku.php?id=doku:modeling:arps:arps_installation}
#'@seealso For retrieving and writing projected data see\code{\link{derive4dParam}}. 
#' 
#'@param file  is a filname of an ARPS netcdf file
#'@param param  is name of the parameter to be extracted. if one use 'newAll' all params will be transcribed
#'
#'@return makenc3D returns the following parameters:
#'\tabular{ll}{
#'outfile \tab new netcdf file with all or a single param\cr
#'}  
#'@export makenc3D
#'@examples   
#'  #### Example to georeference an ARPS netCDF 3.0 file and to calculate some important meteo params
#'       
#' arpsexample=system.file("allgaeu_d1_ARP.nc", package="aRps")
#' makenc3D(arpsexample,'newAll')



makenc3D<-function(file,varname){
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

  writeLines("If you use the function with the 'newAll' option the whole process will take quite a while...")
  writeLines("so please be patient...;)")

  #datadir='/home/creu/Daten/ARPS/'
  #setwd(datadir)
  #varname='newall'
  #file='marburg_d1.nc'
  outvar.longname=varname
  #file = '/home/creu/progs/opengrads/data/stol_d1_ARP.nc'
  #file=system.file("allgaeu_d1_ARP.nc", package="aRps")
  
  outfile=paste0(varname,'.nc')
  infile=file
  # get projection and extent of input netCDF file
  refInfo=getRefInfo(infile)
  # open input netcdf file
  orig.nc <- open.ncdf(infile)
  # generate sequence of x,y coordinates based on the refInfos
  tmpx<-seq(refInfo$ext[1],refInfo$ext[2]- refInfo$dx, by = refInfo$dx )
  tmpy<-seq(refInfo$ext[3],refInfo$ext[4]- refInfo$dy, by = refInfo$dy )
  
  # we have to deal with different projections
  if (refInfo$proj=="+proj=longlat +datum=WGS84 +no_defs"){
    #define corresponding x,y,z,time dimensions of the output netcdf file
    nc.x <- dim.def.ncdf( "X", "Lon", tmpx)
    nc.y <- dim.def.ncdf( "Y", "Lat", tmpy)
    nc.z <- dim.def.ncdf( "Z", "level", orig.nc$dim$z$vals)
  } else {
    #define corresponding x,y,z,time dimensions of the output netcdf file
    nc.x <- dim.def.ncdf( "X", "Meter E-W", tmpx)
    nc.y <- dim.def.ncdf( "Y", "Meter N-S", tmpy)    
    nc.z <- dim.def.ncdf( "Z", "level", orig.nc$dim$z$vals) 
  }
    # Make a empty variable with those dimensions.  Note order: time is LAST
    nc.t   <- dim.def.ncdf("Time", "seconds", orig.nc$dim$Time$vals, unlim=TRUE)
    #nc.var <- var.def.ncdf(outvar.longname,outvar.units, list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
    var_ws <- var.def.ncdf('Wind Speed','m/s', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
    var_wd <- var.def.ncdf('Wind Direction','deg', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
    var_u  <- var.def.ncdf('U-velocity','m/s', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
    var_v  <- var.def.ncdf('V-velocity','m/s', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
    var_w  <- var.def.ncdf('W-velocity','m/s', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
    var_tc <- var.def.ncdf('Air Temperature','deg C', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
    var_td <- var.def.ncdf('Dewpoint Temperature','deg C', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
    var_e  <- var.def.ncdf('Partial Water Vapor','Pa', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
    var_es <- var.def.ncdf('Saturation Water Vapour','Pa', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
    var_p  <- var.def.ncdf('Air Pressure','hPa', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
    var_rh <- var.def.ncdf('Relative Humidity','%', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
    var_kmv <- var.def.ncdf('Vert. turb. mixing coef. for momentum','m2 s-1', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
    var_tke <- var.def.ncdf('Turbulent Kinetic Energy','m2 s-2', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
    var_qc <- var.def.ncdf('Cloud water mixing ratio','1', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
    var_qv <- var.def.ncdf('Water vapor specific humidity','1', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
    var_qr <- var.def.ncdf('Rain water mixing ratio','1', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
    var_qi <- var.def.ncdf('Cloud Ice mixing ratio','1', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
    var_qh <- var.def.ncdf('Hail mixing ratio','1', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
    var_qs <- var.def.ncdf('Snow mixing ratio','1', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )

  #var_rainc <- var.def.ncdf('Cumulus convective rain','mm', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  #var_raing <- var.def.ncdf( 'Grid supersaturation rain','mm', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  #var_zp <- var.def.ncdf('Height','m', list(nc.x,nc.y,nc.z), 1.e30 )


  varIdAll=list(var_ws,var_wd,var_u,var_v,var_w,var_tc,var_td,var_rh,var_e,var_es,var_p,var_kmv,var_tke,var_qc,var_qv,var_qr,var_qi,var_qh,var_qs)
  varid_wind= list(var_ws,var_wd,var_u,var_v,var_w)
  #varid_atm=list(var_tc,var_td,var_rh,var_e,var_es,var_p)
  #varid_newvars=list(var_ws,var_wd,var_u,var_v,var_w,var_tc,var_td,var_rh,var_e,var_es,var_p)
  
  # Create a netCDF file with this variable
  ncnew <- create.ncdf( outfile,varIdAll)  
  
  # if vars should be cropped etc.
  #extract.var=get.var.ncdf(orig.nc, varname,start=c(1,1,1,1), count=c(nc.x$len,nc.y$len,nc.z$len,nc.t$len) )
  
  #calculate and derive wind
  windParam=wind(orig.nc)
  
 if (varname=='ws'|varname=='wd'|varname=='u'|varname=='v'|varname=='w'|varname=='allNew'){
  # put data into empty output variable
  #if (dim3==TRUE){put.var.ncdf( ncnew, nc.var, extract.var, start=c(1,1,1), count=c(length(tmpx),length(tmpy),nc.z$len))}
  for( i in 1:length(varid_wind))
    put.var.ncdf( ncnew, varid_wind[[i]], windParam[[i]], start=c(1,1,1,1), count=c(-1,-1,-1,-1))
  rm(windParam)}
 if (varname=='tc'|varname=='allNew'){put.var.ncdf( ncnew, var_tc, tcelsius(orig.nc),start=c(1,1,1,1), count=c(-1,-1,-1,-1))} 
 if (varname=='td'|varname=='allNew'){put.var.ncdf( ncnew, var_td, dewpoint(orig.nc),start=c(1,1,1,1), count=c(-1,-1,-1,-1))} 
 if (varname=='rh'|varname=='allNew'){put.var.ncdf( ncnew, var_rh, relhum(orig.nc),start=c(1,1,1,1), count=c(-1,-1,-1,-1))} 
 if (varname=='e'|varname=='allNew'){put.var.ncdf( ncnew, var_e, partwatervapor(orig.nc),start=c(1,1,1,1), count=c(-1,-1,-1,-1))}
 if (varname=='es'|varname=='allNew'){put.var.ncdf( ncnew, var_es, satwatervapor(orig.nc),start=c(1,1,1,1), count=c(-1,-1,-1,-1))} 
 if (varname=='P'|varname=='allNew'){ put.var.ncdf( ncnew, var_p, airpressure(orig.nc),start=c(1,1,1,1), count=c(-1,-1,-1,-1))}
 if (varname=='KMV'|varname=='allNew'){ put.var.ncdf( ncnew, var_kmv, get.var.ncdf(orig.nc, 'KMV'),start=c(1,1,1,1), count=c(-1,-1,-1,-1))}
 if (varname=='TKE'|varname=='allNew'){ put.var.ncdf( ncnew, var_tke, get.var.ncdf(orig.nc, 'TKE'),start=c(1,1,1,1), count=c(-1,-1,-1,-1))}
 if (varname=='QC'|varname=='allNew'){ put.var.ncdf( ncnew, var_qc, get.var.ncdf(orig.nc, 'QC'),start=c(1,1,1,1), count=c(-1,-1,-1,-1))}
 if (varname=='QV'|varname=='allNew'){ put.var.ncdf( ncnew, var_qv, get.var.ncdf(orig.nc, 'QV'),start=c(1,1,1,1), count=c(-1,-1,-1,-1))}
 if (varname=='QR'|varname=='allNew'){ put.var.ncdf( ncnew, var_qr, get.var.ncdf(orig.nc, 'QR'),start=c(1,1,1,1), count=c(-1,-1,-1,-1))}
 if (varname=='QI'|varname=='allNew'){ put.var.ncdf( ncnew, var_qi, get.var.ncdf(orig.nc, 'QI'),start=c(1,1,1,1), count=c(-1,-1,-1,-1))}
 if (varname=='QH'|varname=='allNew'){ put.var.ncdf( ncnew, var_qh, get.var.ncdf(orig.nc, 'QH'),start=c(1,1,1,1), count=c(-1,-1,-1,-1))}
 if (varname=='QS'|varname=='allNew'){ put.var.ncdf( ncnew, var_qs, get.var.ncdf(orig.nc, 'QS'),start=c(1,1,1,1), count=c(-1,-1,-1,-1))}
 close.ncdf(ncnew)
 # reopen new netcdf file
 new.nc<-open.ncdf(outfile)
 str(new.nc$nvars)
 writeLines(paste(outfile,'with', new.nc$nvars,' vars in ',workdir,' created...'))
}

