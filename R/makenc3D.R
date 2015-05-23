#'@title Extract/calculate and georeferences meteo params from an ARPS netcdf file
#'@description  The information of the projection is calculated using the informations as derived by the gdalinfo call of the ARPS netcdf file. Currently only "lambert_conformal_conic" and 'latlong' is supported.
#'\code{\link{getRefInfo}} provides all missing projection values of an ARPS netCDF file

#'@source 
#'\tabular{ll}{
#'Package: \tab aRps\cr
#'Type: \tab Package\cr
#'Version: \tab 0.2\cr
#'License: \tab GPL (>= 2)\cr
#'LazyLoad: \tab yes\cr
#'}

#'@name makenc3D
#'@aliases makenc3D

#'@usage makenc3D(file,param)
#'@author Chris Reudenbach and Hanna MeyerQ
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
#' makenc3D(arpsexample,'allNew')



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
  
  
  writeLines("If you use the function with the 'allNew' option the whole process will take quite a while...")
  writeLines("so please be patient...;)")
  
  # substitute varname outfile
  outvar.longname=varname
  outfile1=paste0(varname,'wind.nc')
  outfile2=paste0(varname,'temp.nc')
  outfile3=paste0(varname,'orig.nc')
  infile=file
  # get meta information and open ARPS nc file
  writeLines("getting reference and metadata...")
  # get projection and extent of input netCDF file
  refInfo=getRefInfo(infile)
  # open input netcdf file
  orig.nc <- nc_open(infile)
  
  # define output variables
  writeLines("defining variables")
  # generate sequence of x,y coordinates based on the refInfos
  # originial implmentation
  #tmpx<-seq(refInfo$ext[1],refInfo$ext[2]- refInfo$dx, by = refInfo$dx )
  #tmpy<-seq(refInfo$ext[3],refInfo$ext[4]- refInfo$dy, by = refInfo$dy )
  # flexible for proj = 0
  #tmpx<-seq(refInfo$ext[1],(max(orig.nc$dim$x$vals)-1)*refInfo$dx+refInfo$ext[1], by=refInfo$dx)
  #tmpy<-seq(refInfo$ext[3],(max(orig.nc$dim$y$vals)-1)*refInfo$dy+refInfo$ext[3], by=refInfo$dy)
  # static for entenberg
  tmpx<-seq(7.34+0.5*0.013518519,(max(orig.nc$dim$x$vals-1))*0.013518519+(7.34+0.5*0.013518519), by=0.013518519)
  tmpy<-seq(49.83+0.5*0.01345679,(max(orig.nc$dim$y$vals-1))*0.01345679+(49.83+0.5*0.01345679), by=0.01345679)
  # we have to deal with different projections (not any longer)
  if (refInfo$proj=="+proj=longlat +datum=WGS84 +no_defs"){
    #define corresponding x,y,z,time dimensions of the output netcdf file
    nc.x <- ncdim_def( "lon", "degrees_east", tmpx,create_dimvar=TRUE)
    nc.y <- ncdim_def( "lat", "degrees_north", tmpy,create_dimvar=TRUE)
    nc.z <- ncdim_def( "level", "level", orig.nc$dim$z$vals,create_dimvar=TRUE)
  } else {
    #define corresponding x,y,z,time dimensions of the output netcdf file
    nc.x <- ncdim_def( "x", "meter", tmpx,create_dimvar=TRUE)
    nc.y <- ncdim_def( "y", "meter", tmpy,create_dimvar=TRUE)    
    nc.z <- ncdim_def( "z", "level", orig.nc$dim$z$vals,create_dimvar=TRUE) 
    nc.x <- ncdim_def( "lon",'degrees_east', longname="Longitude", tmpx,create_dimvar=TRUE)
    nc.y <- ncdim_def( "lat", 'degrees_north',longname="Latitude", tmpy,create_dimvar=TRUE)
    nc.z <- ncdim_def( "level",'level', longname="Height", orig.nc$dim$z$vals,create_dimvar=TRUE)    
  }
  
  #  fname.tmp=basename(infile)
  #  substr(orig.nc$dim$Time$units,15,34)
  
  ncvar_get (orig.nc , "Time") ->ti
  
  #  date<-as.POSIXct(ti, origin=c(substr(orig.nc$dim$Time$units,15,34)),tz = "GMT")
  #  date<-as.POSIXct(substr(fname.tmp,start=0,stop=10))
  #st<-timeCalendar(m = as.numeric(substr(fname.tmp,6,7)), d = as.numeric(substr(fname.tmp,9,10)), y = as.numeric(substr(fname.tmp,1,4),FinCenter = "GMT"))  
  #  i=1
  #  datearray<-list()
  #  while (i<length(orig.nc$dim$Time$vals)){
  #    datearray[i] <- as.character(update(date, second = as.numeric(orig.nc$dim$Time$vals[i])))
  #    if (i == 1) {
  #      datearray[i]  <-paste(datearray[i] ,'00:00:00')
  #    }
  #    i=i+1
  #  }
  
  # Make a empty variable with those dimensions.  Note order: time is LAST
  nc.t   <- ncdim_def("Time", orig.nc$dim$Time$units, ti,calendar='standard',create_dimvar=TRUE,unlim=TRUE)
  #nc.var <- var.def.ncdf(outvar.longname,outvar.units, list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  
  var_ws <- ncvar_def('WS',longname='Wind Speed','m/s', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  var_wd <- ncvar_def('WD',longname='Wind Direction','degrees_true', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  var_u  <- ncvar_def('UGRD',longname='U-velocity','m/s', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  var_v  <- ncvar_def('VGRD',longname='V-velocity','m/s', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  var_w  <- ncvar_def('WGRD',longname='W-velocity','m/s', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  var_tc <- ncvar_def('TMP',longname='Air Temperature','Celsius', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  var_td <- ncvar_def('DPT',longname='Dewpoint Temperature','K', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  var_e  <- ncvar_def('PWV',longname='Partial Water Vapor','Pa', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  var_es <- ncvar_def('SWV',longname='Saturation Water Vapour','Pa', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  var_p  <- ncvar_def('PRES',longname='Air Pressure','hPa', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  var_rh <- ncvar_def('RH',longname='Relative Humidity','%', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  var_kmv <- ncvar_def('VVEL',longname='Vert. turb. mixing coef. for momentum','m2 s-1', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  var_tke <- ncvar_def('TKE',longname='Turbulent Kinetic Energy','m2 s-2', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  var_qc <- ncvar_def('CLWMR',longname='Cloud water mixing ratio','kg/kg', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  var_qv <- ncvar_def('WVSMR',longname='Water vapor specific humidity','kg/kg', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  var_qr <- ncvar_def('RWMR',longname='Rain water mixing ratio','kg/kg', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  var_qi <- ncvar_def('CIMR',longname='Cloud Ice mixing ratio','kg/kg', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  var_qh <- ncvar_def('HMR',longname='Hail mixing ratio','kg/kg', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  var_qs <- ncvar_def('SMR',longname='Snow mixing ratio','kg/kg', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  
  #var_rainc <- var.def.ncdf('Cumulus convective rain','mm', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  #var_raing <- var.def.ncdf( 'Grid supersaturation rain','mm', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  var_zp <- ncvar_def('Height',longname='Altitude','meter', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  
  
  varid_orig=list(var_kmv,var_tke,var_qc,var_qv,var_qr,var_qi,var_qh,var_qs)
  varid_wind= list(var_ws,var_wd,var_u,var_v,var_w,var_zp)
  varid_tmp=list(var_tc,var_td,var_rh,var_e,var_es,var_p)
  
  #varid_atm=list(var_tc,var_td,var_rh,var_e,var_es,var_p)
  #varid_newvars=list(var_ws,var_wd,var_u,var_v,var_w,var_tc,var_td,var_rh,var_e,var_es,var_p)
  writeLines("creating raw netcdf output template...")
  # Create a netCDF file with this variable
  #ncnew <- nc_create( outfile,varIdAll,force_v4=FALSE)  
  
  ncnew <- nc_create( outfile1,varid_wind,force_v4=TRUE)
  
  # if vars should be cropped etc.
  #extract.var=ncvar_get(orig.nc, varname,start=c(1,1,1,1), count=c(nc.x$len,nc.y$len,nc.z$len,nc.t$len) )
  count<-orig.nc$var$QC$varsize
  #count<-c(orig.nc$dim$x_stag$len,orig.nc$dim$y_stag$len,orig.nc$dim$z$len,orig.nc$dim$Time$len)
  ndims   <- orig.nc$ndims
  
  #calculate and derive wind
  writeLines("we have to split the file into chunks due to memory limitations")
  writeLines("We will write 3  seperate nc files that are merged later on....")
  writeLines("starting with the staggered fields ")
  if (varname=='ws'|varname=='wd'|varname=='u'|varname=='v'|varname=='w'|varname=='allNew'|varname=='wind'){
    # put data into empty output variable
    # if (dim3==TRUE){ncvar_add( ncnew, nc.var, extract.var, start=c(1,1,1), count=c(length(tmpx),length(tmpy),nc.z$len))}
    writeLines("zp") 
    ncvar_put( ncnew, var_w, wind(orig.nc,'zp') )
    writeLines("wind speed") 
    ncvar_put( ncnew, var_ws,wind(orig.nc,'ws'))
    writeLines("wind direction") 
    ncvar_put( ncnew, var_wd,wind(orig.nc,'wd'))
    writeLines("u") 
    ncvar_put( ncnew, var_u, wind(orig.nc,'u'))
    writeLines("v") 
    ncvar_put( ncnew, var_v, wind(orig.nc,'v') )
    writeLines("w") 
    ncvar_put( ncnew, var_w, wind(orig.nc,'w') )}
    nc_close(ncnew)
  new.nc<-nc_open(outfile1)
  writeLines("checked no. of  variables:")
  str(new.nc$nvars)
  writeLines(paste(outfile1,'with', new.nc$nvars,' vars successfully created...'))
  nc_close(ncnew)
  rm(var_w)
  rm(var_u)
  rm(var_v)
  rm(var_ws)
  rm(var_wd)
  ncnew <- nc_create( outfile2,varid_tmp,force_v4=TRUE)
  new.nc<-nc_open(outfile2, write=TRUE)   
  writeLines("doing temperature calculations")
    #if (varname=='tc'|varname=='allNew'){ncvar_add( ncnew, var_tc, tcelsius(orig.nc) )} 
    ncvar_put(ncnew, var_tc, tcelsius(orig.nc)) 
    ncvar_put( ncnew, var_td, dewpoint(orig.nc))
    writeLines("doing rh, e and es calculations")
    ncvar_put( ncnew, var_rh, relhum(orig.nc))
    ncvar_put( ncnew, var_e, partwatervapor(orig.nc))
    ncvar_put( ncnew, var_es, satwatervapor(orig.nc))
    ncvar_put( ncnew, var_p, airpressure(orig.nc))
    nc_close(ncnew)
  # reopen new netcdf file
  new.nc<-nc_open(outfile2)
  writeLines("checked no. of  variables:")
  str(new.nc$nvars)
  writeLines(paste(outfile2,'with', new.nc$nvars,' vars successfully created...'))
  nc_close(ncnew)
  rm(var_tc)
  rm(var_td)
  rm(var_rh)
  rm(var_e)
  rm(var_es)
  rm(var_p)
  ncnew <- nc_create( outfile3,varid_orig,force_v4=TRUE)
  new.nc<-nc_open(outfile3, write=TRUE) 
  writeLines("retrieve orig KMV, TKE, QC,QV,QI,QR,QH,QS")
  ncvar_put( ncnew, var_kmv, ncvar_get(orig.nc, 'KMV'))
  ncvar_put( ncnew, var_tke, ncvar_get(orig.nc, 'TKE'))
  ncvar_put( ncnew, var_qc, ncvar_get(orig.nc, 'QC'))
  ncvar_put( ncnew, var_qv, ncvar_get(orig.nc, 'QV'))
  ncvar_put( ncnew, var_qr, ncvar_get(orig.nc, 'QR'))
  ncvar_put( ncnew, var_qi, ncvar_get(orig.nc, 'QI'))
  ncvar_put( ncnew, var_qh, ncvar_get(orig.nc, 'QH'))
  ncvar_put( ncnew, var_qs, ncvar_get(orig.nc, 'QS'))
  nc_close(ncnew)
  # reopen new netcdf file
  new.nc<-nc_open(outfile3)
  writeLines("checked no. of  variables:")
  str(new.nc$nvars)
  writeLines(paste(outfile3,'with', new.nc$nvars,' vars successfully created...'))
  writeLines("concatenating all files... using ncks") 
  system(paste("ncks -A ",outfile1,outfile2))
  system(paste("ncks -A ",outfile2,outfile3))
}

