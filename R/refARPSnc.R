
#'@name refARPSnc
#'@aliases refARPSnc

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


#'@usage refARPSnc(file,param)
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
#'@export refARPSnc
#'@examples   
#'  #### Example to georeference an ARPS netCDF 3.0 file and to calculate some important meteo params
#'       
#' arpsexample=system.file("allgaeu_d1_ARP.nc", package="aRps")
#' refARPSnc(arpsexample,'allNew')



refARPSnc<-function(file,varname){
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
  
  
  writeLines("Using 'allNew' will require a bit of time,")
  writeLines("so please be patient...;)")
  writeLines("extracting and calculating georeference- and meta-data...")
  
  # assign orig filename
  in.fname=file
  # get projection and extent of input netCDF file
  refInfo<- getRefInfo(in.fname)
  # open the original (input) netcdf file
  orig.nc<- nc_open(in.fname)
  # generate out.fname
  ffname<-basename(file)
  dname<-dirname(file)
  fname<-strsplit(ffname,split='.',fixed=TRUE)
  out.fname=paste0(dname,'/',fname[[1]][1],'_fix_',substr(orig.nc$dim$Time$units,15,24),'.nc')
  
  # define output variables
  # we have to deal with different projections (not any longer)
  if (refInfo$proj=="+proj=longlat +datum=WGS84 +no_defs"){
    #define corresponding x,y,z,time dimensions of the output netcdf file
    nc.x <- ncdim_def( "lon", "degrees_east", refInfo$coordx[0:orig.nc$dim$x$len],create_dimvar=TRUE)
    nc.y <- ncdim_def( "lat", "degrees_north",refInfo$coordy[0:orig.nc$dim$y$len],create_dimvar=TRUE)
    nc.z <- ncdim_def( "altitude", "m",orig.nc$var$ZP$dim[[3]][[9]][2:length(orig.nc$var$ZP$dim[[3]][[9]])],create_dimvar=TRUE)
    nc.x_stag <- ncdim_def( "long_stag", "degrees_east",refInfo$coordx,create_dimvar=TRUE)
    nc.y_stag <- ncdim_def( "lat_stag", "degrees_north",refInfo$coordy,create_dimvar=TRUE)
    nc.z_stag <- ncdim_def( "altitude_stag", "m", orig.nc$var$ZP$dim[[3]][[9]],create_dimvar=TRUE)
  } else {
    #### !!!!! to be FIXED
    writeLines("Projections are not implemented yet")
    return
  }
  # get the time values from the input data set
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
  

  # create an empty time dimensions variable with the existing time dimension
  # Note order: time has to be LAST
  nc.t   <- ncdim_def("Time", orig.nc$dim$Time$units, ti,calendar='standard',create_dimvar=TRUE,unlim=TRUE)
  
  # create the correct variables including the correct dimensions
  .u  <- ncvar_def('U',longname='U-velocity','m/s', list(nc.x_stag,nc.y,     nc.z,     nc.t), 1.e30 )
  .v  <- ncvar_def('V',longname='V-velocity','m/s', list(nc.x,     nc.y_stag,nc.z,     nc.t), 1.e30 )
  .w  <- ncvar_def('W',longname='W-velocity','m/s', list(nc.x,     nc.y,     nc.z_stag,nc.t), 1.e30 )
  
  .tc <- ncvar_def('TMP',longname='Air Temperature','Celsius', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  .td <- ncvar_def('DPT',longname='Dewpoint Temperature','K', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  .e  <- ncvar_def('PWV',longname='Partial Water Vapor','Pa', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  .es <- ncvar_def('SWV',longname='Saturation Water Vapour','Pa', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  .p  <- ncvar_def('PRES',longname='Air Pressure','hPa', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  .rh <- ncvar_def('RH',longname='Relative Humidity','%', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  
  .kmv <- ncvar_def('VVEL',longname='Vert. turb. mixing coef. for momentum','m2 s-1', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  .tke <- ncvar_def('TKE',longname='Turbulent Kinetic Energy','m2 s-2', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  .qc <- ncvar_def('CLWMR',longname='Cloud water mixing ratio','kg/kg', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  .qv <- ncvar_def('WVSMR',longname='Water vapor specific humidity','kg/kg', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  .qr <- ncvar_def('RWMR',longname='mass_fraction_of_rain_in_air','kg/kg', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  .qi <- ncvar_def('CIMR',longname='Cloud Ice mixing ratio','kg/kg', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  .qh <- ncvar_def('HMR',longname='mass_fraction_of_graupel_in_air','kg/kg', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  .qs <- ncvar_def('SMR',longname='mass_fraction_of_snow_in_air','kg/kg', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  
  .rainc <- ncvar_def('Cumulus convective rain','mm', list(nc.x,nc.y,nc.t), 1.e30 )
  .raing <- ncvar_def('Grid supersaturation rain','mm', list(nc.x,nc.y,nc.t), 1.e30 )
  
  .zp <- ncvar_def('ZP',longname='level','gpm', list(nc.x,nc.y,nc.z_stag), 1.e30 )
  
  writeLines("creating raw netcdf output template...")
  # Create a netCDF file with this variable
  ncnew <- nc_create( out.fname,list(.u,.v,.w,.tc,.td,.rh,.e,.es,.p,.tke,.kmv,.qc,.qr,.qv,.qi,.qh,.qs,.raing,.rainc,.zp))
  writeLines("creating uvw ...")
  ncvar_put( ncnew, .u, ncvar_get(orig.nc, 'U'))
  ncvar_put( ncnew, .v, ncvar_get(orig.nc, 'V'))
  ncvar_put( ncnew, .w, ncvar_get(orig.nc, 'W'))
  writeLines("temp & humidity ...")
  ncvar_put(ncnew,  .tc, tcelsius(orig.nc)) 
  ncvar_put( ncnew, .td, dewpoint(orig.nc))
  ncvar_put( ncnew, .rh, relhum(orig.nc))
  ncvar_put( ncnew, .e, partwatervapor(orig.nc))
  ncvar_put( ncnew, .es, satwatervapor(orig.nc))
  ncvar_put( ncnew, .p, airpressure(orig.nc))
  writeLines("creating turbulence ...")
  ncvar_put( ncnew, .tke, ncvar_get(orig.nc, 'TKE')) 
  ncvar_put( ncnew, .kmv, ncvar_get(orig.nc, 'KMV'))  
  writeLines("creating hydrometeo ...")
  ncvar_put( ncnew, .qc, ncvar_get(orig.nc, 'QC'))  
  ncvar_put( ncnew, .qr, ncvar_get(orig.nc, 'QR'))  
  ncvar_put( ncnew, .qv, ncvar_get(orig.nc, 'QV'))  
  ncvar_put( ncnew, .qi, ncvar_get(orig.nc, 'QI'))  
  ncvar_put( ncnew, .qh, ncvar_get(orig.nc, 'QH'))  
  ncvar_put( ncnew, .qs, ncvar_get(orig.nc, 'QS'))  
  ncvar_put( ncnew, .zp, ncvar_get(orig.nc, 'ZP'))  
  writeLines("creating precip ...")
  ncvar_put( ncnew, .raing, ncvar_get(orig.nc, 'RAING'))  
  ncvar_put( ncnew, .rainc, ncvar_get(orig.nc, 'RAINC'))  
  nc_close(ncnew)
  test<-nc_open(out.fname)
  writeLines("wrote")
  test$nvars
  writeLines("successfully")
  
##  ncid_old <- nc_open( outfile1 , write=TRUE )
##  #xdim <- ncid_old$dim[['lon']]
##  #ydim <- ncid_old$dim[['lat']]
##  #tdim <- ncid_old$dim[['Time']]
##  #zdim <- ncdim_def( "level_stag", "m", orig.nc$var$ZP$dim[[3]][[9]],create_dimvar=TRUE)
##  ncid_old <- ncvar_add( ncid_old, .w )  # NOTE this returns a modified netcdf file handle 
##  ncvar_put( ncid_old, .w, ncvar_get(orig.nc, 'W'))
##  nc_close(ncid_old)
## if vars should be cropped etc.
## extract.var=ncvar_get(orig.nc, varname,start=c(1,1,1,1), count=c(nc.x$len,nc.y$len,nc.z$len,nc.t$len) )


  ##reopen new netcdf file
  ##new.nc<-nc_open(outfile3)
  ##writeLines("checked no. of  variables:")
  ##str(new.nc$nvars)
  ##writeLines(paste(outfile3,'with', new.nc$nvars,' vars successfully created...'))
  ##writeLines("concatenating all files... using ncks") 
  ##system(paste("ncks -A ",outfile1,outfile2))
  ##system(paste("ncks -A ",outfile2,outfile3))
}
