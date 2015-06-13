
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
#'@author Chris Reudenbach and Hanna Meyer
#'@references \url{http://giswerk.org/doku.php?id=doku:modeling:arps:arps_installation}
#'@seealso If you want to use this data in a GIS or otherwise georeferenced you need to extract the projection and domain extent according to the reference system that was used by ARPS \code{\link{getRefInfo}}
#' 
#'@param file  is a filname of an ARPS netcdf file
#'
#'@return refARPSnc returns the following parameter(s):
#'\tabular{ll}{
#'outfile_fix \tab Georeferenced netCDF file with additionally wind and thermodynamic variables\cr
#'}  
#'@export refARPSnc
#'@examples ###############
#'  #### Example to georeference an ARPS netCDF 3.0 file and to calculate some important meteo params
#'       
#' arps.ncfile=system.file("kili.nc", package="aRps")
#' refARPSnc(arps.ncfile)



refARPSnc<-function(file,varname){
  #
  # Copyright 2013-15 Chris Reudenbach, Hanna Meyer
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
  
  
  writeLines("the conversion will require a bit of time,")
  writeLines("so please be patient...;)")
  writeLines(" ")
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
    
    nc.nstyp_total <- ncdim_def( "soil_type_total", "index", orig.nc$dim$nstyp_total$vals,create_dimvar=TRUE)
    nc.zsoil <- ncdim_def( "soil_level", "level", orig.nc$dim$zsoil$vals,create_dimvar=TRUE)
    nc.nstyp <- ncdim_def( "soil_type", "index", orig.nc$dim$nstyp$vals,create_dimvar=TRUE)
  } else {
    #### !!!!! to be FIXED
    writeLines("Projections are not implemented yet")
    return
  }
  # get the time values from the input data set
  ncvar_get (orig.nc , "Time") ->ti
  # create an empty time dimensions variable with the existing time dimension
  # Note order: time has to be LAST
  nc.t   <- ncdim_def("Time", orig.nc$dim$Time$units, ti,calendar='standard',create_dimvar=TRUE,unlim=TRUE)
  
  # create the correct variables including the correct dimensions
  

  # BASE Variables
  .basu   <- ncvar_def('UBAR',longname='Base_U-velocity','m/s',                 list(nc.x_stag,nc.y,     nc.z), 1.e30 )
  .basv   <- ncvar_def('VBAR',longname='Base_V-velocity','m/s',                 list(nc.x,     nc.y_stag,nc.z), 1.e30 )
  .basw   <- ncvar_def('WBAR',longname='Base_W-velocity','m/s',                 list(nc.x,     nc.y,     nc.z_stag), 1.e30 )
  .baspt  <- ncvar_def('PTBAR',longname='base_air_potential_temperature','K',   list(nc.x,     nc.y,     nc.z), 1.e30 )
  .basp   <- ncvar_def('PBAR',longname='base_air_pressure','Pa',                list(nc.x,     nc.y,     nc.z), 1.e30 )
  .basqv  <- ncvar_def('QVBAR',longname='base_specific_humidity','1',           list(nc.x,     nc.y,     nc.z), 1.e30 )
  
  # VEGETATION VARIABLES
  .vtyp  <- ncvar_def('VEGTYP',longname='vegetation_category','index',          list(nc.x,     nc.y), 1.e30 )
  .vlai  <- ncvar_def('LAI',longname='leaf_area_index','1',                     list(nc.x,     nc.y), 1.e30 )
  .vveg  <- ncvar_def('VEG',longname='vegetation_area_fraction','1',            list(nc.x,     nc.y), 1.e30 )
  
  # surface
  .surrou  <- ncvar_def('ROUFNS',longname='surface_roughness_length','m',       list(nc.x,     nc.y), 1.e30 )
  .sursnow  <- ncvar_def('SNOWDPTH',longname='surface_snow_thickness','m',      list(nc.x,     nc.y, nc.t), 1.e30 )
    
  # soil
  .soizp  <- ncvar_def('ZPSOIL',longname='geopotential_height_soil','m',        list(nc.x,   nc.y,     nc.zsoil), 1.e30 )
  .soitype  <- ncvar_def('SOILTYP',longname='soil_category','index',            list(nc.x,   nc.y,     nc.nstyp), 1.e30 )
  .soifrac  <- ncvar_def('STYPFRCT',longname='soil_fraction','1',               list(nc.x,   nc.y,     nc.nstyp), 1.e30 )
  .soitmp  <- ncvar_def('TSOIL',longname='soil_temperature','K',                list(nc.x,   nc.y,  nc.nstyp_total, nc.zsoil,  nc.t), 1.e30 )
  .soiQ  <- ncvar_def('QSOIL',longname='soil_moisture_content','kg m-2',        list(nc.x,   nc.y,  nc.nstyp_total, nc.zsoil,  nc.t), 1.e30 )
  .soiwca  <- ncvar_def('WETCANP',longname='canopy_water_amount','kg m-2',      list(nc.x,   nc.y,  nc.nstyp_total,  nc.t), 1.e30 )
    
  # fluxes
  .fxfrc  <- ncvar_def('RADFRC',longname='radiation_forcing','K/s',             list(nc.x,   nc.y,     nc.z,     nc.t), 1.e30 )
  .fxsw  <- ncvar_def('RADSW',longname='surface_downwelling_shortwave_flux_in_air','W m-2', list(nc.x,   nc.y,   nc.t), 1.e30 )
  .fxflw  <- ncvar_def('RNFLX',longname='surface_net_downward_longwave_flux','W m-2', list(nc.x,   nc.y,     nc.t), 1.e30 )
  .fxfsw  <- ncvar_def('RADSWNET',longname='surface_net_downward_shortwave_flux','W m-2', list(nc.x,   nc.y,     nc.t), 1.e30 )
  .fxflwa  <- ncvar_def('RADLWIN',longname='surface_downwelling_longwave_flux_in_air','W m-2', list(nc.x,   nc.y,     nc.t), 1.e30 )
  .fxflu  <- ncvar_def('USFLX',longname='surface_u_momentum_flux','kg m-1 s-2', list(nc.x,   nc.y,     nc.t), 1.e30 )
  .fxflv  <- ncvar_def('VSFLX',longname='surface_v_momentum_flux','kg m-1 s-2', list(nc.x,   nc.y,     nc.t), 1.e30 )
  .fxdsh  <- ncvar_def('PTSFLX',longname='surface_downward_sensible_heat_flux','kg m-1 s-2', list(nc.x,   nc.y,     nc.t), 1.e30 )
  .fxqv  <- ncvar_def('QVSFLX',longname='surface_downward_water_flux','kg m-1 s-2', list(nc.x,   nc.y,     nc.t), 1.e30 )
  
  
  # wind
  .u  <- ncvar_def('U',longname='U-velocity','m/s', list(nc.x_stag,nc.y,     nc.z,     nc.t), 1.e30 )
  .v  <- ncvar_def('V',longname='V-velocity','m/s', list(nc.x,     nc.y_stag,nc.z,     nc.t), 1.e30 )
  .w  <- ncvar_def('W',longname='W-velocity','m/s', list(nc.x,     nc.y,     nc.z_stag,nc.t), 1.e30 )
  .pt  <- ncvar_def('PT',longname='air_potential_temperature','K', list(nc.x, nc.y, nc.z,nc.t), 1.e30 )
  
  # additional calculations
  .tc <- ncvar_def('TMP',longname='Air Temperature','Celsius', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  .td <- ncvar_def('DPT',longname='Dewpoint Temperature','K', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  .e  <- ncvar_def('PWV',longname='Partial Water Vapor','Pa', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  .es <- ncvar_def('SWV',longname='Saturation Water Vapour','Pa', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  .p  <- ncvar_def('PRES',longname='Air Pressure','Pa', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  .hp  <- ncvar_def('PR',longname='Air Pressure','hPa', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  .rh <- ncvar_def('RH',longname='Relative Humidity','%', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  
  # mixing coefficients
  .kmh <- ncvar_def('KMH',longname='turbulent_mixing_coefficient_for_horizontal_momentum','m2 s-1', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  .kmv <- ncvar_def('KMV',longname='turbulent_mixing_coefficient_for_vertical_momentum','m2 s-1', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  .tke <- ncvar_def('TKE',longname='Turbulent Kinetic Energy','m2 s-2', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )

  # atmospheric water
  .qc <- ncvar_def('CLWMR',longname='cloud_water_mixing_ratio','kg/kg', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  .qv <- ncvar_def('WVSMR',longname='water_vapor_specific_humidity','kg/kg', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  .qr <- ncvar_def('RWMR',longname='mass_fraction_of_rain_in_air','kg/kg', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  .qi <- ncvar_def('CIMR',longname='cloud_ice_mixing_ratio','kg/kg', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  .qh <- ncvar_def('HMR',longname='mass_fraction_of_graupel_in_air','kg/kg', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  .qs <- ncvar_def('SMR',longname='mass_fraction_of_snow_in_air','kg/kg', list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  
  # precipitation
  .rainc <- ncvar_def('RAINC',longname='thickness_of_convective_rainfall_amount','mm', list(nc.x,nc.y,nc.t), 1.e30 )
  .raing <- ncvar_def('RAING',longname='thickness_of_large_scale_rainfall_amount','mm', list(nc.x,nc.y,nc.t), 1.e30 )
  .precep1 <- ncvar_def('PRCRATE1',longname='lwe_large_scale_precipitation_rate','m s-1', list(nc.x,nc.y,nc.t), 1.e30 )
  .precep2 <- ncvar_def('PRCRATE2',longname='lwe_convective_precipitation_rate','m s-1', list(nc.x,nc.y,nc.t), 1.e30 )
  .precep3 <- ncvar_def('PRCRATE3',longname='lwe_microphysics_precipitation_rate','m s-1', list(nc.x,nc.y,nc.t), 1.e30 )
  .precep4 <- ncvar_def('PRCRATE4',longname='lwe_precipitation_rate','m s-1', list(nc.x,nc.y,nc.t), 1.e30 )
  
  # geopotential height
  .zp <- ncvar_def('ZP',longname='level','gpm', list(nc.x,nc.y,nc.z_stag), 1.e30 )
  
  writeLines("creating raw netcdf output template")
  # Create a netCDF file with this variable
  ncnew <- nc_create( out.fname,list(.basu,.basv,.basw,.baspt,.basp,.basqv,.vtyp, .vlai, .vveg , .surrou,.soizp,.soitype,.soifrac,.soitmp,.soiQ,.soiwca, .sursnow,.u,.v,.w,.pt,.tc,.td,.rh,.e,.es,.p,.hp,.kmh,.kmv,.tke,.qc,.qr,.qv,.qi,.qh,.qs,.raing,.rainc,.precep1,.precep2,.precep3,.precep4,.zp,.fxfrc,.fxsw,.fxflw,.fxfsw,.fxflwa,.fxflu,.fxflv,.fxdsh,.fxqv ))
  writeLines("creating uvw        .")
  ncvar_put( ncnew, .u, ncvar_get(orig.nc, 'U'))
  ncvar_put( ncnew, .v, ncvar_get(orig.nc, 'V'))
  ncvar_put( ncnew, .w, ncvar_get(orig.nc, 'W'))
  ncvar_put( ncnew, .pt, ncvar_get(orig.nc, 'PT'))
  writeLines("temp & humidity     ..")
  ncvar_put(ncnew,  .tc, tcelsius(orig.nc)) 
  ncvar_put( ncnew, .td, dewpoint(orig.nc))
  ncvar_put( ncnew, .rh, relhum(orig.nc))
  ncvar_put( ncnew, .e, partwatervapor(orig.nc))
  ncvar_put( ncnew, .es, satwatervapor(orig.nc))
  ncvar_put( ncnew, .hp, airpressure(orig.nc))
  ncvar_put( ncnew, .p, ncvar_get(orig.nc, 'P'))
  writeLines("creating turbulence   ...")
  ncvar_put( ncnew, .tke, ncvar_get(orig.nc, 'TKE')) 
  ncvar_put( ncnew, .kmv, ncvar_get(orig.nc, 'KMV'))  
  ncvar_put( ncnew, .kmh, ncvar_get(orig.nc, 'KMH'))
  writeLines("creating hydrometeors ....")
  ncvar_put( ncnew, .qc, ncvar_get(orig.nc, 'QC'))  
  ncvar_put( ncnew, .qr, ncvar_get(orig.nc, 'QR'))  
  ncvar_put( ncnew, .qv, ncvar_get(orig.nc, 'QV'))  
  ncvar_put( ncnew, .qi, ncvar_get(orig.nc, 'QI'))  
  ncvar_put( ncnew, .qh, ncvar_get(orig.nc, 'QH'))  
  ncvar_put( ncnew, .qs, ncvar_get(orig.nc, 'QS'))  
  ncvar_put( ncnew, .zp, ncvar_get(orig.nc, 'ZP'))  
  writeLines("creating precipitation .....")
  ncvar_put( ncnew, .raing, ncvar_get(orig.nc, 'RAING'))  
  ncvar_put( ncnew, .rainc, ncvar_get(orig.nc, 'RAINC'))  
  ncvar_put( ncnew, .precep1, ncvar_get(orig.nc, 'PRCRATE1')) 
  ncvar_put( ncnew, .precep2, ncvar_get(orig.nc, 'PRCRATE2')) 
  ncvar_put( ncnew, .precep3, ncvar_get(orig.nc, 'PRCRATE3')) 
  ncvar_put( ncnew, .precep4, ncvar_get(orig.nc, 'PRCRATE4')) 
  writeLines("creating flux & rad    .......")
  ncvar_put( ncnew,  .fxfrc ,ncvar_get(orig.nc, 'RADFRC')) 
  ncvar_put( ncnew,  .fxsw  ,ncvar_get(orig.nc, 'RADSW'))  
  ncvar_put( ncnew,  .fxflw ,ncvar_get(orig.nc, 'RNFLX'))  
  ncvar_put( ncnew,  .fxfsw ,ncvar_get(orig.nc, 'RADSWNET'))
  ncvar_put( ncnew,  .fxflwa,ncvar_get(orig.nc, 'RADLWIN'))
  ncvar_put( ncnew,  .fxflu ,ncvar_get(orig.nc, 'USFLX'))  
  ncvar_put( ncnew,  .fxflv ,ncvar_get(orig.nc, 'VSFLX'))  
  ncvar_put( ncnew,  .fxdsh ,ncvar_get(orig.nc, 'PTSFLX')) 
  ncvar_put( ncnew,  .fxqv  ,ncvar_get(orig.nc, 'QVSFLX'))
  writeLines("creating static base Variables        .......")
  ncvar_put( ncnew,  .basu  ,ncvar_get(orig.nc, 'UBAR'))      
  ncvar_put( ncnew,  .basv  ,ncvar_get(orig.nc, 'VBAR'))     
  ncvar_put( ncnew,  .basw  ,ncvar_get(orig.nc, 'WBAR'))   
  ncvar_put( ncnew,  .baspt ,ncvar_get(orig.nc, 'PTBAR'))     
  ncvar_put( ncnew,  .basp  ,ncvar_get(orig.nc, 'PBAR'))      
  ncvar_put( ncnew,  .basqv ,ncvar_get(orig.nc, 'QVBAR'))     
  writeLines("creating VEGETATION    .......")
  ncvar_put( ncnew,  .vtyp ,ncvar_get(orig.nc, 'VEGTYP'))     
  ncvar_put( ncnew,  .vlai ,ncvar_get(orig.nc, 'LAI'))        
  ncvar_put( ncnew,  .vveg ,ncvar_get(orig.nc, 'VEG'))        
  writeLines("ceating surface        ..........")
  ncvar_put( ncnew,  .surrou ,ncvar_get(orig.nc, 'ROUFNS'))   
  ncvar_put( ncnew,  .sursnow ,ncvar_get(orig.nc, 'SNOWDPTH'))
  writeLines("creating soil          .........")
  ncvar_put( ncnew,  .soizp ,ncvar_get(orig.nc, 'ZPSOIL'))    
  ncvar_put( ncnew,  .soitype ,ncvar_get(orig.nc, 'SOILTYP')) 
  ncvar_put( ncnew,  .soifrac ,ncvar_get(orig.nc, 'STYPFRCT'))
  ncvar_put( ncnew,  .soitmp ,ncvar_get(orig.nc, 'TSOIL'))    
  ncvar_put( ncnew,  .soiQ ,ncvar_get(orig.nc, 'QSOIL'))      
  ncvar_put( ncnew,  .soiwca ,ncvar_get(orig.nc, 'WETCANP'))  
  
  nc_close(ncnew)
  test<-nc_open(out.fname)
  writeLines("wrote")
  test$nvars
  writeLines("successfully")
  
  ##writeLines(paste(outfile3,'with', new.nc$nvars,' vars successfully created...'))
  ##writeLines("concatenating all files... using ncks") 
  ##system(paste("ncks -A ",outfile1,outfile2))
  ##system(paste("ncks -A ",outfile2,outfile3))
}
