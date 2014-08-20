#'@title Extract one complete variable of an ARPS netcdf file and writes it back to a new netcdf
#'@description The ARPS netcdf files lack of the correct handling of projection and extent. Furthermore they are organized after netcdf3.0 The function var2nc extract all data plus the desired variable and write a new netcdf 4.0 file.
#'@source 
#'\tabular{ll}{
#'Package: \tab aRps\cr
#'Type: \tab Package\cr
#'Version: \tab 0.2\cr
#'Date: \tab \Sexpr[echo=TRUE]{paste0(getYear(Sys.Date()),"-",getMonth(Sys.Date()),"-",getDay(Sys.Date()))}\cr
#'License: \tab GPL (>= 2)\cr
#'LazyLoad: \tab yes\cr
#'}

#'@name var2nc
#'@aliases var2nc

#'@usage var2nc(file,var)
#'@author Chris Reudenbach and Hanna Meyer
#'@references \url{http://giswerk.org/doku.php?id=doku:modeling:arps:arps_installation}
#'@seealso For retrieving and writing projected data see\code{\link{derive4dParam}}. 
#' 
#'@param 
#'file  is a filname of an ARPS netcdf file
#'@param 
#'var is the parameter names (see\code{\link{derive4dParam}})
#'
#'@return var2nc returns the following parameters:
#'\tabular{ll}{
#'new netcdf file \tab named  as <PARAMETER> old netcdf file\cr
#'}
#'@export getRefInfo
#'@examples   
#'#### Example to extract the projection and calculate the extent coordinates from a ARPS nc file
#'       
#' arpsexample=system.file("allgaeu_d1_ARP.nc", package="aRps")
#' cparam=derive4d4Sound(arpsexample)
#' var2nc(arpsexample,'QV')
#' 
#' 


var2nc<-function(filename,var,vardata,new=FALSE,latlon=TRUE){
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
  vardata=u
  var='U'
  
  outfileName= paste0(var,'.nc')
  
  # get projection and extent of input netCDF file
  refInfo=getRefInfo(filename)
  # open input netcdf file
  orig.nc <- open.ncdf(filename)
  # generate sequence of x,y coordinates using the refInfos
  if (latlon==TRUE){
    tmpx<-seq(refInfo$ext[1],refInfo$ext[2], by = refInfo$dx )
    tmpy<-seq(refInfo$ext[3],refInfo$ext[4], by = refInfo$dy )
  }
  if (latlon==FALSE){
  tmpx<-seq(refInfo$ext[1],refInfo$ext[2]+1+refInfo$dx, by = refInfo$dx )
  tmpy<-seq(refInfo$ext[3],refInfo$ext[4]+refInfo$dy, by = refInfo$dy )
  }
  #define corresponding x,y,z,time dimensions of the output netcdf file
  nc.x <- dim.def.ncdf( "X", "Meter", tmpx)
  nc.y <- dim.def.ncdf( "Y", "Meter", tmpy)
  nc.z <- dim.def.ncdf( "Z", "level", orig.nc$dim$z$vals)
  nc.t <- dim.def.ncdf( "Time", "seconds", orig.nc$dim$Time$vals, unlim=TRUE)
  extract.var=vardata
  if (new==TRUE){
  # set varname to derive
  orig.nc.varname=var
  # extract this variable from orig.nc
  extract.var=get.var.ncdf(orig.nc, orig.nc.varname,start=c(1,1,1,1), count=c(length(tmpx),length(tmpy),nc.z$len,nc.t$len) )
  # define corresponding varnames for output
  }
  nc.var.varname=var
  nc.var.varshort=var
  # Make a empty variable with those dimensions.  Note order: time is LAST
  nc.var <- var.def.ncdf(nc.var.varname,nc.var.varshort, list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  # Create a netCDF file with this variable
  ncnew <- create.ncdf( outfileName,nc.var)
  # put data into empty output variable
  put.var.ncdf( ncnew, nc.var, extract.var, start=c(1,1,1,1), count=c(length(tmpx),length(tmpy),nc.z$len,nc.t$len) )
  close.ncdf(ncnew)
}

