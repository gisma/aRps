#'@title Calculate projection and extent from an ARPS netcdf file
#'@description  The information of the projection is calculated using by analysing the ARPS netcdf file parameters. Currently only "no_mapping" (default) and "lambert_conformal_conic" (north hemisphere) is supported.
#'\code{\link{getRefInfo}} provides the missing projection values of an ARPS netCDF file

#'@source 
#'\tabular{ll}{
#'Package: \tab peRfectpeak\cr
#'Type: \tab Package\cr
#'Version: \tab 0.2\cr
#'License: \tab GPL (>= 2)\cr
#'LazyLoad: \tab yes\cr
#'}

#'@name getRefInfo
#'@aliases getRefInfo

#'@usage getRefInfo(file)
#'@author Chris Reudenbach and Hanna Meyer
#'@references \url{http://giswerk.org/doku.php?id=wac:modeling:arps:intro}
#'@seealso For retrieving variables see\code{\link{derive4dParam}}. 
#'@seealso For calculate thermodynamic variables see\code{\link{calcMeteoParam}}. 
#' 
#'@param file  is a filname of an ARPS netcdf file
#'
#'@return getRefInfo returns the following parameters:
#'\tabular{ll}{
#'$proj \tab proj4 projection string\cr
#'$lat \tab central latitude of the data domain\cr
#'$lon \tab central longitude of the data domain\cr
#'$dx \tab grid spacing in x direction\cr
#'$dy \tab grid spacing in y direction\cr
#'$ext \tab xmin,xmax,ymin,ymax in $proj\cr
#'$coordx \tab list of x coordinates according to the domain $xcoord\cr
#'$coordy \tab list of y coordinates according to the domain $ycoord\cr
#'}  

#'@export getRefInfo
#'@examples   
#'  #### Example to extract the projection and 
#'  #### calculate the extent coordinates from a ARPS nccdf file
#'       
#' arps.ncfile=system.file("kili.nc", package="aRps")
#' refInfo=getRefInfo(arps.ncfile)
#' refInfo

getRefInfo=function(file){
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
  # along with aRps; if not, write to the Free Software
  # Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
  #
  # getRefInfo identifies the projection and extent of an arps model run netCDF outputfile
  # this is neccessary due to the fact that ARPS writes a non standard netCDF 3.0 format 
  # without correct projection and extent

  # using gdalinfo to derive the necessary parameters
  finfo <- gdalinfo(file)
  
  # extract parameters
  projname=strsplit(finfo[which(grepl("NC_GLOBAL#grid_mapping_name", finfo))],"=")[[1]][2]
  lat_1=as.numeric(strsplit(finfo[which(grepl("NC_GLOBAL#TRUELAT1", finfo))],"=")[[1]][2])
  lat_2=as.numeric(strsplit(finfo[which(grepl("NC_GLOBAL#TRUELAT2", finfo))],"=")[[1]][2])
  lat0=as.numeric(strsplit(finfo[which(grepl("NC_GLOBAL#CTRLAT", finfo))],"=")[[1]][2])
  lon0=as.numeric(strsplit(finfo[which(grepl("NC_GLOBAL#CTRLON", finfo))],"=")[[1]][2])
  dx=as.numeric(strsplit(finfo[which(grepl("NC_GLOBAL#DX", finfo))],"=")[[1]][2])
  dy=as.numeric(strsplit(finfo[which(grepl("NC_GLOBAL#DY", finfo))],"=")[[1]][2])
  x_0=as.numeric(strsplit(finfo[which(grepl("NC_GLOBAL#false_easting", finfo))],"=")[[1]][2])
  y_0=as.numeric(strsplit(finfo[which(grepl("NC_GLOBAL#false_northing", finfo))],"=")[[1]][2])
  
  ## get correct extent from netcdf file dimensions 
  ## because it is not provided by gdalinfo )
  ## **FIXME** we could get all data from the netcdf file
  netcdf=nc_open(file)
  x=netcdf$dim$x$len
  y=netcdf$dim$y$len

  ## check if projection is supported
  if (projname == "no_mapping"){
    writeLines("'no mapping' means generic (wgs84) geo-coordinates")
    writeLines("assign '+proj=longlat +datum=WGS84 +no_defs'")

    # approximative calculation of the latitude (lat0) dependend grid resolution for lat and lon
    # ARPS uses the WGS84 ellipsoid
    # so we take the WGS84 resolution for lat and lon in meter at the equator
    rad.cof=3.1459/180
    lat.1deg=110540
    lon.1deg=111320*cos(rad.cof*lat0)
    # calculate stepsize
    deltalat=1/lat.1deg*dy
    deltalon=1/lon.1deg*dx
    
    # derive min and max of the grid
    xmin<-   lon0-trunc(netcdf$dim$x_stag$len/2)*deltalon
    xmax<-   lon0+trunc(netcdf$dim$x_stag$len/2)*deltalon
    ymin<-   lat0-trunc(netcdf$dim$y_stag$len/2)*deltalat
    ymax<-   lat0+trunc(netcdf$dim$y_stag$len/2)*deltalat
    
    # calculate lat and lon coordinates  
    coordx<-seq(xmin,xmax, by = deltalon )
    coordy<-seq(ymin,ymax, by = deltalat )
    
    # no mapping means latlon wgs84
    latlon=TRUE

    # define latlong proj4 string
    proj="+proj=longlat +datum=WGS84 +no_defs" 
  } 
  ## ***FIXME*** LCC did not work properly in ARPS
  if(projname=="lambert_conformal_conic"){
    ## generate compliant prj4 string
    proj=paste("+proj=lcc +lat_1=",lat_1," +lat_2=",lat_2,
               " +lat_0=",lat0," +lon_0=",lon0, " +x_0=",x_0," +y_0=",y_0,sep="") 
    ## project centre coordinates
    tr <- ptransform(cbind(lon0, lat0)/180*pi,'+proj=longlat +datum=WGS84 +no_defs',proj)
    ## calculate extent using the Lambertian units (m) 
    ## !!!to be fixed!!! this does not work correctly due to the ARPS way of calculationg domains
    xmin=as.integer(tr[1,1]-(x/2*dx-dx))
    xmax=as.integer(tr[1,1]+(x/2*dx-dx))
    ymin=as.integer(tr[1,2]-(y/2*dy-dy))
    ymax=as.integer(tr[1,2]+(y/2*dy-dy))
    coordx<-seq(xmin,xmax, by = dx )
    coordy<-seq(ymin,ymax, by = dy )  
      }

  ## put coords in ext for export
  ext=c(xmin,xmax,ymin,ymax)
  
  ## generate return list
  geoRefOut=list("proj"=proj,"ctrlat"=lat0,"ctrlon"=lon0, "dx"=dx, "dy"=dy,"ext"=ext,"coordx"=coordx,"coordy"=coordy)
  return (geoRefOut)
}
