#'@name getARPSsounding
#'@aliases getARPSsounding
#'@title Extract single ARPS columns and plots data as skew-T, log p diagram 
#'@description
#' getARPSsounding extract a arbitrary column of the ARPS 4D data cube. 
#' The derived data column is converted to a data.frame 
#' and can be plotted as a skew-T, log p diagram using plotradiosonde
#'@usage  getARPSsounding(nc,col,row,tim)
#'@param filename of an valid ARPS netCDF file
#'@param col   array position of the column to be extracted
#'@param row   array position of the row to be extracted
#'@param tim   time step of the dataset to be extracted
#'@param zoom  if (zoom=TRUE) the skewt log digramm is plotted only for a pressure between 1050 and 400 hPa
#'@param winds  if (wind=TRUE) a windbarb column is added beside the digramm
#'@param viewtable  if (viewtable=TRUE) showing result table

#'@return a dataframe containing the above variables

#'@author Chris Reudenbach, Hanna Meyer
#'@source 
#'\tabular{ll}{
#'Package: \tab aRps\cr
#'Type: \tab Package\cr
#'Version: \tab 0.3\cr
#'License: \tab GPL (>= 2)\cr
#'LazyLoad: \tab yes\cr
#'}
#'
#'@examples ###############
#'  # get data
#'  arps.ncfile=system.file("kili.nc", package="aRps")
#'  nc <- nc_open(arps.ncfile)
#'  
#'  getARPSsounding(nc,10,10,2)
#'  
#'@export getARPSsounding
#'@keywords keywords
#'
getARPSsounding=function(nc,col,row,tim){
    
    lev=nc$dim$z$len
    pr1=slice(airpressure(nc), i=col:col ,j=row:row,k=1:lev,l=tim:tim)
    #pr1=slice(get.var.ncdf( nc, "PT"), i=col:col ,j=row:row,k=1:lev,l=tim:tim)
    writeLines('pressure done')
    tc1=slice(tcelsius(nc), i=col:col ,j=row:row,k=1:lev,l=tim:tim)
    writeLines('temperature done')
    td1=slice(dewpoint(nc), i=col:col ,j=row:row,k=1:lev,l=tim:tim)
    writeLines('dewpoint done')
    list(wind)
    wind=wind(nc)
    zp=alt(nc)
    u1=slice(wind[[3]], i=col:col ,j=row:row,k=1:lev,l=tim:tim)
    writeLines('u vector done')
    v1=slice(wind[[4]], i=col: col ,j=row:row,k=1:lev,l=tim:tim)
    writeLines('v vector done')
    w1=slice(wind[[5]], i=col:col ,j=row:row,k=1:lev,l=tim:tim)
    writeLines('w  done')
    ws1=slice(wind[[1]], i=col:col ,j=row:row,k=1:lev,l=tim:tim)
    writeLines('windspeed done')
    wd1=slice(wind[[2]], i=col:col ,j=row:row,k=1:lev,l=tim:tim)
    writeLines('wind dir.  done')
    zp=slice(zp, i=col:col ,j=row:row,k=1:lev)
    writeLines('zp  done')
    rm(wind)
    rh1=slice(relhum(nc), i=col:col ,j=row:row,k=1:lev,l=tim:tim)
    writeLines('rel. humid.  done')
    column=list(pr1,tc1,td1,u1,v1,ws1,wd1,rh1,zp,w1)
    return(column) 
  }