#'@name plotARPScol
#'@aliases plotARPScol
#'@title Extract single ARPS columns and plots data as skew-T, log p diagram 
#'@description
#' plotARPScol extract a unique column of the arps ARPS cube. The derived single
#' data column is converted to a data.frame as used bei the plotradiosonde function of the adapted Radiosonde package
#'@usage  plotARPScol(filename,col, row, tim, zoom, winds=FALSE, viewtable=FALSE)
#'@param filename of an valid ARPS netCDF file
#'@param col   array position of the column to be extracted
#'@param row   array position of the row to be extracted
#'@param tim   time step of the dataset to be extracted
#'@param zoom  if (zoom=TRUE) the skewt log digramm is plotted only for a pressure between 1050 and 400 hPa
#'@param winds  if (wind=TRUE) a windbarb column is added beside the digramm
#'@param viewtable  if (viewtable=TRUE) showing result table

#'@return plots a skew-T, log p diagram from the required domain position
#'returns additionale the corresponding table of data

#'@author Chris Reudenbach, Hanna Meyer
#'@source 
#'\tabular{ll}{
#'Package: \tab aRps\cr
#'Type: \tab Package\cr
#'Version: \tab 0.3\cr
#'License: \tab GPL (>= 2)\cr
#'LazyLoad: \tab yes\cr
#'}
#'@seealso seealso
#'@examples ###############
#'  # get data
#'  arps.ncfile=system.file("kili.nc", package="aRps")
#   default usage: plots skew-T, log p diagram array (domain) position 10, 10 at timepos 2
#'  plotARPScol(arps.ncfile,10,10,2)
#'  #plots skew-T, log p diagram derived at array (domain) position 10, 10 at timepos 2 and shows data table
#'  plotARPScol(arps.ncfile,10,10,2, zoom=FALSE, winds=TRUE,viewtable=TRUE) 
#'@export  
#'@keywords keywords
#'
extcol=function(file,col,row,tim){
  nc <- nc.open(file)

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
  zp=slice(wind[[6]], i=col:col ,j=row:row,k=1:lev)
  writeLines('zp  done')
  rm(wind)
  rh1=slice(relhum(nc), i=col:col ,j=row:row,k=1:lev,l=tim:tim)
  writeLines('rel. humid.  done')
  column=list(pr1,tc1,td1,u1,v1,ws1,wd1,rh1,zp,w)
  #param=c("pr","tc","td","u","v","ws","wd","rh","zp")
  #names(column)=param
  
}

plotARPScol <- function(file,col,row,tim,zoom=TRUE,winds=FALSE,viewtable=FALSE){

  # arps.ncfile = '/home/creu/progs/opengrads/data/stol_d1_ARP.nc'
  # filename= '/home/creu/Daten/ARPS/marburg_d1.nc'

  column=extcol(file,col,row,tim)

sounding<- melt(column[[1]], value.name = "press")
zp      <- melt(column[[9]], value.name = "height")
tc      <- melt(column[[2]], value.name = "temp")
td      <- melt(column[[3]], value.name = "dewpt")
rh      <- melt(column[[8]], value.name = "rh")
u       <- melt(column[[4]], value.name = "uwind")
v       <- melt(column[[5]], value.name = "vwind")
w       <- melt(column[[10]], value.name = "wwind")
ws      <- melt(column[[6]], value.name = "wspd")
wd      <- melt(column[[7]], value.name = "dir")
#column=list(pr1,tc1,td1,u1,v1,ws1,wd1,rh1)
#rs[ "press"] <- pr_
sounding[, "height"]  <- zp
sounding[, "temp"]  <- tc
sounding[, "dewpt"] <- td
sounding[, "rh"]    <- rh
sounding[, "uwind"] <- u
sounding[, "vwind"] <- v
sounding[, "wwind"] <- w
sounding[, "wspd"]  <- ws
sounding[, "dir"]   <- wd
if (viewtable){
  View(sounding) 
  write.table(sounding,file=paste0(col,row,tim,'.txt'))
  save(sounding,file=paste0(col,row,tim,'.RData'))
}

if(winds){
  if (zoom ){plotsonde(sounding, zoom=TRUE, winds=TRUE)}
  else if (!zoom) { plotsonde(sounding, zoom=FALSE, winds=TRUE)}
}
else if ( !winds){
if (zoom ){plotsonde(sounding, zoom=TRUE, winds=FALSE)}
else if (!zoom) { plotsonde(sounding, zoom=FALSE, winds=FALSE)}
}

}
