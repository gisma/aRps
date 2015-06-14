#'@name plotARPSskewT
#'@aliases plotARPSskewT
#'@title Extract single ARPS columns and plots data as skew-T, log p diagram 
#'@description
#' plotARPSskewT extract a unique column of the arps ARPS cube. The derived single
#' data column is converted to a data.frame as used bei the plotradiosonde function of the adapted Radiosonde package
#'@usage  plotARPSskewT(file,col, row, tim, zoom = TRUE, winds=FALSE, writesounding=FALSE)
#'@param file  ARPS netCDF file
#'@param col   array position of the column to be extracted
#'@param row   array position of the row to be extracted
#'@param tim   time step of the dataset to be extracted
#'@param zoom  if (zoom=TRUE) the skewt log digramm is plotted only for a pressure between 1050 and 400 hPa
#'@param winds  if (wind=TRUE) a windbarb column is added beside the digramm
#'@param writesounding  if (writesounding=TRUE) showing result table

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
#'
#'@examples
#'  # get data
#'  arps.ncfile=system.file("kili.nc", package="aRps")
#'  nc <- nc_open(arps.ncfile)
#'  
#'  # plots skew-T, log p diagram domain position 10, 10 at timepos 2 
#'  plotARPSskewT(nc,10,10,2,zoom=TRUE, winds=FALSE,writesounding=FALSE)
#'  
#'  ### plots skew-T, log p diagram domain position 10, 10 at timepos 2 and shows data table
#'  plotARPSskewT(nc,10,10,2, zoom=FALSE, winds=TRUE,writesounding=TRUE) 

#'@import arrayhelpers reshape2
#'@export plotARPSskewT 
#'@keywords keywords
#'


plotARPSskewT <- function(file,col,row,tim,zoom=TRUE,winds=FALSE,writesounding=FALSE){

  # extract column data
  column=getARPSsounding(file,col,row,tim)
  
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
  if (writesounding){
    sounding 
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
