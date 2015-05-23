plotxy<-function(nc,lev,tim){
############################################  
##### ok let's plot  
library(aRps)
library(maptools)
library(maps)
library(mapdata)
library(RNCEP)
data(world.cities)
timeslot=2
lev=1
# generate the RNCEP array they have to be 3D!!!!!!!!!!!!
wx.extent=array(dim=c(nc.x$len,nc.y$len,nc.z$len),dimnames=c("x","y","z"))

# also here we have to deal with different data sources ie projections
# if there is only lcc data w
if (refInfo$proj!="+proj=longlat +datum=WGS84 +no_defs")
{
  # generate dataframe of xy coords
  points=data.frame(tmpx,tmpy)
  # define colnames of dataframe
  colnames(points)=c('x','y')
  # convert dataframe to coordinates
  coordinates(points) <- c("x", "y")
  # assign crs to coordinates
  proj4string(points) <- CRS(refInfo$proj)
  #reproject the coordinats to geographic latlon
  latlon=spTransform(points,CRS("+proj=longlat +ellps=WGS84"))
  # generate a matrix of latlong coordinates
  latlongrid=expand.grid(x=data.frame(latlon)[,1],y=data.frame(latlon)[,2])
  # assign coland rownames
  dimnames(wx.extent)=list(coordinates(latlon)[,2],coordinates(latlon)[,1],1:nc.z$len)
}
# generate a matrix of latlong coordinates
latlongrid=expand.grid(x=tmpx,y=tmpy)
# assign coland rownames
dimnames(wx.extent)=list(tmpy,tmpx,nc.z$vals)
# put var in the RNCEP array
if (varname!='ZP'){
  wx.extent[,,]=extract.var[,,nc.z$len,timeslot] 
}else{
  
  wx.extent[,,]=extract.var[,,]
}
# use of RNCEP visualisation function to plot the data
NCEP.vis.area(wx.data=wx.extent, layer=lev, show.pts=FALSE, draw.contours=TRUE,
              cols=heat.colors(64), transparency=.5,
              map.args=list(database = "worldHires",resolution =0), 
              title.args=list(main=paste("Domain centre Marburg" ),sub =paste("centre Lat:",refInfo$ctrlat," Lon: ",refInfo$ctrlon )),
              interp.loess.args=list(gridlen = c(length(tmpx)/2,length(tmpy)/2),span=0.001))
}