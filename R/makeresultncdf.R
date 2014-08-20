makenc<-function(file,var){
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
  library(aRps)
  library(maptools)
  library(maps)
  library(mapdata)
  data(world.cities)
  #file = '/home/creu/progs/opengrads/data/stol_d1_ARP.nc'
  file='/home/creu/Daten/ARPS/marburg_d1.nc'
  outfile='/home/creu/Daten/ARPS/marburg_d1_out_p.nc'
  # get projection and extent of input netCDF file
  refInfo=getRefInfo(file)
  # open input netcdf file
  orig.nc <- open.ncdf(file)
  # generate sequence of x,y coordinates using the refInfos
  tmpx<-seq(refInfo$ext[1],refInfo$ext[2]+1+refInfo$dx, by = refInfo$dx )
  tmpy<-seq(refInfo$ext[3],refInfo$ext[4]+refInfo$dy, by = refInfo$dy )
  #define corresponding x,y,z,time dimensions of the output netcdf file
  nc.x <- dim.def.ncdf( "X", "Meter", tmpx)
  nc.y <- dim.def.ncdf( "Y", "Meter", tmpy)
  nc.z <- dim.def.ncdf( "Z", "level", orig.nc$dim$z$vals)
  nc.t <- dim.def.ncdf( "Time", "seconds", orig.nc$dim$Time$vals, unlim=TRUE)
  # set varname to derive
  orig.nc.varname='P'
  # extract this variable from orig.nc
  extract.var=get.var.ncdf(orig.nc, orig.nc.varname,start=c(1,1,1,1), count=c(length(tmpx),length(tmpy),nc.z$len,nc.t$len) )
  
  # define corresponding varnames for output
  nc.var.varname='pressure'
  nc.var.varshort='pr'
  # Make a empty variable with those dimensions.  Note order: time is LAST
  nc.var <- var.def.ncdf(nc.var.varname,nc.var.varshort, list(nc.x,nc.y,nc.z,nc.t), 1.e30 )
  
  # Create a netCDF file with this variable
  ncnew <- create.ncdf( outfile,nc.var)
  # put data into empty output variable
  put.var.ncdf( ncnew, nc.var, extract.var, start=c(1,1,1,1), count=c(length(tmpx),length(tmpy),nc.z$len,nc.t$len) )
  close.ncdf(ncnew)

  # reopen new netcdf file
  new.nc<-open.ncdf(outfile)
  
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
  # generate an RNCEP array
  wx.extent=array(dim=c(212,212,10),dimnames=c("x","y","time"))
  # assign coland rownames
  dimnames(wx.extent)=list(coordinates(latlon)[,2],coordinates(latlon)[,1],1:10)
  # put var in the RNCEP array
  wx.extent[,,]=extract.var[,,1:10,2]
  # use of RNCEP visualisation function to plot the data
  NCEP.vis.area(wx.data=wx.extent, layer=2, show.pts=FALSE, draw.contours=TRUE,
                cols=heat.colors(64), transparency=.6,
                map.args=list(database = "worldHires",resolution =0), 
                title.args=list(main=paste("Domain centre Marburg" ),sub =paste("centre Lat:",refInfo$ctrlat," Lon: ",refInfo$ctrlon )),
                interp.loess.args=list(gridlen = c(length(tmpx)/2,length(tmpy)/2),span=0.1))
  #### get additional admin vector layers
  de <- getData('GADM', country='DEU', level=1)
  #reproject y in de
  #de <- spTransform(y, CRS(refInfo$proj))
  # add it to the plot
  # filter cities from world.cities 
  sc <- world.cities[world.cities$country.etc=="Germany" & world.cities$pop >= 500000, ]
  coordinates(sc) <- c("long", "lat")
  proj4string(sc)<-CRS("+proj=longlat +ellps=WGS84")
  plot(sc ,add=T)
  plot(de,add=T)
 ##### find a specific coordinate 

  # define coord of request
  wherelon=8.0
  wherelat=50.0
  # find nearest position to requestet coord in arry
  pos=which(pointDistance(latlongrid, c(wherelon ,wherelat), lonlat=TRUE)==min(pointDistance(latlongrid, c(wherelon ,wherelat), lonlat=TRUE)))
  x.arraypos=which(data.frame(latlon)[,1]==latlongrid[pos,]$x)
  y.arraypos=which(data.frame(latlon)[,2]==latlongrid[pos,]$y)  
  
  
#### define extent and projection of raster brick  
  tst <- brick(outfile, nc.var.varname = extract.var)
  extent <- refInfo$ext
  projection(tst) <-refInfo$proj



  
  # 
  # put the data in a handy form 
  z = pt         # matrix of elevations
  x = (ext[1]-3000)+3000* (1:nrow(pt))   # meter spacing (S to N)
  y = (ext[3]-3000)+3000* (1:ncol(pt))   # meter spacing (E to W)
  
  # define the netcdf coordinate variables -- note these have values!
  
  dim1 = dim.def.ncdf( "EW","meters", as.double(x))
  dim2 = dim.def.ncdf( "SN","meters", as.double(y))
  
  # define the EMPTY (elevation) netcdf variable
  
  varz = var.def.ncdf("Potential Temperature","Kelvin", list(dim1,dim2), -1, 
                      longname="Potential Temp of Arps run")
  
  # associate the netcdf variable with a netcdf file   
  # put the variable into the file, and
  # close
  
  nc.ex = create.ncdf( "example.nc", varz )
  put.var.ncdf(nc.ex, varz, pt)
  close.ncdf(nc.ex)
  
  
  
  
  # get pressure (Pa)
  p = get.var.ncdf( nc2, "P", start=c(1,1,2,1), count=c(5,5,1,1) )
  x <- get.var.ncdf( nc, "x")
  
  xx <- dim.def.ncdf( "Lon", "degreesE", ext[1]:ext[2],longvector)
  yy <- dim.def.ncdf( "Lat", "degreesN", as.double(-89:89))
  tt <- dim.def.ncdf( "Time", "days since 1900-01-01", 1:10, unlim=TRUE)
  
  # get u wind vector (m/s)
  u <- get.var.ncdf( nc, "U")    
  # get V wind vector (m/s)
  v <- get.var.ncdf( nc, "V")    
  # get V wind vector (m/s)
  #w <- get.var.ncdf( nc, "W")   
  close.ncdf(nc2)
  
  # do some pseudo destaggering
  uxdim=dim(u)[1]
  uydim=dim(u)[2]
  vxdim=dim(v)[1]
  vydim=dim(v)[2]
  ldim=dim(u)[3]
  tdim=dim(u)[4]
  u=slice (u, i=1:uxdim-1 ,j=1:uydim,k=1:ldim,l=1:tdim)
  v=slice (v, i=1:vxdim ,j=1:vydim-1,k=1:ldim,l=1:tdim)
  rm(u)
  rm(v)
  p = get.var.ncdf( nc, "P")
  # get potentialk Temperatur in K
  pt = get.var.ncdf( nc2, "PT")
  # get Water Vapor Mixing Ratio (g/kg) 
  qv <- get.var.ncdf( nc, "QV")
  # calculate windspeed (m/s)
  ws=sqrt(u^2+v^2)
  # calculate winddirection in degree
  wd=180+atan2(u,v)*57.295
  
  # calculate dry bulb temperature from potential temperature using exner function
  # first calculate Exner pressure (e_p)
  
  ep = (p / 100000.0) ^ (287.058 / 1005.0)
  # calculate dry bulb temperature using exner pressure
  # Air Temp in ° C
  tc = ep * pt -273.15
  save(tc,file='tc.Rdata')
  
  # Air Temp in Kelvin (K)
  #tk = ep * pt
  
  
  # Computes the saturation vapour pressure over water at temperature t (K).
  # es in hPas, t in K
  if (any(param=="es")){
    #  a0 = 23.832241 - 5.02808 * log10(tk)
    #  a1 = 0.00000013816 * 10 ^ (11.344 - 0.0303998 * tk)
    #  a2 = 0.0081328 * 10 ^ (3.49149 - 1302.8844 / tk)
    #  es = (10 ^ (a0 - a1 + a2 - 2949.076 / tk))
    
    # alternative calculation 
    # a,b params for water, water dewpoint and ice must be set
    #  T > 0 above water
    a <- replace(tc, tc >= 0, 7.5)
    b <- replace(tc, tc >= 0,  237.3)
    # T < 0 above ice Eis (freezing point)
    a <- replace(a, tc <0, 9.5)
    b <- replace(b, tc <0,  285.5)
    # Saturation vapor pressure alternative calculatio (hPa)
    es = 6.1078 * 10^((a*tc)/(b+tc))
    save(es,file='es.Rdata')
    rm(es)
  }
  
  if (any(param=="e")){
    # Water vapor specific humidity qv (g/kg)
    # water vapour partial pressure e (Pa)
    # pressure p (Pa)
    # Ra specific gas constant air 287.06
    # Rw specifs constant water vapor  461.6
    # e = p/(Ra/Rw)*qv (Pa)
    # e = e/100 (hPa)
    e = (p/0.622 * qv)/100
  }
  
  if (any(param=="td")){
    #  Dew-point temprature (Td in C)  from vapor pressure (e) 
    # T> 0 above water
    a <- replace(tc, tc >= 0, 7.6)
    b <- replace(tc, tc >= 0,  240.7)
    # T < 0 above ice Eis (freezing point)
    a <- replace(a, tc <0, 9.5)
    b <- replace(b, tc <0,  285.5)
    v = log10(e/6.1078)
    # Dewpoint Temperature C
    td = b*v/(a-v)  
    
    #alterntive calculation dewpoint C
    #td = (243.5 * log(e/6.112)) / (17.67 - log(e/6.112))
  }
  
  if (any(param=="rh")){
    # calculate relative air humidity
    rh=(e/es)*100 
  }
  if (any(param=="pr")){
    # convert pressure from Pa to hPa
    pr=p/100.0
  }
  
  
  result=list()
  for (i in 1:length(param)){
    result[[i]]=get(param[i])
  }
  names(result)=param
  return (result)  
}
