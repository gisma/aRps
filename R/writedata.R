writedata=function(file){
  
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
  
  ### extractProj identifies the projection of an arps model run netCDF outputfile
  ### actuallay it uses the gdalinfo binary which have to be installed on the system
  #prj<-extractPrj(file)
  #proj=tmp$proj
  tmp<-derive4dParam<-(file)
  ext<-extractExt(file)
  cparam=derive4dParam(file)
  str(cparam)
  tc=cparam$tc[,,2,1]
  tc=raster(tc)
  str(ext)
  lat0=proj$lat
  lon0=proj$lon
  proj[1]
  dx=proj$dx
  dy=proj$dy
  tc@extent@xmin<-ext[1]
  tc@extent@xmax<-ext[2]
  tc@extent@ymin<-ext[3]
  tc@extent@ymax<-ext[4]  
  tc@crs@projargs <- proj
 # writeRaster(tc, filename="test.tif", format="GTiff", overwrite=TRUE)
  rr<-brick(raster(tc))
  
  for(r in 1:nrow(tc)){
    dd <- getValues(tc,r)
    rr <- setValues(rr,dd,r)
    rr <- writeRaster(rr,"ref.img", format="HFA",overwrite=T)
  }
  #stackNC <- stack(file)
  #stackNC@extent@xmin<-ext[1]
  #stackNC@extent@xmax<-ext[2]
  #stackNC@extent@ymin<-ext[3]
  #stackNC@extent@ymax<-ext[4]
  #stackNC@data@haveminmax<-'logi TRUE'
  
}