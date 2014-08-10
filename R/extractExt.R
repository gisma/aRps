extractExt=function(file){

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
  #  
  # extractExt identifies the extension of an arbitrary ARPS model run ut in netCDF format
  
  # t<-GDALinfo("inst/allgaeu_d1_ARP.nc" ,returnScaleOffset=FALSE, returnRAT=TRUE)
  #dx=as.numeric(strsplit(attributes(t)$mdata[13][which(grepl(attributes(t)$mdata[13], attributes(t)$mdata[13]))],"=")[[1]][2])
  tmp=extractProj(file)
  proj=tmp$proj
  lat0=tmp$lat
  lon0=tmp$lon
  dx=tmp$dx
  dy=tmp$dy
  testncdf=open.ncdf(file)
  x = length(get.var.ncdf( testncdf, "x"))
  y = length(get.var.ncdf( testncdf, "y"))
  write.table(cbind(lat0,lon0),'incclatlon.txt',row.names=FALSE,col.names=FALSE,sep=' ')
  system(paste("gdaltransform -s_srs EPSG:4326  -t_srs '", proj,"' < incclatlon.txt > outcclcc.txt"))
  tmp=read.table("outcclcc.txt")
  xmin=tmp[1,2]-x/2*dx
  xmax=tmp[1,2]+x/2*dx
  ymin=tmp[1,1]-y/2*dy
  ymax=tmp[1,1]+y/2*dy
  #write.table(matrix(c(xmin,ymin,xmax,ymax),ncol=2,nrow=2,byrow=T),"aRpsIn.txt",row.names=FALSE,col.names=FALSE)
  #system(paste("gdaltransform -s_srs EPSG:4326  -t_srs ","'", proj,"'"," < aRpsIn.txt > aRpsOut.txt"))
  #tmp=read.table("aRpsOut.txt")
  ext=c(xmin,xmax,ymin,ymax)
  system("rm incclatlon.txt")
  system("rm outcclcc.txt")
  return (ext)
}