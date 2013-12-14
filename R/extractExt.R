extractExt=function(file,proj){

  
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
  require(ncdf)
  testncdf=open.ncdf(file)
  x = length(get.var.ncdf( testncdf, "x"))
  y = length(get.var.ncdf( testncdf, "y"))
  xmin=11.1332
  xmax=xmin+(x*0.0269)
  ymin=44.5809
  ymax=ymin+(y*0.0181)
  write.table(matrix(c(xmin,ymin,xmax,ymax),ncol=2,nrow=2,byrow=T),"aRpsIn.txt",row.names=FALSE,col.names=FALSE)
  system(paste("gdaltransform -s_srs EPSG:4326  -t_srs ","'", proj,"'"," < aRpsIn.txt > aRpsOut.txt"))
  tmp=read.table("aRpsOut.txt")
  ext=c(tmp[1,1],tmp[2,1],tmp[1,2],tmp[2,2])
  system("rm aRpsOut.txt")
  system("rm aRpsIn.txt")
  return (ext)
}