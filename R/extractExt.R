extractExt=function(file,proj){
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