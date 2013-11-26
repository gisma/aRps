extractProj=function(file){
  system(paste("gdalinfo ",file," >aRpsTmp.txt"))
  x=readLines("aRpsTmp.txt", n = -1)[]
  ##extract parameters
  projname=strsplit(x[which(grepl("NC_GLOBAL#grid_mapping_name", x))],"=")[[1]][2]
  lat_1=as.numeric(strsplit(x[which(grepl("NC_GLOBAL#TRUELAT1", x))],"=")[[1]][2])
  lat_2=as.numeric(strsplit(x[which(grepl("NC_GLOBAL#TRUELAT2", x))],"=")[[1]][2])
  lat0=as.numeric(strsplit(x[which(grepl("NC_GLOBAL#CTRLAT", x))],"=")[[1]][2])
  lon0=as.numeric(strsplit(x[which(grepl("NC_GLOBAL#CTRLON", x))],"=")[[1]][2])
  x_0=as.numeric(strsplit(x[which(grepl("NC_GLOBAL#false_easting", x))],"=")[[1]][2])
  y_0=as.numeric(strsplit(x[which(grepl("NC_GLOBAL#false_northing", x))],"=")[[1]][2])
  #paste parameters
  if (projname!="lambert_conformal_conic"){
    stop("Currently only 'lambert_conformal_conic' is supported")
  }
  proj=paste("+proj=lcc +lat_1=",lat_1," +lat_2=",lat_2,
             " +lat_0=",lat0," +lon_0=",lon0, " +x_0=",x_0," +y_0=",y_0,sep="")  
  system("rm aRpsTmp.txt")
  return (proj)
}
