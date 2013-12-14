getarpsoundparam <- function(col,row,lev,tim,winds=FALSE){

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
# getarpsoundparam extract a unique column of the arps data cube. the derived single
# data columns are converted to a data.frame as used bei the plotradiosone function
# 
  
#col=5
#row=5
#tim=2
### todo loop etc
### todo windspeed
### todo winddirection
lev=dim(cparam$tc)[3]
tc=cparam$tc
td=cparam$td
pr=cparam$pr
rh=cparam$rh
u=cparam$u
v=cparam$v
#w=cparam$w
ws=cparam$ws
wd=cparam$wd
pr1=slice (pr, i=col:col ,j=row:row,k=1:lev,l=tim:tim)
tc1=slice (tc, i=col:col ,j=row:row,k=1:lev,l=tim:tim)
td1=slice (td, i=col:col ,j=row:row,k=1:lev,l=tim:tim)
u1=slice (u, i=col:col ,j=row:row,k=1:lev,l=tim:tim)
v1=slice (v, i=col:col ,j=row:row,k=1:lev,l=tim:tim)
ws1=slice (ws, i=col:col ,j=row:row,k=1:lev,l=tim:tim)
wd1=slice (wd, i=col:col ,j=row:row,k=1:lev,l=tim:tim)
rh1=slice (rh, i=col:col ,j=row:row,k=1:lev,l=tim:tim)
rs <- melt(pr1, value.name = "press")
tc_ <- melt(tc1, value.name = "temp")
td_  <- melt(td1, value.name = "dewpt")
rh_  <- melt(rh1, value.name = "rh")
u_  <- melt(u1, value.name = "uwind")
v_  <- melt(v1, value.name = "vwind")
ws_  <- melt(ws1, value.name = "wspd")
wd_  <- melt(wd1, value.name = "dir")
#rs[ "press"] <- pr_
rs[, "temp"] <- tc_
rs[, "dewpt"] <- td_
rs[, "rh"] <- rh_
rs[, "uwind"] <- u_
rs[, "vwind"] <- v_
rs[, "wspd"] <- ws_
rs[, "dir"] <- wd_
View(rs)

if( winds==TRUE){
plotsonde(rs, winds=TRUE)
}
else if ( winds==FALSE){
  plotsonde(rs, winds=FALSE)
}
}