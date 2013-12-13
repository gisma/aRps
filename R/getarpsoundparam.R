getarpsoundparam <- function(col,row,lev,tim){
col=5
row=5
tim=2
### todo loop etc
### todo windspeed
### todo winddirection
lev=dim(cparam$tc)[3]
tc=cparam$tc
td=cparam$td
pr=cparam$pr
#rh=cparam$rh
#u=cparam$u
#v=cparam$v
#w=cparam$w
#ws=cparam$ws
#wd=cparam$wd
pr1=slice (pr, i=col:col ,j=row:row,k=1:lev,l=tim:tim)
tc1=slice (tc, i=col:col ,j=row:row,k=1:lev,l=tim:tim)
td1=slice (td, i=col:col ,j=row:row,k=1:lev,l=tim:tim)
pr_ <- melt(pr1, value.name = "press")
tc_ <- melt(tc1, value.name = "temp")
td_  <- melt(td1, value.name = "dewpt")
rs[, "temp"] <- pr_
rs[, "temp"] <- tc_
rs[, "dewpt"] <- td_
View(rs)
plotsonde(rs, winds=FALSE)
}