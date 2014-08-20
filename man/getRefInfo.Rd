\name{getRefInfo}
\alias{getRefInfo}
\title{Extract projection and extent from an ARPS netcdf file}
\source{
\tabular{ll}{ Package: \tab aRps\cr Type: \tab Package\cr
Version: \tab 0.2\cr Date: \tab
\Sexpr[echo=TRUE]{paste0(getYear(Sys.Date()),"-",getMonth(Sys.Date()),"-",getDay(Sys.Date()))}\cr
License: \tab GPL (>= 2)\cr LazyLoad: \tab yes\cr }
}
\usage{
getRefInfo(file)
}
\arguments{
  \item{file}{is a filname of an ARPS netcdf file}
}
\value{
getRefInfo returns the following parameters: \tabular{ll}{
$proj \tab proj4 projection string\cr $lat \tab central
latitude of the data domain\cr $lon \tab central longitude
of the data domain\cr $dx \tab grid spacing in x
direction\cr $dy \tab grid spacing in y direction\cr $ext
\tab xmin,xmax,ymin,ymax in $proj\cr }
}
\description{
The information of the projection is calculated using the
informations as derived by the gdalinfo call of the ARPS
netcdf file. Currently only "lambert_conformal_conic" is
supported. \code{\link{getRefInfo}} provides all missing
projection values of an ARPS netCDF file
}
\examples{
#### Example to extract the projection and
 #### calculate the extent coordinates from a ARPS nccdf file

arpsexample=system.file("allgaeu_d1_ARP.nc", package="aRps")
refInfo=getRefInfo(arpsexample)
refInfo
}
\author{
Chris Reudenbach and Hanna Meyer
}
\references{
\url{http://giswerk.org/doku.php?id=doku:modeling:arps:arps_installation}
}
\seealso{
For retrieving and writing projected data
see\code{\link{derive4dParam}}.
}

