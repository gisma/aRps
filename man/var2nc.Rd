\name{var2nc}
\alias{var2nc}
\title{Extract one complete variable of an ARPS netcdf file and writes it back to a new netcdf}
\source{
\tabular{ll}{ Package: \tab aRps\cr Type: \tab Package\cr
Version: \tab 0.2\cr Date: \tab
\Sexpr[echo=TRUE]{paste0(getYear(Sys.Date()),"-",getMonth(Sys.Date()),"-",getDay(Sys.Date()))}\cr
License: \tab GPL (>= 2)\cr LazyLoad: \tab yes\cr }
}
\usage{
var2nc(file,var)
}
\arguments{
  \item{file}{is a filname of an ARPS netcdf file}

  \item{var}{is the parameter names
  (see\code{\link{derive4dParam}})}
}
\value{
var2nc returns the following parameters: \tabular{ll}{ new
netcdf file \tab named as <PARAMETER> old netcdf file\cr }
}
\description{
The ARPS netcdf files lack of the correct handling of
projection and extent. Furthermore they are organized after
netcdf3.0 The function var2nc extract all data plus the
desired variable and write a new netcdf 4.0 file.
}
\examples{
#### Example to extract the projection and calculate the extent coordinates from a ARPS nc file

arpsexample=system.file("allgaeu_d1_ARP.nc", package="aRps")
cparam=derive4d4Sound(arpsexample)
var2nc(arpsexample,'QV')
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

