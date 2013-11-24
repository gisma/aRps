\name{derivParam}
\alias{derivParam}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calculate climatic parameters derivated from the arps output
}
\description{
  Calculate air temperature (t) ,water saturation pressure (es), water partial pressure (e), relative air humidity (rh) from arps output
}
\details{
\itemize{
  \item air temperature \eqn{t =(\frac{pt}{(1000/p)})^0.287}
  \item water saturation pressure \eqn{es = 6.107 * (10^(\frac{(7.5 * t)}{(235 + t)}))}
  \item water partial pressure \eqn{e = \frac{(p * qv)}{622}}
  \item relative air humidity \eqn{rh=(\frac{e}{es})*1000}
}
with pt is the potential temperature,p is the pressure and qv is the water vapor specific humidity from the inptfile
}
\usage{
  derivParam(x,param=c("t","es","e","rh"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    A netcdf object
  }
  \item{param}{
    A string vector of the parameters to be calculated
  }
}

\value{
  A list of containing the calculated paameters as arrays
}

\author{
  Chris Reudenbach and Hanna Meyer
}
\seealso{
  For adaption of the extent according to the identified reference system see \code{\link{extractExt}}. Use alwas first extractProj and then extractExt!!
}
\examples{
  #### Example to extract the proj4 information from a nc file
  #  arpsexample=system.file("stol_d1_ARP.nc", package="aRps")
  #  cparam=derivParam(arpsexample)
  #  str(cparam) #see which parameters are listed
  #  cparam$t #to extract temperature from the list
}