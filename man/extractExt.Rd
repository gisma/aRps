\name{extractExt}
\alias{extractExt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calclates the extent of a netcdf file
}
\description{
  Calculates the extent of a netcdf file according to its reference system
}
\usage{
  extractExt(file,proj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
    A netcdf file
  }
    \item{proj}{
    The proj4 string preferably idenified with \code{\link{extractProj}}
  }
}

\value{
  A vector representing the extent of the netcdf object.
}

\author{
  Chris Reudenbach and Hanna Meyer
}
\seealso{
  To identify the reference system see \code{\link{extractProj}}. Use alwas first extractProj and then extractExt!!
}
\examples{
  #### Example to determine the extent coordinates from a nc file
  arpsexample=system.file("allgaeu_d1_ARP.nc", package="aRps")
  proj=extractProj(arpsexample)
  ext=extractExt(arpsexample,proj)
}