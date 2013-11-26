\name{extractProj}
\alias{extractProj}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Extract Projection information from a netcdf file
}
\description{
  The information of the projection is build up by reading the necessary parameters from the netcdf file. Currently only "lambert_conformal_conic" is supported.
}
\usage{
  extractProj(file)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{
    A netcdf file
  }
}

\value{
  A proj string.
}

\author{
  Chris Reudenbach and Hanna Meyer
}
\seealso{
  For adaption of the extent according to the identified reference system see \code{\link{extractExt}}. Use alwas first extractProj and then extractExt!!
}
\examples{
  #### Example to extract the proj4 information from a nc file
  arpsexample=system.file("allgaeu_d1_ARP.nc", package="aRps")
  proj=extractProj(arpsexample)
}