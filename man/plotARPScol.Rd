\name{plotARPScol}
\alias{plotARPScol}
\title{Extract single ARPS columns and plots data as skew-T, log p diagram}
\source{
\tabular{ll}{ Package: \tab aRps\cr Type: \tab Package\cr
Version: \tab 0.2\cr Date: \tab
\Sexpr[echo=TRUE]{paste0(getYear(Sys.Date()),"-",getMonth(Sys.Date()),"-",getDay(Sys.Date()))}\cr
License: \tab GPL (>= 2)\cr LazyLoad: \tab yes\cr }
}
\usage{
plotARPScol(filename,col, row, tim, zoom, winds=FALSE, viewtable=FALSE)
}
\arguments{
  \item{filename}{of an valid ARPS netCDF file}

  \item{col}{array position of the column to be extracted}

  \item{row}{array position of the row to be extracted}

  \item{tim}{time step of the dataset to be extracted}

  \item{zoom}{if (zoom=TRUE) the skewt log digramm is
  plotted only for a pressure between 1050 and 400 hPa}

  \item{winds}{if (wind=TRUE) a windbarb column is added
  beside the digramm}

  \item{viewtable}{if (viewtable=TRUE) showing result
  table}
}
\value{
plots a skew-T, log p diagram from the required domain
position returns additionale the corresponding table of
data
}
\description{
plotARPScol extract a unique column of the arps ARPS cube.
The derived single data column is converted to a data.frame
as used bei the plotradiosonde function of the adapted
Radiosonde package
}
\examples{
###############
 # get data
 arpsexample=system.file("allgaeu_d1_ARP.nc", package="aRps")
 plotARPScol(arpsexample,10,10,2)
 #plots skew-T, log p diagram derived at array (domain) position 10, 10 at timepos 2 and shows data table
 plotARPScol(arpsexample,10,10,2, zoom=FALSE, winds=TRUE,viewtable=TRUE)
}
\author{
Hanna Meyer, Chris Reudenbach
}
\seealso{
seealso
}
\keyword{keywords}

