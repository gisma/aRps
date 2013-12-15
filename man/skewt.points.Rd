\name{skewt.points}
\alias{skewt.points}
\title{Overlays data on a SKEW-T, log p axis}
\description{Overlays observations on a SKEW-T, log p axis 
             (as created by \code{skewt.axis}). }

\usage{ skewt.points(temp, pressure, ...) }

\arguments{
  \item{temp}{ Temperature in degrees C. }
  \item{pressure}{ Pressure in millibars }
  \item{...}{ Any graphical arguments }
}

\details{\code{skewt.points} overlays observations on a SKEW-T, log p axis}

\seealso{ \code{\link{skewt.axis}}, 
          \code{\link{skewt.lines}}, 
          \code{\link{plotsonde}} }

\examples{
# draw a background, then
#    draw the temperature (with a solid line) in color 6
#    draw the dewpoint in color 7
#    overlay the temperature observations in a different color
#    you get the point ...

arpsexample=open.ncdf(system.file("allgaeu_d1_ARP.nc", package="aRps"))
cparam=derive4dParam(arpsexample)
sounding = getexamplesounding(10, 10, 2) 
skewt.axis()
skewt.lines( sounding$temp,  sounding$press, col = 6)
skewt.lines( sounding$dewpt, sounding$press, col = 7)
skewt.points(sounding$temp,  sounding$press, col = 3)
skewt.points(sounding$dewpt, sounding$press, col = 4)
}
\keyword{ aplot }
