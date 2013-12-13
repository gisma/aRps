\name{station.symbol}
\alias{station.symbol}
\title{ Adds a meteorological surface station annotation to a plot. }
\description{ Adds a meteorological surface station annotation at the given 
              coordinates. The annotation includes speed and direction of the 
              wind, temperature, pressure, dewpoint ...  to a given plot. }

\usage{
station.symbol(cx, cy, direction=0, speed=NA, fill=rep(0,length(cx)), temp=NA,
                  press=NA, dewpt=NA, circle=TRUE, cex=1)
}

\arguments{
  \item{cx}{x coordinate for location of the annotation.}
  \item{cy}{y coordinate for the annotation.}
  \item{direction}{Wind direction.}
  \item{speed}{Wind speed.}
  \item{fill}{Fill for visibility: 0 = clear skies, 1 = 25\% obscured, 
              2 = 50\% obscured, 3 = 75\% obscured, 4 = no visibility }
  \item{temp}{Temperature value to plot symbol (must be a scalar).
              If NULL then no temperature value will be annotated.}
  \item{press}{Pressure value to plot symbol (must be a scalar).
              If NULL then no pressure value will be annotated.}
  \item{dewpt}{Dewpoint Temperature value to plot symbol (must be a scalar).
               If NULL then no dewpoint value will be annotated.}
  \item{circle}{If TRUE, will plot the usual station symbol with a circle 
                at its base.}
  \item{cex}{Usual plotting parameter. }
}

\value{ Adds to a plot. }

\author{ Doug Nychka, Eric Gilleland }

\seealso{ \code{\link{plotsonde}}, 
          \code{\link{plotwind}}, 
          \code{\link{skewt.axis}}
}

\examples{
   plot(0:1, 0:1, type="n")
   station.symbol(0.5, 0.5, direction=130, speed=30, fill=3, 
                  temp=31, press=987, dewpt=26, cex=5)
}

\keyword{ aplot }
