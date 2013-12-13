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
data(ExampleSonde)
skewt.axis()
skewt.lines( ExampleSonde$temp,  ExampleSonde$press, col = 6)
skewt.lines( ExampleSonde$dewpt, ExampleSonde$press, col = 7)
skewt.points(ExampleSonde$temp,  ExampleSonde$press, col = 3)
skewt.points(ExampleSonde$dewpt, ExampleSonde$press, col = 4)
}

\keyword{ aplot }
