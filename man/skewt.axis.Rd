\name{skewt.axis}
\alias{skewt.axis}
\title{ Draws a SKEW-T, log p axis. }
\description{
Draws a SKEW-T, log p axis. This is the standard axis for interpreting atmospheric
sounding profiles like those collected by the instrument carried aloft by a 
weather balloon (\emph{radiosonde}). Use \code{skewt.lines} or \code{skewt.points} 
to layer information on top of the skewt axis.
}
\usage{
skewt.axis(BROWN="brown", GREEN="green", redo=TRUE, ... )
}
\arguments{
  \item{BROWN}{ Color of lines for both Temperature (solid skewed) and 
                Pressure (dashed horizontal)}
  \item{GREEN}{ Color of lines for dry adiabats (solid) and constant mixing
ratio (dashed) }
  \item{redo}{ flag to generate the adiabats, should \code{skewt.data} become
               corrupt. The adiabats are the result of an iterative process
               and to make smooth curves, you need a lot of points. Hence this
               is time-consuming, so [FALSE] is the preferred value.}
  \item{\dots}{ The usual \code{plot} parameters. }
}
\details{ Radiosondes record temperature, humidity and winds. 
          They can be lifted by weather balloons, dropped from aircraft, 
          there is even something called a glidersonde. The data collected by 
          radiosondes are plotted versus pressure/height to give details on the 
          vertical structure of the atmosphere.  The type of plot is called a 
          SKEW-T, log p diagram.\cr
          \file{skewt.axis} creates the traditional axis for a SKEW-T, log p 
          diagram, including moist and dry adiabats, etc ...\cr
          Generating the necessary plot data is time-consuming, so there is an
          option to read from a specific dataset that Doug will change.
}
\value{ Returns the \code{par()$plt} values, which are used by 
        \code{plotsonde} if you choose to plot the wind profile 
        adjacent to the skewt axis. }

\author{ Tim Hoar }
\seealso{ \code{\link{getsonde}}, 
          \code{\link{plotsonde}}, 
          \code{\link{skewt.lines}}, 
          \code{\link{skewt.points}}  }

\examples{
# draw a background, then
#    draw the temperature (with a solid line) in color 6
#    draw the dewpoint in color 7
#    overlay the temperature observations in a different color
#    you get the point ...
#
data(ExampleSonde)
skewt.axis( mar=c(5.1, 1.1, 2.1, 5.1) )
skewt.lines( ExampleSonde$temp,  ExampleSonde$press, col = 6)
skewt.lines( ExampleSonde$dewpt, ExampleSonde$press, col = 7)
skewt.points(ExampleSonde$temp,  ExampleSonde$press, col = 3)
skewt.points(ExampleSonde$dewpt, ExampleSonde$press, col = 4)
#
# Changing the moist adiabats: you must edit the \code{skewt.axis} function
# directly and then capture the output in \code{skewt.data} to be used in 
# subsequent calls.
skewt.data <- skewt.axis(redo=TRUE)
skewt.axis() 
skewt.axis() 
}
\keyword{ hplot }
