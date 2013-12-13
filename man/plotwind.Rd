\name{plotwind}
\alias{plotwind}
\title{ Winds Profile Plot }
\description{ Creates a wind profile in the standard atmospheric 
              notation. Each full barb = 10 m/s, half barb = 5 m/s, 
              and a filled triangle for 50 m/s.  The maximum wind 
              speed that can be plotted without amending the program 
              is 65 m/s. }

\usage{ plotwind(dataframe, size=5, ylim=c(1050, 100), legend=FALSE) }

%- maybe also `usage' for other objects documented here.

\arguments{
  \item{dataframe}{ Data frame for sounding data, must have components for 
                    wind speed \code{wspd}, wind direction \code{dir}, 
                    and pressure \code{press}.}
  \item{size}{change the thickness of the plotted lines}
  \item{ylim}{plot extent, in units of pressure}
  \item{legend}{explains wind barb strategy}
}

\details{ the dataframe must have components for wind speed \code{wspd}, 
          wind direction \code{dir}, and pressure \code{press}.  
          Missing values may be coded as either NA or 999. and are 
          not plotted.\cr
          The standard atmospheric wind sybmol is a vector of fixed length 
          with barbs proportional to wind speed. A full barb for each 10 m/s,  
          half barbs for 5 m/s and a triangular barb for 50 m/s. }

\value{None -- creates a plot.}

\author{ Eric Gilleland, Tim Hoar, Doug Nychka }

\seealso{ \code{\link{getsonde}}, \code{\link{plotsonde}} }

\examples{
# Example 1:
data(ExampleSonde)
plotwind(ExampleSonde)
# Example 2:
plotwind(ExampleSonde, size = 7, legend=TRUE)
}
\keyword{ hplot }          % at least one, from doc/KEYWORDS
