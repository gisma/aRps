\name{plotsonde}
\alias{plotsonde}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Creates a SKEW-T, log p diagram for a radiosonde dataframe.}
\description{
   Creates a SKEW-T, log p diagram with dry-bulb temperature and 
   dewpoint temperature traces versus (log) pressure. Optionally plots
   the vertical wind profile using wind barbs.
}
\usage{ plotsonde(dataframe, skewT=TRUE, winds=FALSE, site="", title="",
           windplot=NULL, s=3, col=c(1, 2), ... ) }

\arguments{
  \item{dataframe}{ R dataframe with  at least the components 
\code{c( "press", "temp",  "dewpt")} corresponding to pressure (hPa), temperature (C) and dew point temperature (C).   }
  \item{skewT}{ Logical value.  If false, will not plot the 
	        SKEW-T, log p diagram. }
  \item{winds}{ Logical value.  If false, will not plot the
		winds profile. }
  \item{site}{ Optional character to add to plot title. }
  \item{title}{ Title for plot (must be a character value). }
  \item{windplot}{ 4X1 numeric vector that gives the position of wind profile 
		   plot (next to SKEW-T diagram).  Only used if it is desired
		   to have this plot in a different position.  Generally, the 
		   default NULL is used and the placement is chosen 
		   automatically.  }
  \item{s}{ Size of winds profile plot.  }
  \item{col}{ Usual plotting parameter. }
  \item{...}{ Other optional plotting parameters. }
}
\value{
  No value returned.  Creates a  skewt plot. Note that default is temperature horizontal scale is Farenheit. 
}
\references{ 
	1.  Department of Defense, 1969, "USAF SKEW-T, log p DIAGRAM,"
	DOD-WPC-9-16-1, Aeronautical Chart and Information Center,
	United States Air Force, St. Louis, Missouri 63118.

	2.  List, R.J. (editor), 1958, \emph{Smithsonian Meteorological Tables},
	Smithsonian Institute, Washington, D.C.

	3.  Nordquist, W.S., 1973, "Numerical Approximations of Selected
	Meteorological Parameters for Cloud Physics Problems," ECOM-5475,
	Atmospheric Sciences Laboratory, US Army Electronics Command,
	White Sands Missile Range, New Mexico 88002.

	4.  Stipanuk, G.S., 1973, "Algorithms for Generating a SKEW-T, log p
	Diagram and Computing Selected Meteorological Quantities," American
	Sciences Laboratory, US Army Electronics Command, White Sands Missile
	Range, New Mexico 88002.

	5. \url{http://www.atd.ucar.edu/dir_off/tc_corr/index.html}

	6. \url{http://weather.unisys.com/upper_air/skew/details.html}
}
\author{ Tim Hoar, Eric Gilleland, Doug Nychka}
\seealso{ \code{\link{getsonde}}, 
          \code{\link{skewt.axis}} ,
          \code{\link{skewt.lines}} ,
          \code{\link{skewt.points}} }

\examples{
data(ExampleSonde)
plotsonde(ExampleSonde, winds=TRUE)
}
\keyword{ hplot }% at least one, from doc/KEYWORDS
\keyword{ aplot }% __ONLY ONE__ keyword per line
