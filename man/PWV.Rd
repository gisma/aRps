\name{PWV}
\alias{PWV}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{ Precipitable Water Vapor }
\description{
Calculates precipitable water (in mm) up to \code{minp} (minimum pressure)
using dew-point temperature (\code{td}) and temperature (\code{temp}) (both
in degrees C).
}
\usage{
PWV(sonde = NULL, press, td, temp, minp = 400)
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{sonde}{ Sonde data read in using \code{getsonde} function. }
  \item{press}{ Pressure (only used if \code{sonde} is NULL) }
  \item{td}{ Dew-point temperature (in deg C) (only used if \code{sonde}
		is NULL) }
  \item{temp}{ Temperature (in deg C) (only used if \code{sonde} is NULL)}
  \item{minp}{ Minimum Pressure }
}
\value{
	single numeric value is returned.
}
\author{ Junhong Wang }

\seealso{ \code{\link{getsonde}}, \code{td.to.q} }

\examples{
# Example 1

data(ExampleSonde)
######
# OK, now find precipitable water vapor.

PWV(ExampleSonde)

}
\keyword{ manip }% at least one, from doc/KEYWORDS
