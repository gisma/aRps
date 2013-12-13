\name{ExampleSonde}
\alias{ExampleSonde}
\alias{ExampleSondeText}
\title{Example of radiosonde observations}

\description{
The \code{ExampleSonde} data frame has 461 rows and 21 columns.
The columns are different physical measurements made during the balloon's
ascent. \code{ExampleSondeText} is an image of a text file that allows for testing the 
getsonde function and is an example of a format with metadata used to document a 
sounding. 

These data are a sounding from a well known field experiment TOGA/COARE studying the 
larege scale dynamical relationship of the Tropical Pacific and the atmospshere. 
This particular sounding was launched January 17, 1993 17:12 GMT from Kavieng, New Guninea. 
To write out the text dataset to a sounding file just use:

\code{ data(ExampleSondeText) ;  write( ExampleSondeText, file="sondedata.txt")}

}
\format{
    This data frame contains the following named components:

    \tabular{ll}{
         time   \tab Time in seconds from the ballon's release, a numeric vector. \cr
         press  \tab Pressure (mb), a numeric vector \cr
         temp   \tab Dry-bulb Temperature (deg. C), a numeric vector \cr
         dewpt  \tab Dew point temperaure (deg. C), a numeric vector \cr
         rh     \tab Relative humidity (\%), a numeric vector \cr
         uwind  \tab East-West component of the wind, a numeric vector \cr
         vwind  \tab North-South component of the wind, a numeric vector \cr
         wspd   \tab wind speed  (m/s), a numeric vector \cr
         dir    \tab Wind direction, a numeric vector \cr
         dz     \tab Ascension rate (m/s), a numeric vector \cr
         lon    \tab Longitude, a numeric vector \cr
         lat    \tab Latitude, a numeric vector \cr
         rng    \tab range, a numeric vector \cr
         az     \tab azimuth (angle along horizon), a numeric vector \cr
         alt    \tab altitude (m), a numeric vector \cr
         qp     \tab QC flag for pressure, a numeric vector \cr
         qt     \tab QC flag for temperature, a numeric vector \cr
         qh     \tab QC flag for humidity, a numeric vector \cr
         qu     \tab QC flag for U component, a numeric vector \cr
         qv     \tab QC flag for V component, a numeric vector \cr
         quv    \tab QC flag for ascension rate, a numeric vector \cr
    }
}
\note{Quality Control (QC) flags are generated locally at JOSS based on either
automated or visual checks made.\cr
999.0 implies the QC information is missing, \cr
1.0 implies datum seems physically reasonable (good),\cr
2.0 implies datum questionable on a physical basis (maybe),\cr
3.0 implies datum seems to be in error (bad), \cr
4.0 implies datum interpolated (estimated), \cr
9.0 implies datum missing in original file.  }
\seealso{\code{\link{getsonde}} ,
         \code{\link{skewt.lines}},
         \code{\link{skewt.points}},
         \code{\link{plotsonde}} }
\keyword{datasets}
