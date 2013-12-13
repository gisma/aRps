\name{getsonde}
\alias{getsonde}
\title{read a radiosonde file to an R dataframe.}
\description{ \code{getsonde} takes a common radiosonde data file and reads it in as an R dataframe. }
\usage{ getsonde(filename, datakey="------", varkey=" Time", unitkey="  sec") }
\arguments{
  \item{filename}{Full path and file name of radiosonde data.}
  \item{datakey}{character string identifying the line \bold{preceeding} the datastream. 
                 The string gets compared to the first N characters of each line, so 
                 all whitespace is important!}
  \item{varkey}{character string identifying the line containing the variable names.
                 The string gets compared to the first N characters of each line, so 
                 all whitespace is important!}
  \item{unitkey}{character string identifying the line containing the units of the variables.
                 The string gets compared to the first N characters of each line, so 
                 all whitespace is important!}
}

\details{
   \code{getsonde} assumes the data is in a very common format for radiosonde data.
   The files are typically ASCII files with a header of unknown length stating such
   things as the location and time of the data collection etc. Appended to this is
   a table of data where each column is a particular quantity and each row corresponds
   to the time of the observation. This typically has a header identifying the
   quantity in each of the columns and is separated from the data by a particular
   character string. The location of this string in the file is crucial in deciphering
   the start of the datastream from the metadata. For much more about the file format,
   look at the example in    \file{RadioSonde/inst/exampleData/ExampleSonde.txt}. To find the 
   exact path and file name on your system try 
   \code{system.file("exampleData", "ExampleSonde.txt", package = "RadioSonde")}.

}

\value{
   Returns a dataframe with the following items, if present in the input file.
   Note that each of these items can be present in any order in the input file.\cr

  \item{time }{time in seconds since weather balloon launched or recording begins. }
  \item{press}{Pressure in mb. }
  \item{temp }{Dry-bulb Temperature in degrees C. }
  \item{dewpt}{Dew point temperature in degrees C. }
  \item{rhum }{Relative Humidity (Percent).  }
  \item{uwind}{East-West wind component (m/s).  }
  \item{vwind}{North-South wind component (m/s). }
  \item{wspd }{Wind speed (m/s).  }
  \item{wdir }{Wind direction (deg.).  }
  \item{dz   }{Ascension rate of weather balloon (m/s).  }
  \item{lon  }{Longitude of weather balloon.   }
  \item{lat  }{Latitude of weather balloon .  }
  \item{rng  }{Range (see \bold{warning} below)}
  \item{az   }{Azimuth of weather balloon from originating station (see \bold{warning} below) }
  \item{alt  }{Altitude of weather balloon (m).  }
  \item{qp   }{Quality Control (QC) flag for pressure (see note below)}
  \item{qt   }{QC flag for temperature (see note below)}
  \item{qh   }{QC flag for humidity (see note below)}
  \item{qu   }{QC flag for U Component (see note below)}
  \item{qv   }{QC flag for V Component (see note below)}
  \item{quv  }{QC flag for Ascension rate (see note below)}

   If the \code{units} are avialable in the datafile, they are included in 
   the dataframe as the attribute \code{units}.\cr
   
   Rather than throw away the original header information, which usually contains
   valuable metadata, it is included as another attribute: \code{metadata}.\cr
}

\author{ Tim Hoar, Eric Gilleland, Doug Nychka }

\note{
   The Quality Control information (flags): qp, qt, qh, qu, qv, and quv 
   are generated at JOSS are based on the automated or visual checks made.  
   The JOSS QC flags are as follows:\cr
   99.0 means it is unchecked,\cr
   1.0 implies datum seems physically reasonable (good),\cr
   2.0 implies datum seems questionable on physical basis (maybe),\cr
   3.0 implies datum seems to be in error (bad),\cr
   4.0 implies datum is interpolated (estimated), and\cr
   9.0 implies datum was missing in original file (missing).\cr
} % End of note

\section{Warning}{The connotation of fields is not exactly standard. Different
recording systems use these fields in different ways. Hence, they cannot be
automatically interpreted without knowledge of the recording system.}

\seealso{ \code{\link{plotsonde}} , 
          \code{\link{skewt.points}} , 
          \code{\link{skewt.lines}} }

\examples{
# Read a typical radiosonde (ASCII) datafile, headers and all.
# The datakey,varkey,and unitkey arguments come from examining 
# the datafile. The whitespace is important.

# The Sonde text file is stored in this package as a text dataset (this is just
# to make it easier to include in the package.)
# First create a real text file to read in
  data( ExampleSondeText)
  write( ExampleSondeText, file="SondeFile.txt")

  sample.sonde <- getsonde("SondeFile.txt")
# NOTE this assumes that the default datakey, varkey and unitkey. 
# it is best to check the file for these choices!
  attr(sample.sonde,"units")
  attr(sample.sonde,"metadata")

#  skewt plot of data
  plotsonde(sample.sonde,title="SondeFile.txt")

#
# Read the same radiosonde dataset, but ignore the units ... 
#
  datakey  <- "------"
  varkey   <- " Time"
  sample.sonde <- getsonde("SondeFile.txt",datakey,varkey)
  attr(sample.sonde,"metadata")
}
\keyword{ manip }        % at least one, from doc/KEYWORDS
