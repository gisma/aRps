\name{getexamplesounding}
\alias{getexamplesounding}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ getexamplesounding extract from a vertical column of the ARPS output standard sounding parameters


}
\description{
The routine extracts from an arbitrary column and timestep of the ARPS example data file the standard sounding data. The resulting data.frame can be used by the plotsonde function to plot a rawinsounding as a skewt-log diagram
}
\usage{
getexamplesounding(col, row, tim, zoom, winds = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{col}{
The number of the column to be extracted
}
  \item{row}{
The number of the row to be extracted
}
  \item{tim}{
The time step of the dataset that should be extracted
}
  \item{zoom}{
if (zoom==TRUE) the skewt log digramm is plotted only for a pressure between 1050 and 400 hPa
}
  \item{winds}{
if (wind==TRUE) a windbarb column is added beside the digramm
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
the resulting dataframe looks like this:

\tabular{rllllllll}{
  \tab press \tab temp \tab dewpt \tab rh \tab uwind \tab vwind \tab wspd \tab dir\cr
  \tab 956.2816 \tab -3.104371 \tab -8.551372 \tab 64.72929 \tab -0.927388 \tab -0.2933325 \tab 1.419727 \tab 139.21605\cr
  \tab 903.0207 \tab -6.060003 \tab -9.253030 \tab 77.23463 \tab -0.927388 \tab -0.3182072 \tab 1.419727 \tab 139.21605\cr
  \tab 852.2169 \tab -8.647230 \tab -10.350819 \tab 86.96480 \tab -1.743931 \tab -0.3573799 \tab 2.235914 \tab 51.25896\cr
     \tab 803.7259 \tab -12.128829 \tab -13.067703 \tab 92.42893 \tab -2.140920 \tab -0.4556845 \tab 4.062937 \tab 31.80089\cr
 \tab 757.3327 \tab -16.112190 \tab -17.354655 \tab 89.81493 \tab -2.307307 \tab -0.6148502 \tab 6.152695 \tab 22.02693\cr
 \tab 713.0648 \tab -18.376847 \tab -24.397483 \tab 58.32759 \tab -4.047227 \tab -0.8876823 \tab 10.859465 \tab 21.88384\cr
 \tab 671.1069 \tab -19.696056 \tab -34.324269 \tab 25.45263 \tab -6.581403 \tab -1.2982168 \tab 15.761286 \tab 24.68323\cr
 \tab 631.4566 \tab -20.211362 \tab -38.475418 \tab 17.54258 \tab -9.114533 \tab -1.4796765 \tab 20.596943 \tab 26.26679\cr
  \tab 594.1414 \tab -20.470665 \tab -38.300416 \tab 18.27550 \tab -11.312689 \tab -1.4719036 \tab 24.872080 \tab 27.05639\cr
 \tab 558.8318 \tab -22.813807 \tab -35.676296 \tab 29.40366 \tab -11.964995 \tab -1.3566560 \tab 25.374941 \tab 28.13560\cr
 \tab 525.2889 \tab -25.773756 \tab -36.760957 \tab 34.57229 \tab -12.339058 \tab -1.4039979 \tab 25.557884 \tab 28.86975\cr
 \tab 493.3732 \tab -28.899487 \tab -39.392304 \tab 35.42830 \tab -12.704635 \tab -1.5205818 \tab 26.124077 \tab 29.10098\cr
 \tab 463.0140 \tab -32.114169 \tab -43.601386 \tab 31.02343 \tab -13.173757 \tab -1.7123421 \tab 28.013795 \tab 28.05322\cr
 \tab 434.1548 \tab -35.209111 \tab -47.763759 \tab 26.77561 \tab -13.592569 \tab -1.9086518 \tab 29.995752 \tab 26.94804\cr
 \tab 406.7557 \tab -38.253866 \tab -51.932663 \tab 22.78006 \tab -13.952765 \tab -2.1122829 \tab 32.016797 \tab 25.83800\cr
 \tab 380.7670 \tab -41.129432 \tab -55.344709 \tab 20.62946 \tab -13.827488 \tab -2.2844347 \tab 33.528327 \tab 24.35857\cr
 \tab 356.1506 \tab -43.950944 \tab -58.415457 \tab 19.26542 \tab -13.559421 \tab -2.4437896 \tab 34.835529 \tab 22.90943\cr
 \tab 332.8498 \tab -46.784042 \tab -60.148827 \tab 21.19190 \tab -13.194642 \tab -2.5356595 \tab 35.155552 \tab 22.04635\cr
 \tab 310.8177 \tab -49.578034 \tab -60.704341 \tab 26.97640 \tab -13.194642 \tab -2.5654021 \tab 35.155552 \tab 22.04635\cr
}

}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Hanna Meyer, Chris Reudenbach
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{ \code{\link{derive4dParam}}, 
          \code{\link{getarpsoundparam}}, 
          \code{\link{plotsonde}} }
\examples{
##---- extract at position  x=10, y=10 and timestep 2 a vertical column of 
##---- pressure,temperature, dewpoint, relative humidity, u, v, windspeed, winddir
##---- the data.frame (sounding) is returned by the function
## (1) opens the example dataset
## (2) get the full set of variables
## (3) generates an example sounding

arpsexample=open.ncdf(system.file("allgaeu_d1_ARP.nc", package="aRps"))
cparam=derive4dParam(arpsexample)
sounding = getexamplesounding(10, 10, 2) 

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
