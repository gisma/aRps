\name{getarpsoundparam}
\alias{getarpsoundparam}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plots Arps soundings
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getarpsoundparam(col, row, tim, zoom, winds=FALSE)
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
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
    arpsexample=open.ncdf(system.file("allgaeu_d1_ARP.nc", package="aRps"))
    cparam=derive4dParam(arpsexample)
    getarpsoundparam (10,10,2, zoom=FALSE, winds=TRUE) # plots a sounding at position 10,10 at timestep2

  }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
