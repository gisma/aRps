\name{skewt.axis.z}
\alias{skewt.axis.z}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
skewt.axis.z generates a zoom skewt plot with 400 hPa min pressure level
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
skewt.axis.z(BROWN = "brown", GREEN = "green", redo = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{BROWN}{
%%     ~~Describe \code{BROWN} here~~
}
  \item{GREEN}{
%%     ~~Describe \code{GREEN} here~~
}
  \item{redo}{
%%     ~~Describe \code{redo} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (BROWN = "brown", GREEN = "green", redo = TRUE, ...) 
{
    tmr <- function(w, p) {
        c1 <- 0.0498646455
        c2 <- 2.4082965
        c3 <- 7.07475
        c4 <- 38.9114
        c5 <- 0.0915
        c6 <- 1.2035
        x <- log10((w * p)/(622 + w))
        tmrk <- 10^(c1 * x + c2) - c3 + c4 * ((10^(c5 * x) - 
            c6)^2)
        tmrk - 273.15
    }
    tda <- function(o, p) {
        ok <- o + 273.15
        tdak <- ok * ((p * 0.001)^0.286)
        tdak - 273.15
    }
    par(pty = "s", ...)
    ymax <- skewty(1050)
    ymin <- skewty(400)
    xmin <- skewtx(-80, skewty(1050))
    xmax <- skewtx(50, skewty(1000))
    kinkx <- skewtx(5, skewty(400))
    xc <- c(xmin, xmin, xmax, xmax, kinkx, kinkx, xmin)
    yc <- c(ymin, ymax, ymax, skewty(625), skewty(400), ymin, 
        ymin)
    plot(xc, yc, type = "l", axes = FALSE, xlab = "", ylab = "", 
        lwd = 0.1)
    ypos <- skewty(1050)
    degc <- seq(-120, 50, by = 10)
    axis(1, at = skewtx(degc, ypos), labels = seq(-120, 50, by = 10), 
        pos = ymax)
    mtext(side = 1, line = 1, "Temperature (C)")
    pres <- c(1050, 1000, 950, 900, 850, 800, 750, 700, 600, 
        500, 400)
    NPRES <- length(pres)
    xpl <- rep(xmin, times = NPRES)
    xpr <- c(xmax, xmax, xmax, xmax, skewtx(10, skewty(500)), 
        kinkx, kinkx, kinkx, kinkx, kinkx, kinkx)
    y <- skewty(pres)
    segments(xpl, y, xpr, y, col = BROWN, lwd = 0.2, lty = 2)
    ypos <- skewty(pres[2:NPRES])
    axis(2, at = ypos, labels = pres[2:NPRES], pos = xmin)
    mtext(side = 2, line = 1.5, "P (hPa)")
    temp <- seq(from = -120, to = 50, by = 10)
    NTEMP <- length(temp)
    lendt <- rep(1050, NTEMP)
    inds <- seq(1, NTEMP)[(temp >= -130) & (temp <= -80)]
    exponent <- (132.182 - (xmin - 0.90692 * temp[inds])/0.90692)/44.061
    lendt[inds] <- 10^exponent
    rendt <- rep(400, NTEMP)
    inds <- seq(1, length(temp))[(temp >= 5) & (temp <= 10)]
    exponent <- (132.182 - (kinkx - 0.90692 * temp[inds])/0.90692)/44.061
    rendt[inds] <- 10^exponent
    rendt[temp == 10] <- 427
    rendt[temp == 20] <- 485
    rendt[temp == 30] <- 540
    rendt[temp == 40] <- 610
    rendt[temp == 50] <- 990
    yr <- skewty(rendt)
    xr <- skewtx(temp, yr)
    yl <- skewty(lendt)
    xl <- skewtx(temp, yl)
    segments(xl, yl, xr, yr, col = "red", lwd = 0.1)
    text(xr[11:NTEMP], yr[11:NTEMP], labels = paste(" ", as.character(temp[11:NTEMP])), 
        srt = 45, adj = 0, cex = 0.75, col = "black")
    text(xr[1:10], yr[1:10], labels = paste(" ", as.character(temp[1:10])), 
        srt = 0, adj = c(0.3, -1), cex = 0.75, col = "black")
    mixrat <- c(20, 12, 8, 5, 3, 2, 1)
    NMIX <- length(mixrat)
    yr <- skewty(440)
    tmix <- tmr(mixrat[1], 440)
    xr <- skewtx(tmix, yr)
    yl <- skewty(1000)
    tmix <- tmr(mixrat[1], 1000)
    xl <- skewtx(tmix, yl)
    segments(xl, yl, xr, yr, lty = 2, col = "gray", lwd = 1.5)
    yl <- skewty(1025)
    xl <- skewtx(tmix, yl)
    text(xl, yl, labels = as.character(mixrat[1]), col = "gray", 
        srt = 55, adj = 0.5, cex = 0.75)
    yr <- skewty(rep(400, NMIX - 1))
    tmix <- tmr(mixrat[2:NMIX], 400)
    xr <- skewtx(tmix, yr)
    yl <- skewty(rep(1000, NMIX - 1))
    tmix <- tmr(mixrat[2:NMIX], 1000)
    xl <- skewtx(tmix, yl)
    segments(xl, yl, xr, yr, lty = 2, col = "gray", lwd = 1.5)
    yl <- skewty(rep(1025, NMIX - 1))
    xl <- skewtx(tmix, yl)
    text(xl, yl, labels = as.character(mixrat[2:NMIX]), col = "darkgray", 
        srt = 55, adj = 0.5, cex = 0.75)
    theta <- seq(from = -80, to = 80, by = 10)
    NTHETA <- length(theta)
    lendth <- rep(400, times = NTHETA)
    lendth[1:4] <- c(980, 735, 565, 439)
    rendth <- rep(1050, times = NTHETA)
    rendth[14:NTHETA] <- c(1003, 869, 758, 678)
    for (itheta in 1:NTHETA) {
        p <- seq(from = lendth[itheta], to = rendth[itheta], 
            length = 200)
        sy <- skewty(p)
        dry <- tda(theta[itheta], p)
        sx <- skewtx(dry, sy)
        lines(sx, sy, lty = 1, col = "brown")
    }
    p <- seq(from = 1050, to = 410, by = -10)
    npts <- length(p)
    sy <- skewty(p)
    sx <- double(length = npts)
    if (redo) {
        pseudo <- c(32, 28, 24, 20, 16, 12, 8, 5, 1)
        NPSEUDO <- length(pseudo)
        holdx <- matrix(0, nrow = npts, ncol = NPSEUDO)
        holdy <- matrix(0, nrow = npts, ncol = NPSEUDO)
        for (ipseudo in 1:NPSEUDO) {
            for (ilen in 1:npts) {
                moist <- satlft(pseudo[ipseudo], p[ilen])
                sx[ilen] <- skewtx(moist, sy[ilen])
            }
            inds <- (sx < xmin)
            sx[inds] <- NA
            sy[inds] <- NA
            holdx[, ipseudo] <- sx
            holdy[, ipseudo] <- sy
        }
    }
    else {
        holdx <- skewt.data$pseudox
        holdy <- skewt.data$pseudoy
        pseudo <- skewt.data$pseudo
        NPSEUDO <- skewt.data$NPSEUDO
    }
    for (ipseudo in 1:NPSEUDO) {
        sx <- holdx[, ipseudo]
        sy <- holdy[, ipseudo]
        lines(sx, sy, lty = 3, col = "darkblue")
        moist <- satlft(pseudo[ipseudo], 430)
        labely <- skewty(430)
        labelx <- skewtx(moist, labely)
        if (labelx > xmin) 
            text(labelx, labely, labels = as.character(pseudo[ipseudo]), 
                col = "darkblue", adj = 0.5, cex = 0.75)
        moist <- satlft(pseudo[ipseudo], 1100)
        labely <- skewty(1100)
        labelx <- skewtx(moist, labely)
        text(labelx, labely, labels = as.character(pseudo[ipseudo]), 
            col = "darkblue", adj = 0.5, cex = 0.75)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
