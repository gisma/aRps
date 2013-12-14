\name{derive4dParam}
\alias{derive4dParam}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Calculate meteorological parameters as derived from the ARPS (Advanced Regional Prediction System) netCDF output file
}
\description{
  Calculates exner pressure (Pa,) air temperature in degree Celsius (C) , air temperature (K), dew point temperature (C),relative air humidity (percent), water vapor saturation pressure (hPa), water vapor pressure (hPa,  windspeed (m/s) and wind direction (deg) from arps output
provides additionally: potential temperature (K), pressure (hPa), zonal and meridional and vertical windspeed (m/s)
}
\details{
\itemize{
  \item exner pressure (Pa):  \eqn{e_p = (p / 100000) ^ (287.058 / 1005)}
  \itemize{ 
  \item \eqn{p = pressure (Pa)}
  \item \eqn{cp = 1005 = specific heat capacity (J/kg K)}
  \item \eqn{R =  287.058 0 specific gas constant (J/kg K)}
   }
  \item air temperature (C): \eqn{tc = e_p * pt -273.15}
  \itemize{ 
  \item \eqn{pt = potential temperature (K)}
  \item \eqn{e_p = exner pressure (Pa)}
  }
  \item air temperature (K):  \eqn{tk = e_p * pt} 
  \itemize{ 
  \item \eqn{pt = potential temperature (K)}
  \item \eqn{e_p = exner pressure (Pa)}
  }
  
 \item water vapor saturation pressure (hPa):   \eqn{es = (10 ^ (a0 - a1 + a2 - 2949.076 / tk))}
   \itemize{ 
  \item \eqn{v = log10(e/6.1078)}
   \itemize{ 
  \item \eqn{e = water vapor pressure (hPa)}
  }
  \item \eqn{a =   7.5 (if tc >= 0)}
  \item \eqn{b = 237.3 (if tc >= 0)}
    
  \item \eqn{a =   9.5 (if tc < 0)}
  \item \eqn{b = 285.5 (if tc < 0)}
}
     
  \item water vapor pressure (hPa):  \eqn{e = (p/0.622 * qv)/100} 
  \itemize{ 
  \item \eqn{p = pressure (Pa)}
  \item \eqn{qv = Water Vapor Mixing Ratio (g/kg)}
   }

  \item dew point temperature (C) \eqn{td = b*v/(a-v)}
  \itemize{ 
  \item \eqn{v = log10(e/6.1078)}
   \itemize{ 
  \item \eqn{e = water vapor pressure (hPa)}
  }
  \item \eqn{a =   7.6 (if tc >= 0)}
  \item \eqn{b = 240.7 (if tc >= 0)}
    
  \item \eqn{a =   9.5 (if tc < 0)}
  \item \eqn{b = 285.5 (if tc < 0)}
 }
  \item relative air humidity (percent): \eqn{ rh=(e/es)*100}
  \itemize{ 
  \item \eqn{e = water vapor pressure (hPa)}
  \item \eqn{es = water vapor saturation pressure (hPa)}
  }
 \item wind speed (m/s): \eqn{ ws=sqrt(u^2+v^2)}
  \itemize{ 
  \item \eqn{u = zonal velocity of horizontal wind  (m/s)}
  \item \eqn{v = meridional velocity of horizontal wind (m/s)}
  }  
 \item wind direction (m/s): \eqn{ wd=180+atan2(u,v)*57.295}
  \itemize{ 
  \item \eqn{u = zonal velocity of horizontal wind  (m/s)}
  \item \eqn{v = meridional velocity of horizontal wind (m/s)}
  \item the use of atan2 avoids the quadrant search for calculating the correct angle
  }    
}

For all with: pt = potential temperature (K), p = pressure (Pa),  qv = water vapor mixing ratio (g/kg) as derived from the model inputfile.
}
\usage{
  derive4dParam(nc,param=c("tc","td","tk","es","e","rh","pr","ep","u","v","w","ws","wd"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nc}{
    A netcdf object
  }
  \item{param}{
    A string vector of the parameters to be calculated
  }
}

\value{
  A list containing the calculated parameters as arrays
}

\author{
  Hanna Meyer and Chris Reudenbach
}
\seealso{
  For adaption of the projection and domain extent according to the identified reference system see \code{\link{extractExt}}. It is strongly recommended to use always first extractProj and then extractExt!!
}
\examples{
  #### Example to calculate derivated parameters from a nc file
    arpsexample=open.ncdf(system.file("allgaeu_d1_ARP.nc", package="aRps"))
    cparam=derive4dParam(arpsexample)
    str(cparam) # see which parameters are listed
    cparam$tc # to extract air dry bulb temperature (C)from the list
    getarpsoundparam (10,10,1,2) # plots a sounding at position 10,10 at timestep 2
}