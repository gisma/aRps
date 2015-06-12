#' @name calcMeteoParam
#' @aliases calcMeteoParam
#' @title calcMeteoParam calculates unstaggered meteorological parameters based on the ARPS-netCDF output file
#' @description
#'  calcMeteoParam converts, and derives meterorlogical parameters from the ARPS netCDF  modelrun result file.
#'    \itemize{
#'      \item pressure (hPa)
#'      \item exner pressure (hPa)
#'      \item air temperature in degree Celsius (C)
#'      \item dew point temperature (C)
#'      \item water vapor saturation pressure (hPa)
#'      \item water vapor pressure (hPa)
#'      \item relative air humidity (%)
#'      }
#' @details
#' The following equations are used:\cr
#'  \itemize{
#'           \item exner pressure (Pa):  \eqn{e_p = (p/100000)^(R/cp)}
#'           \tabular{lll}{
#'                    \tab \eqn{p} \tab \eqn{= pressure (Pa)}\cr
#'                    \tab \eqn{cp} \tab \eqn{= 1005 = specific heat capacity (J/kg K)}\cr
#'                    \tab \eqn{R}  \tab \eqn{=  287.058 0 specific gas constant (J/kg K)}\cr
#'          }
#'          \item air temperature (C): \eqn{tc = e_p*pt-273.15}
#'          \tabular{lll}{ 
#'                   \tab \eqn{pt} \tab \eqn{= potential temperature (K)}\cr
#'                   \tab \eqn{e_p} \tab \eqn{= exner pressure (Pa)}\cr
#'          }
#'          \item air temperature (K):  \eqn{tk = e_p*pt} 
#'          \tabular{lll}{ 
#'                   \tab \eqn{pt} \tab \eqn{= potential temperature (K)}\cr
#'                   \tab \eqn{e_p} \tab \eqn{= exner pressure (Pa)}\cr
#'          }
#'          \item water vapor saturation pressure (hPa): \eqn{es = 6.1078*10^((a*tc)/(b+tc))} 
#'          \tabular{lll}{ 
#'                   \tab \eqn{a}  \tab \eqn{=   7.5 (if tc >= 0)}\cr
#'                   \tab \eqn{b}  \tab \eqn{= 237.3 (if tc >= 0)}\cr
#'                   \tab \eqn{a}  \tab \eqn{=   9.5 (if tc < 0)}\cr
#'                   \tab \eqn{b}  \tab \eqn{= 285.5 (if tc < 0)}\cr
#'          }
#'         \item water vapor pressure (hPa):  \eqn{e = (p/0.622*qv)/100} 
#'         \tabular{lll}{ 
#'                  \tab \eqn{p}  \tab \eqn{ = pressure (Pa)}\cr
#'                  \tab \eqn{qv}  \tab \eqn{ = Water Vapor Mixing Ratio (g/kg)}\cr
#'         }
#'         \item dew point temperature (C): \eqn{td = b*v/(a-v)}
#'          \tabular{lll}{ 
#'                   \tab \eqn{v}  \tab \eqn{= log10(e/6.1078)}\cr
#'                   \tab \eqn{e}  \tab \eqn{= water vapor pressure (hPa)}\cr
#'                   \tab \eqn{a}  \tab \eqn{=   7.6 (if tc >= 0)}\cr
#'                   \tab \eqn{b}  \tab \eqn{= 240.7 (if tc >= 0)}\cr
#'                   \tab \eqn{a}  \tab \eqn{=   9.5 (if tc < 0)}\cr
#'                   \tab \eqn{b}  \tab \eqn{= 285.5 (if tc < 0)}\cr
#'        }
#'        \item relative air humidity (percent): \eqn{ rh=(e/es)*100}
#'        \tabular{lll}{ 
#'                 \tab \eqn{e} \tab \eqn{= water vapor pressure (hPa)}\cr
#'                 \tab \eqn{es} \tab \eqn{= water vapor saturation pressure (hPa)}\cr
#'        }
#'      \item For all operations is valid:
#'        \tabular{lll}{ 
#'                 \tab \eqn{pt} \tab \eqn{= potential temperature (K)}\cr
#'                 \tab \eqn{p} \tab {= air pressure (Pa)}\cr
#'                 \tab \eqn{qv} \tab   {= water vapor mixing ratio (g/kg)}\cr
#' #'      }}
#'@usage exner<-exnerpress(nc)

#'@param function name
#'@param nc netcdf object as opened by: nc <- nc_open(arps.ncfile)
#'@return  
#'The function exnerpress returns the Exner pressure (hPa).
#' The other functions will convert and return the requested data 
#' Be careful the data arrays may be VERY big. 
#'
#'@author  Chris Reudenbach, Hanna Meyer
#'@source 
#'\tabular{ll}{
#'Package: \tab aRps\cr
#'Type: \tab Package\cr
#'Version: \tab 0.3\cr
#'License: \tab GPL (>= 2)\cr
#'LazyLoad: \tab yes\cr
#'}
#'@seealso If you want to use this data georeferenced you need to extract the projection and domain extent according to the reference system that was used by ARPS \code{\link{getRefInfo}}
#'@export calcMeteoParam
#'@examples
#'  #### Examples how  to use the function in calcMeteoParam:
#'  ###  (1) provide a valid netcdf file
#'  ##   (2) open it
#'  #    (3) use it (i.e. air pressure)

#'  arps.ncfile=system.file("kili.nc", package="aRps")
#'  nc <- nc_open(arps.ncfile)
#'  pr<-airpressure(nc) 


exnerpress<- function(nc){
  # calculate Exner pressure (hPa)
  (ncvar_get ( nc, "P") / 100000.0) ^ (287.058 / 1005.0)
}

tcelsius=function(nc){
  # calculate air temperature Celsius using the exner pressure
  exnerpress(nc) * ncvar_get ( nc, "PT") -273.15
}  

satwatervapor<-function(nc){
  # calculate the saturation vapour pressure 
  # es in hPa, t in C
  # a,b params for water, water dewpoint and ice must be set
  tc=tcelsius(nc)
  #  T > 0 above water
  a <- replace(tc, tc >= 0, 7.5)
  b <- replace(tc, tc >= 0,  237.3)
  # T < 0 above ice Eis (freezing point)
  a <- replace(a, tc <0, 9.5)
  b <- replace(b, tc <0,  285.5)
  6.1078 * 10^((a*tc)/(b+tc))
}

partwatervapor<-function(nc){ 
  # Water vapor specific humidity qv (g/kg)
  # water vapour partial pressure e (Pa)
  # pressure p (Pa)
  # Ra specific gas constant air 287.06
  # Rw specifs constant water vapor  461.6
  # e = p/(Ra/Rw)*qv (Pa)
  # e = e/100 (hPa)
  (ncvar_get ( nc, "P")/0.622 * ncvar_get ( nc, "QV"))/100
}

dewpoint<-function(nc){
  #  Dew-point temprature (Td in C)  from vapor pressure (e) 
  e=partwatervapor(nc)
  tc=tcelsius(nc)
  # T> 0 above water
  a <- replace(tc, tc >= 0, 7.5)
  b <- replace(tc, tc >= 0,  237.3)
  # T < 0 above ice Eis (freezing point)
  a <- replace(a, tc <0, 9.5)
  b <- replace(b, tc <0,  285.5)
  #v = log10(e/6.1078)
  # Dewpoint Temperature C
  td = b*log10(e/6.1078)/a-(log10(e/6.1078))
  #alterntive calculation dewpoint C
  #td = (243.5 * log(e/6.112)) / (17.67 - log(e/6.112))
}
airpressure<-function(nc){
  # get and convert pressure from Pa to hPa
  ncvar_get ( nc, "P")/100
}
relhum<-function(nc){
  # calculate relative air humidity
  (partwatervapor(nc)/satwatervapor(nc))*100 
}
