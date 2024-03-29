% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rmacrolite.r
\docType{methods}
\name{rmacroliteDegradation}
\alias{rmacroliteDegradation}
\alias{rmacroliteDegradation.macroParFile}
\alias{rmacroliteDegradation<-}
\alias{rmacroliteDegradation<-.macroParFile}
\title{Fetch or set substance degradation paremeters in an imported MACRO par-file}
\usage{
rmacroliteDegradation(x, ...)

\method{rmacroliteDegradation}{macroParFile}(x, ...)

rmacroliteDegradation( x, dt50_depth_f = NULL, ... ) <- value

\method{rmacroliteDegradation}{macroParFile}(x, dt50_depth_f = NULL, ...) <- value
}
\arguments{
\item{x}{A \code{macroParFile}, as imported with 
\code{\link[rmacrolite]{rmacroliteImportParFile-methods}}}

\item{\dots}{Additional parameters passed to specific methods. 
Currently not used.}

\item{dt50_depth_f}{A vector of numeric-values with as many values as layers 
in the imported par-file \code{x}. When not \code{NULL}, 
the dt50 of each layer will be set as dt50 * \code{dt50_depth_f} 
corresponding to the layer, where \code{dt50} is the value 
of the substance half life as given in the parameter 
\code{value}. If \code{NULL} (the default), 
\code{dt50_depth_f} is calculated internally from the 
original degradation parameters in \code{x}. Please notice 
that when calculated internally, \code{dt50_depth_f} is 
rounded to 4-digits, to avoid problems of numerical 
accuracy.}

\item{value}{Vector of five numeric-value, optionally with named 
"dt50", "dt50_ref_temp", "dt50_pf", "exp_temp_resp" and 
"exp_moist_resp" (observe the lowercase). 
Value of the degradation parameters (see above) to be set 
in the par-file \code{x}.}
}
\value{
Returns a list with two items, \code{"layer"} 
 (layer-specific parameters) and \code{"site"} 
 (site-specific parameters, i.e. parameters that do 
 not vary with depth or with the crop). \code{"layer"} is 
 a \code{\link[base]{data.frame}} with the following 
 columns \code{layer_no}, \code{dt50} and 
 \code{dt50_depth_f},  (Layer number, DT50 [days] and the 
 depth factor of the DT50 (DT50 horizon/ DT50 first horizon), 
 respectively). \code{dt50_depth_f} is 
 not a MACRO input parameters and is here calculated from 
 the \code{dt50} of different horizons.
 \code{"site"} is a vector of named numeric values with 
 the follwing items \code{dt50_ref_temp}, 
 \code{dt50_pf}, \code{exp_temp_resp} and  
 \code{exp_moist_resp}  
 (reference temperature at which the DT50 was measured 
 [degrees Celcius], pF at which the DT50 was measured 
 [log10(cm)], the exponent of the temperature response, 
 the exponent of the moisture response)
}
\description{
Fetch or set substance degradation paremeters in an imported 
 MACRO par-file: DT50 [days], reference temperature at 
 which the DT50 was measured [degrees Celcius], pF at which 
 the DT50 was measured [log10(cm)], the exponent of the 
 temperature response and the exponent of the moisture 
 response (The DT50 is assigned to four parameters DEGMAL, 
 DEGMAS, DEGMIL and DEGMIS, and the other parameters are 
 TREF, PF1, TRESP and EXPB in MACRO par-files, respectively). 
 The function either fetches all these parameters or set 
 them all at once.
}
