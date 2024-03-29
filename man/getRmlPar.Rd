% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rmacrolite.r
\name{getRmlPar}
\alias{getRmlPar}
\title{Get a single default parameters for the package.}
\usage{
getRmlPar(par)
}
\arguments{
\item{par}{See the \code{par} argument in \code{\link{rmlPar}}. Notice that if
more than one parameter name is provided, only the first one will be
returned.}
}
\value{
Return the value of the parameter \code{par}, without the list
 container of \code{\link{rmlPar}}.
}
\description{
Get a single default parameters for the package. Wrapper around
 \code{\link{rmlPar}}.
}
