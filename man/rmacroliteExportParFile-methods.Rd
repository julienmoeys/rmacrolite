% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rmacrolite.r
\docType{methods}
\name{rmacroliteExportParFile}
\alias{rmacroliteExportParFile}
\alias{rmacroliteExportParFile.macroParFile}
\title{Export parameters for one or several MACRO simulations}
\usage{
rmacroliteExportParFile(x, f = NULL, verbose = 1L, ...)

\method{rmacroliteExportParFile}{macroParFile}(x, f = NULL,
  verbose = 1L, ...)
}
\arguments{
\item{x}{A \code{macroParFile} object, containing one simulations 
to be exported}

\item{f}{Single character string. Name of, and optionally path to, 
the par-file where the simulations par-file should be 
written. If \code{NULL}, a name will be attributed using 
the template given by 
\code{\link[rmacrolite]{getRmlPar}("fileNameTemplate")} 
and the RUNID contained in the par-file.}

\item{verbose}{Single integer value. If set to a value \code{< 1}, 
the program is silent (except on errors or warnings). If 
set to \code{1}, the program outputs messages. Values 
\code{> 1} may also activate messages from lower level 
functions (for debugging purpose).}

\item{\dots}{Additional parameters passed to specific methods.}
}
\value{
WRITE DESCRIPTION HERE.
}
\description{
Export parameters for one or several MACRO simulations
}
\examples{

library( "rmacrolite" )

#   Path to an example par-file
par_file_path <- system.file( "par-files", 
    "chat_winCer_GW-X_900gHa_d182.par", 
    package = "rmacrolite" )  

#   Import the example par-file
par_file <- rmacroliteImportParFile( 
    file = par_file_path ) 

#   Path where the file will be exported (temporary directory)
export_path <- tempfile( fileext = ".par" )

#   Export the par-file elsewhere
rmacroliteExportParFile( x = par_file, f = export_path )

#   Internal control
md5_files <- as.character( tools::md5sum( files = c( 
    par_file_path, export_path ) ) ) 

md5_files 

#   
if( md5_files[ 1L ] != md5_files[ 2L ] ){ 
    stop( "Test of rmacroliteExportParFile() failed" ) } 


#   Clean-up
rm( par_file_path, par_file, export_path, md5_files )
}
\seealso{
\code{\link[rmacrolite:rmacroliteExport-methods]{rmacroliteExport}}.
}
