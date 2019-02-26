
# +--------------------------------------------------------+ 
# | Package:    See 'Package' in file ../DESCRIPTION       | 
# | Author:     Julien MOEYS                               | 
# | Language:   R                                          | 
# | Contact:    See 'Maintainer' in file ../DESCRIPTION    | 
# | License:    See 'License' in file ../DESCRIPTION       | 
# |             and file ../LICENSE                        | 
# +--------------------------------------------------------+ 



# +--------------------------------------------------------+ 
# | Original file: rmacro-options.r                        | 
# +--------------------------------------------------------+ 

#'Windows System Environment Variables used by rmacrolite
#'
#'It is possible, but not compulsory, to define 
#'  Windows System Environment Variables, 
#'  \code{rmacrolite_macro_path}, \code{rmacrolite_macro_exe}, 
#'  and/ or \code{rmacrolite_macro_exeparfile}, with the default 
#'  value to be used by the package \code{rmacrolite} for 
#'  the path to the folder where MACRO or MACRO In FOCUS is 
#'  installed, the name of MACRO or MACRO In FOCUS executable 
#'  and the name of the so-called exeparfile executable, 
#'  respectively. The path to MACRO executable is the most 
#'  important, as it can vary between computers.
#'
#'  When these Windows System Environment Variables have been 
#'  set, it is not necessary to use the function 
#'  \code{\link[rmacrolite:rmacroliteSetModelVar-methods]{rmacroliteSetModelVar}} in your code, 
#'  as the package will automatically find the relevant values 
#'  that need to be used.
#'
#'  These variables may be especially useful when trying to 
#'  build the package from source and run \code{R CMD check}.
#'
#'  If you want to check if these System variable have been 
#'  set and what is their current value, you can type:
#'  \code{Sys.getenv("rmacrolite_macro_path")} or 
#'  \code{Sys.getenv("rmacrolite_macro_exe")} or 
#'  \code{Sys.getenv("rmacrolite_macro_exeparfile")}.
#'  A value of \code{""} indicates that the system variables 
#'  do not exist.
#'
#'  You (most likely) need administrator rights on your 
#'  computer to set these variables. Or you need your System 
#'  admin to set them. In Windows 10, the variables can be 
#'  set as: Windows Start menu > Settings > System > About > 
#'  Related settings - system info > Advanced System Settings 
#'  > Advanced tab > "Environment variables..." > System variables.
#'  Alternatively type 
#'  \code{shell.exec("SystemPropertiesAdvanced.exe")} in 
#'  R command prompt, to reach the Advanced System Settings.
#'  If these variables don't exist, they need to be created.
#'
#'@seealso \code{\link[rmacrolite:rmacroliteSetModelVar-methods]{rmacroliteSetModelVar}} and 
#'  \code{\link[rmacrolite]{rmacroliteGetModelVar}}.
#'
#'@example inst/examples/rmacrolite-system-variables-example.r
#'
#'@name rmacrolite-system-variables
NULL 



# +--------------------------------------------------------+
# Create two environments that will contain the package's
# parameters.

# - Backup / reference 
.rmlParList <- new.env() 

# - User visible container
rmlParList  <- new.env() 



# Set some default parameters: 

.rmlParList[[ "encoding" ]] <- c( "UTF-8-BOM", "UTF-8-BOM", "cp1252" )  
.rmlParList[[ "maxPathLength" ]] <- 65L 
.rmlParList[[ "fileNameTemplate" ]] <- list( "r" = "rml_%s.%s", 
    "macro" = "MACRO%s.%s" ) 
.rmlParList[[ "idWidth" ]] <- 3L
# .rmlParList[[ "addParToSimRes" ]] <- FALSE  
# .rmlParList[[ "verbose" ]]           <- 2L 
# .rmlParList[[ "macroVersion" ]] <- "5.5.4" 
# .rmlParList[[ "macro.exe" ]] <- list( 
    # "5.5.4" = "Macro52Model.exe"  ) 
# .rmlParList[[ "macro.exeparfile" ]] <- list( 
    # "5.5.4" = "macro.exeparfile"  ) 
# # .rmlParList[[ "delete" ]]            <- TRUE 
# .rmlParList[[ "timeFormat" ]] <- "%Y-%m-%d %H:%M" 
.rmlParList[[ "tz" ]]  <- "GMT"  
.rmlParList[[ "errorKeywords" ]] <- c( "error", "invalid", 
    "unhandled", "exception", "cannot access", "overflows", 
    "severe" ) 
.rmlParList[[ "handleErrors" ]] <- FALSE 
.rmlParList[[ "balanceFile" ]]  <- "balance.txt" 
.rmlParList[[ "macro_path" ]] <- NULL
.rmlParList[[ "macro_exe" ]] <- NULL
.rmlParList[[ "macro_exeparfile" ]] <- NULL
.rmlParList[[ "macro_path_default" ]] <- "C:\\swash\\macro"
.rmlParList[[ "macro_exe_default" ]] <- "Macro52Model.exe"
.rmlParList[[ "macro_exeparfile_default" ]] <- "exeparfile.exe"
# .rmlParList[[ "digits_parfile_k" ]] <- c( "parent" = 8L, 
    # "metabolite" = 9L ) 
.rmlParList[[ "digits_parfile_k" ]] <- 7L 
.rmlParList[[ "digits_dt50_depth_f" ]] <- 4L 
.rmlParList[[ "id_range" ]] <- c( 1L, 999L ) 
# .rmlParList[[ "exeparfilePath" ]]    <- character(0)
.rmlParList[[ "log_width" ]] <- 80L # 80 char is ~1 row with font size 10 in courier new



# +--------------------------------------------------------+
# Define the function that handles the package default parameters: 

#'Get or set default parameters for the package.
#'
#'Get or set default parameters for the package. Notice changes done to the
#'parameter values are reset everytime the R session is closed and the package
#'is reloaded.
#'
#'
#'@details 
#'  The function has 3 possible, non-exclusive behaviours: \itemize{ \item If
#'  \code{reset=TRUE}, resetting the parameters to their initial values, as
#'  defined in this function. \item (Silently) returning the actual value of the
#'  package parameters. If \code{par=NULL}, all the values are returned.  If
#'  \code{par} is a vector of parameter names, their value will be returned.
#'  \item Setting-up the value of some parameters, passing a list of parameter
#'  value to \code{par} OR setting some of the parameters listed above. }
#'
#'  Notice that when \code{reset=TRUE} and some new parameter values are
#'  provided, the parameters are first reset, and then the new parameter values
#'  are set. If \code{par} is a list, parameters are set first according to
#'  values in \code{par}, and then according to values in the parameters listed
#'  below. This combination is not recommended, but nonetheless possible.
#'
#'  The actual value of the parameters is stored in (and can be retrieved from)
#'  the environment \code{rspPars}. The default value of the parameters are
#'  stored in the environment \code{rspPars}. Do not use them directly.
#'
#'
#'@param par  
#'  Three possible cases: \itemize{ \item If \code{par} is \code{NULL}
#'  (default): All the actual value of the parameters will be silently returned.
#'  \item If \code{par} is a vector of character strings representing parameter
#'  names. The value of the parameters named here will be (silently) returned.
#'  \item If \code{par} is a list following the format \code{tag = value}, where
#'  \code{tag} is the name of the parameter to be changed, and \code{value} is
#'  its new value.  Such a list is returned by \code{rmlPar()}. Notice that
#'  parameters can also be set individually, using the options listed below. }
#'
#'@param reset 
#'  Single logical. If TRUE, all the parameters will be set to their
#'  default value. Values are reset before any change to the parameter values, as
#'  listed below.
#'
#'@param encoding 
#'  Vector of three character strings. (1) \code{encoding} of the 
#'  MACRO par file (when imported), passed to \code{\link{readLines}}, 
#'  (2) \code{encoding} of the MACRO par file (when exported), 
#'  passed to \code{\link{writeLines}} (via \code{\link{file}}), 
#'  and (3) \code{encoding} of the MACRO crop parameter file 
#'  (when imported).
#'
#'@param maxPathLength 
#'  Single integer value. Maximum length of a path for the
#'  MACRO command line modules:
#'
#'@param fileNameTemplate 
#'  List of character strings, with two items, \code{"r"} and 
#'  \code{"macro"}. \code{"macro"} is a single character string, 
#'  the name and extension (but without path) of the default 
#'  MACRO parameter files that is exported and MACRO output 
#'  file to be generated. Should include the wilcard \code{\%s}, 
#'  that will internally be replaced by the simulation ID 
#'  (RUNID), and a second willcard \code{\%s} instead of the 
#'  extension. For example \code{rml\%s.\%s} .
#'  \code{"r"} is the same thing, except that it the the template 
#'  for how the file should be renamed after it has been 
#'  output by MACRO, or for how the package should name the 
#'  files it generates.
#'  
#'
#'@param idWidth
#'  Single integer value. Width of the formatted simulation 
#'  ID in MACRO In FOCUS output files (bin-files). Also used 
#'  for formatting the par-files generated by this package.
#'
#'@param tz
#'  See \code{\link[base:as.POSIXlt]{as.POSIXct}}. Time zone.
#'
#'@param errorKeywords
#'  Vector of character strings. Keywords that should be interpreted 
#'  as an error or problem in MACRO command message output.
#'
#'@param handleErrors
#'  Single character string. If set to \code{TRUE}, \code{rmacro} 
#'  tries to handle error when running a list of MACRO simulations,
#'  in order to avoid crashing the whole list.
#'
#'@param balanceFile
#'  Single of character string. Name of the file containing output 
#'  water and solute balance, as internally calculated by MACRO.
#'
#'@param macro_path 
#'  Single character string. Path to the folder where the MACRO 
#'  or MACRO In FOCUS executable is installed. See 
#'  \code{\link[rmacrolite:rmacroliteSetModelVar-methods]{rmacroliteSetModelVar}} and 
#'  \code{\link[rmacrolite]{rmacrolite-system-variables}}.
#'  If equal to \code{NULL} (the default), the value defined 
#'  in the Windows Environment Variable called 
#'  \code{rmacrolite_macro_path} will be used in the 1st 
#'  place, when it exists, or alternatively the value in 
#'  \code{macro_path_default} (see below)
#'
#'@param macro_exe 
#'  Single character string. Name of the the MACRO 
#'  or MACRO In FOCUS executable to be used to run simulations, 
#'  in the folder defined by \code{macro_path}. 
#'  See \code{\link[rmacrolite:rmacroliteSetModelVar-methods]{rmacroliteSetModelVar}} and 
#'  \code{\link[rmacrolite]{rmacrolite-system-variables}}.
#'  If equal to \code{NULL} (the default), the value defined 
#'  in the Windows Environment Variable called 
#'  \code{rmacrolite_macro_exe} will be used in the 1st 
#'  place, when it exists, or alternatively the value in 
#'  \code{macro_exe_default} (see below)
#'
#'@param macro_exeparfile
#'  Single character string. Name of the the "exeparfile" 
#'  executable that converts par-files into MACRO input files 
#'  (indump.tmp), in the folder defined by \code{macro_path}. 
#'  Notice that the exeparfile is not provided with MACRO 
#'  5.2, while it is provided with MACRO In FOCUS. It can thus 
#'  be copied from the MACRO In FOCUS installation directory 
#'  to the MACRO installation directory.
#'  See \code{\link[rmacrolite:rmacroliteSetModelVar-methods]{rmacroliteSetModelVar}} and 
#'  \code{\link[rmacrolite]{rmacrolite-system-variables}}.
#'  If equal to \code{NULL} (the default), the value defined 
#'  in the Windows Environment Variable called 
#'  \code{rmacrolite_macro_exeparfile} will be used in 
#'  the 1st place, when it exists, or alternatively the value in 
#'  \code{macro_exeparfile_default} (see below)
#'
#'@param macro_path_default
#'  Single character string. Default value for \code{macro_path} 
#'  (see above).
#'
#'@param macro_exe_default
#'  Single character string. Default value for \code{macro_exe} 
#'  (see above).
#'
#'@param macro_exeparfile_default
#'  Single character string. Default value for \code{macro_exeparfile} 
#'  (see above).
#'
#'@param digits_parfile_k
#'  Single integer values. Number of significant digits to be 
#'  used when rounding degradation rates (DEMAL, DEGMAS, 
#'  DEGMIL, DEGMIS) in the par-file when modifying degradation 
#'  parameters.
#'
#'@param digits_dt50_depth_f
#'  Single integer value. Number of digits to be used when 
#'  rounding the factor describing DT50 decrease with depth 
#'  when calculating the values from the par-file or writing 
#'  new degradation values in a par-file.
#'
#'@param id_range
#'  Vector of 2 integer values. Min and max value allowed for 
#'  MACRO RUNID.
#'
#'@param log_width
#'  Single integer value. Width of the log output messages 
#'  (maximum number of characters). Notice that some messages 
#'  may get larger.
#'
#'
#'@seealso \code{\link{getRmlPar}}.
#'
#'@export rmlPar
#'
rmlPar <- function( 
    par = NULL, 
    reset = FALSE, 
    encoding, 
    maxPathLength, 
    fileNameTemplate, 
    idWidth, 
    # addParToSimRes, 
    # verbose, 
    # macroVersion, 
    # macro.exe, 
    # timeFormat, 
    tz, 
    errorKeywords, 
    handleErrors, 
    balanceFile,  
    macro_path, 
    macro_exe, 
    macro_exeparfile, 
    macro_path_default, 
    macro_exe_default, 
    macro_exeparfile_default, 
    digits_parfile_k, 
    digits_dt50_depth_f, 
    id_range, 
    log_width 
){  
    parList <- names( formals(rmlPar) ) 
    parList <- parList[ !(parList %in% c( "par", "reset" )) ] 
    
    
    ## (1) Reset the parameter values:
    if( reset ){ 
        v  <- as.list( .rmlParList ) 
        nv <- names( v ) 
        
        lapply( 
            X   = 1:length(v), 
            FUN = function(X){ 
                assign( x = nv[ X ], value = v[[ X ]], envir = rmlParList ) 
            }   
        )   
        
        rm( nv, v ) 
    }   
    
    
    ## (2) Change the parameter values:
    
    # Get actual parameter values:
    rmlParValues <- as.list( get( x = "rmlParList" ) ) 
    
    # Case: par is a list of parameters to be set
    if( is.list( par ) ){
        parNames <- names( par ) 
         
        if( is.null( parNames ) ){ 
            stop( "If 'par' is a list, its item must be named." )
        }   
        
        # Check that all parameters in par exists:
        testpar1 <- !(parNames %in% names(rmlParValues)) 
        
        if( any( testpar1 ) ){ 
            stop( sprintf( 
                "Some of the parameter names listed in 'par' could not be found: %s.", 
                paste( parNames[ testpar1 ], collapse=", " ) 
            ) ) 
        }  
        
        # Set the values
        for( i in parNames ){ 
            rmlParValues[[ i ]] <- par[[ i ]] 
        }   
    }   
    
    # Set all the individual parameters provided as a function's 
    # argument(s)
    for( parLabel in parList ){ 
        testExpr <- substitute( 
            expr = !missing(theLabel), 
            env  = list( theLabel = as.symbol(parLabel) ) 
        )   
        
        if( eval( testExpr ) ){ 
            rmlParValues[[ parLabel ]] <- get( x = parLabel )  
        }   
    }   
    
    # Set the parameter values at once 
    nv <- names( rmlParValues ) 
    lapply( 
        X   = 1:length(rmlParValues), 
        FUN = function(X){ 
            assign( x = nv[ X ], value = rmlParValues[[ X ]], envir = rmlParList ) 
        }   
    )   
    
    
    ## (3) Return the parameter values:
    
    # Case: return the value of some parameters:
    if( is.character(par) & (length(par) != 0) ){ 
        # Test that all demanded parameters exists:    
        testpar <- !(par %in% names(rmlParValues)) 
        
        if( any( testpar ) ){ 
            stop( sprintf( 
                "Some of the parameter names listed in 'par' could not be found: %s.", 
                paste( par[ testpar ], collapse=", " ) 
            ) ) 
        }  
        
        ret <- rmlParValues[ par ] 
    
    # Case: return the value of all parameters:
    }else{ 
        ret <- rmlParValues 
    }   
    
    return( invisible( ret ) ) 
### Returns a partial or complete list of (actual) parameter values, 
### as a named list.
}   





#'Get a single default parameters for the package.
#'
#'Get a single default parameters for the package. Wrapper around
#'  \code{\link{rmlPar}}.
#'
#'
#'@param par 
#'  See the \code{par} argument in \code{\link{rmlPar}}. Notice that if
#'  more than one parameter name is provided, only the first one will be
#'  returned.
#'
#'
#'@return 
#'  Return the value of the parameter \code{par}, without the list
#'  container of \code{\link{rmlPar}}.
#'
#'@export getRmlPar
#'
getRmlPar <- function(
    par 
){  
    return( rmlPar( par = par )[[ 1L ]] ) 
}   




# +--------------------------------------------------------+
# Test that all parameters in '.rmlParList' have been included in 
# the function rspParameters() 

# List of parameter names:
parNames <- names( as.list( .rmlParList ) ) 

# List of argument names
rmlParF <- names(formals(rmlPar))
rmlParF <- rmlParF[ !(rmlParF %in% c("par","reset")) ]

# List of parameters handled by rmlPar(): do they match with 
# the default parameters?
testpar  <- !(parNames %in% rmlParF)

if( any(testpar) ){ 
    stop( sprintf( 
        "Some parameters in '.rmlParList' are not in names(formals(rmlPar)): %s", 
        paste( parNames[ testpar ], collapse = ", " ) 
    ) )  
}   

# Other way round
testpar2 <- !(rmlParF %in% parNames)

if( any(testpar2) ){ 
    stop( sprintf( 
        "Some parameters in names(formals(rmlPar)) are not in '.rmlParList': %s", 
        paste( rmlParF[ testpar2 ], collapse = ", " ) 
    ) )  
}   

rm( testpar, parNames, testpar2, rmlParF ) 



# Set the current list of parameters
rmlParList <- list2env( as.list( .rmlParList ) ) 



# +--------------------------------------------------------+ 
# | Original file: onAttach.r                              | 
# +--------------------------------------------------------+ 

#'@importFrom utils packageVersion
NULL

.onAttach <- function(# Internal. Message displayed when loading the package.
    libname, 
    pkgname  
){  
    # .rml_testDateFormat()
    
    # .rml_testDecimalSymbol() 
    
    # Welcome message
    if( interactive() ){ 
        gitRevision <- system.file( "GIT_REVISION", package = pkgname ) 
        
        if( gitRevision != "" ){ 
            gitRevision <- readLines( con = gitRevision )[ 1L ] 
            gitRevision <- strsplit( x = gitRevision, split = " ", 
                fixed = TRUE )[[ 1L ]][ 1L ]
            gitRevision <- sprintf( "(git revision: %s)", gitRevision ) 
        }else{ 
            gitRevision <- "(git revision: ?)" 
        }   
        
        msg <- sprintf( 
            "%s %s %s. For help type: help(pack='%s')", 
            pkgname, 
            as.character( utils::packageVersion( pkgname ) ), 
            gitRevision, 
            pkgname ) 
        
        packageStartupMessage( msg ) 
    }   
}   



# +--------------------------------------------------------+ 
# | Original file: rmacro.R                                | 
# +--------------------------------------------------------+ 

# .rml_logMessage ==========================================

.rml_justify_text <- function( 
    txt, 
    log_width = 60L, 
    indent = "    " 
){  
    txt <- strsplit( x = txt, split = " " )[[ 1L ]]
    txt[ txt == "" ] <- " "
    txt_nchar <- nchar( txt ) 
    
    output <- vector( length = length(txt), mode = "list" )
    
    current_row <- 1L
    
    for( i in 1:length(txt) ){
        if( is.null( output[[ current_row ]] ) ){
            output[[ current_row ]] <- txt[ i ]
        }else{
            tmp <- paste( output[[ current_row ]], txt[ i ], 
                sep = ifelse( test = txt[ i ] == " ", 
                yes = "", no = " " ) )
            
            if( nchar( tmp ) < log_width ){
                output[[ current_row ]] <- tmp 
            }else{
                current_row <- current_row + 1L 
                
                output[[ current_row ]] <- paste( 
                    indent, txt[ i ], sep = "" )
            }   
        }   
    }   
    
    row_not_null <- !unlist( lapply( X = output, 
        FUN = is.null ) )
    
    output <- output[ row_not_null ]
    
    output <- paste( unlist( output ), collapse = "\n" )
    
    return( output )
}

    # test_txt <- c(
        # "<1905-01-01_23:13:59> alpha beta gamma delta epsilon zeta eta theta.", 
        # "theta eta zeta epsilon delta gamma beta alpha.", 
        # "alpha beta gamma delta epsilon zeta eta theta.", 
        # "theta eta zeta epsilon delta gamma beta alpha.", 
        # "alpha beta gamma delta epsilon zeta eta theta." )
    # test_txt <- paste( test_txt, collapse = " " ) 
    # message( .rml_justify_text( txt = test_txt, log_width = 30L ) ) 

.text_to_files <- function(text,logfiles,append){
    n_logfiles <- length( logfiles )
    
    if( n_logfiles != 0 ){
        if( (length( append ) == 1L) & (n_logfiles > 1L) ){
            append <- rep( append, times = n_logfiles )
        }   
        
        silence <- lapply( 
            X   = 1:n_logfiles, 
            FUN = function(i){
            con <- file( 
                description = logfiles[ i ], 
                open        = ifelse( 
                    test = append[ i ], 
                    yes  = "at", 
                    no   = "wt" ), 
                encoding    = "UTF-8" )
            
            on.exit( close( con ) )
            
            writeLines( text = text, con = con, 
                sep = "\n" )
        } )
    }   
}   

#'@importFrom utils flush.console
NULL

## # Send one or several information message(s) about work progresses
## # 
## # Send one or several information message(s) about work progresses. 
## #   Wrapper around \code{message(sprintf())}, with an additional 
## #   information about message time and date.
## # 
## # 
## # @param m
## #   See \code{fmt} in \code{\link[base]{sprintf}}. Message 
## #   to be displayed whenever \code{verbose} is >= 1.
## # 
## # @param verbose
## #   See \code{verbose} in \code{\link[rrmacrolite]{rmlPar}}.
## # 
## # @param \dots
## #   See \code{\link[base]{sprintf}}.
## # 
## # @return 
## #   Does not return anything. Output messages
## # 
## # 
## # @rdname .rml_logMessage
## # 
.rml_logMessage <- function( 
    m, 
    # fmt2 = NULL, 
    verbose = 1L, 
    fun = message, 
    # infix = "", 
    frame = NULL, 
    log_width = getRmlPar("log_width"), 
    values = NULL, # a list
    logfiles = NULL, 
    append = rep(FALSE,length(logfiles))
){  
    if( verbose >= 1L ){ 
        
        frame_not_null <- !is.null(frame)
        
        if( frame_not_null ){
            frame0 <- rep( x = substr( frame, 1L, 1L ), 
                times = log_width ) 
            
            frame0 <- paste( frame0, collapse = "" )
            
            .text_to_files( text = frame0, logfiles = logfiles, 
                append = append )
            
            fun( frame0 )
        }   
        
        if( !is.null( values ) ){
            m <- do.call( what = "sprintf", 
                args = c( list( "fmt" = paste( "<%s>", m, sep = " " ), 
                format( Sys.time(), "%Y-%m-%d|%H:%M:%S" ) ), 
                values ) )
            
        }else{
            m <- sprintf( paste( "<%s>", m, sep = " " ), format( 
                Sys.time(), "%Y-%m-%d|%H:%M:%S" ) )
        }   
        
        # fun( sprintf( paste( "<%s>", m ), Sys.time(), ... ) ) 
        m <- .rml_justify_text( txt = m, log_width = log_width )
        
        .text_to_files( text = m, logfiles = logfiles, 
            append = append )
        
        fun( m ) 
        
        if( frame_not_null ){
            .text_to_files( text = frame0, logfiles = logfiles, 
                append = append )
            
            fun( frame0 )
        }   
        
        utils::flush.console() 
    }   
}   

    # .rml_logMessage( m = "Hello" )
    # .rml_logMessage( m = "Hello %s", values = list( "you" ) )
    # .rml_logMessage( m = "Hello %s", values = list( "you" ), infix = "    " )
    # .rml_logMessage( m = "Hello %s", values = list( "you" ), frame = "*+" )



# rmacroliteSetModelVar ========================================

#'Set the absolute path of the folder in which MACRO (or MACRO In FOCUS) executable is installed.
#'
#'Set the absolute path of the folder in which MACRO (or 
#'  MACRO In FOCUS) executable is installed. as well as 
#'  the name of MACRO executable and the name of the exeparfile 
#'  executable.
#'  
#'  Regarding the path to MACRO-folder, the function 
#'  proceeds as follow: If a value is given (argument 
#'  code{path}, see below), it is used to set MACRO-path in 
#'  the package-option \code{macro_path} (see 
#'  \code{\link[rmacrolite]{rmlPar}}), value that is then used 
#'  by other function in this package. If no value is given 
#'  (argument code{path} left to \code{NULL}), the function 
#'  will first search for a Windows Environment Variable 
#'  (System Variable) called \code{rmacrolite_macro_path}, 
#'  and use this value if it exists, to set \code{macro_path} 
#'  in \code{\link[rmacrolite]{rmlPar}}. See 
#'  \code{\link[rmacrolite]{rmacrolite-system-variables}}. 
#'  Finally, if neither the argument 
#'  code{path} is set, nor a System Variable called 
#'  \code{rmacrolite_macro_path}, the function will try the 
#'  factory-default path to MACRO In FOCUS (type 
#'  \code{getRmlPar("macro_path_default")} to find out).
#'  
#'  The principle is the same for MACRO executable (argument 
#'  \code{exe}, System Variable \code{rmacrolite_macro_exe}, factory 
#'  default set in \code{getRmlPar("macro_exe_default")}) 
#'  as well as for the exeparfile executable (argument 
#'  \code{exeparfile}, System Variable 
#'  \code{rmacrolite_macro_exeparfile}, factory default set in 
#'  \code{getRmlPar("macro_exeparfile_default")}).
#'
#'
#'@param path
#'  Single character string. Absolute path to the folder 
#'  (directory) in which MACRO or MACRO In FOCUS executable 
#'  can be found. When both programs are installed on your 
#'  computer, chose which of the two you want \code{rmacrolite} 
#'  to use. Do not include the name of the executable, 
#'  only its folder. See the introduction above.
#'
#'@param exe
#'  Single character string. Name, without path, but with the 
#'  extension, of the MACRO or MACRO In FOCUS executable to 
#'  be used by \code{rmacrolite}. The executable must be 
#'  present in the folder \code{path}.  See the introduction 
#'  above.
#'
#'@param exeparfile
#'  Single character string. Name, without path, but with the 
#'  extension, of the exeparfile executable to 
#'  be used by \code{rmacrolite}. The executable must be 
#'  present in the folder \code{path}. As it is currently 
#'  not shipped with MACRO, the user must install the executable 
#'  beforehand. It can be copy-pasted from a MACRO In FOCUS 
#'  installation. See the introduction above.
#'
#'@param \dots
#'  Additional parameters passed to specific methods.
#'
#'
#'@return 
#'  Invisibly returns a \code{\link[base]{list}} with 3 named 
#'  items: \code{path}, \code{exe} and \code{exeparfile}, 
#'  set to the values found out by the function.
#'
#'
#'@seealso \code{\link[rmacrolite]{rmacroliteGetModelVar}} and 
#'  \code{\link[rmacrolite]{rmacrolite-system-variables}}.
#'
#'
#'@example inst/examples/rmacroliteSetModelVar-example.r
#'
#'@rdname rmacroliteSetModelVar-methods
#'@aliases rmacroliteSetModelVar
#'
#'@export 
#'
#'@docType methods
#'
rmacroliteSetModelVar <- function( 
    path = NULL, 
    exe = NULL, 
    exeparfile = NULL, 
    ... 
){ 
    UseMethod( "rmacroliteSetModelVar" )
}   


#'@rdname rmacroliteSetModelVar-methods
#'
#'@method rmacroliteSetModelVar default
#'
#'@export 
rmacroliteSetModelVar.default <- function( 
    path = NULL, 
    exe = NULL, 
    exeparfile = NULL, 
    ... 
){  
    #   Set the path to MACRO executable:
    if( !is.null( path ) ){
        path <- path[1L]
        
        #   Test that the folder exists
        if( !file.exists( path ) ){
            stop( sprintf( 
                "The folder %s could not be found (defined from argument 'path')", 
                path 
            ) )  
        }   
        
    }else{
        path <- Sys.getenv( x = "rmacrolite_macro_path", 
            unset = NA_character_ )
        
        if( !is.na( path ) ){
            #   Test that the folder exists
            if( !file.exists( path ) ){
                stop( sprintf( 
                    "The folder %s could not be found (defined from: Sys.getenv('rmacrolite_macro_path'))", 
                    path 
                ) )  
            }   
            
        }else{
            path <- getRmlPar( "macro_path_default" )
            
            if( !file.exists( path ) ){
                stop( sprintf( 
                    "The folder %s could not be found (defined from: getRmlPar('macro_path_default'))", 
                    path 
                ) )  
            }   

        }   
    }   
    
    #   Set the parameter
    rmlPar( "macro_path" = path ) 
    
    
    
    #   Set the name of MACRO and the exeparfile executables
    for( v in c( "exe", "exeparfile" ) ){
        v_value <- get0( x = v )
        
        if( !is.null( v_value ) ){
            v_value <- v_value[1L]
            
            #   Test that the folder exists
            if( !file.exists( file.path( path,v_value ) ) ){
                stop( sprintf( 
                    "The file %s folder could not be found in %s (defined from argument '%s')", 
                    v_value, path, v 
                ) )  
            }   
            
        }else{
            v_value <- Sys.getenv( sprintf( "rmacrolite_%s", v ), 
                unset = NA_character_ )
            
            if( !is.na( v_value ) ){
                #   Test that the folder exists
                if( !file.exists( file.path( path,v_value ) ) ){
                    stop( sprintf( 
                        "The file %s folder could not be found in %s (defined from: Sys.getenv('rmacrolite_macro_%s'))", 
                        v_value, path, v 
                    ) ) 
                }   
            
            }else{
                path <- getRmlPar( sprintf( "macro_%s_default", v ) )
                
                if( !file.exists( path ) ){
                    stop( sprintf( 
                        "The file %s folder could not be found in %s (defined from: getRmlPar('macro_%s_default'))", 
                        v_value, path, v 
                    ) )  
                }   

            }   
        }   
        
        assign( x = v, value = v_value )
    }       
    
    #   Set the parameters
    rmlPar( "macro_exe" = exe ) 
    rmlPar( "macro_exeparfile" = exeparfile ) 
    
    
    
    out <- list( "path" = path, "exe" = exe, "exeparfile" = exeparfile )
    return( invisible( out ) )
}   



# rmacroliteGetModelVar ======================================

#'Set the absolute path of the folder in which MACRO (or MACRO In FOCUS) executable is installed.
#'
#'Set the absolute path of the folder in which MACRO (or 
#'  MACRO In FOCUS) executable is installed. as well as 
#'  the name of MACRO executable and the name of the exeparfile 
#'  executable.
#'  
#'  Regarding the path to MACRO-folder, the function 
#'  proceeds as follow: If a value is given (argument 
#'  code{path}, see below), it is used to set MACRO-path in 
#'  the package-option \code{macro_path} (see 
#'  \code{\link[rmacrolite]{rmlPar}}), value that is then used 
#'  by other function in this package. If no value is given 
#'  (argument code{path} left to \code{NULL}), the function 
#'  will first search for a Windows Environment Variable 
#'  (System Variable) called \code{rmacrolite_macro_path}, 
#'  and use this value if it exists, to set \code{macro_path} 
#'  in \code{\link[rmacrolite]{rmlPar}}. See 
#'  \code{\link[rmacrolite]{rmacrolite-system-variables}}. 
#'  Finally, if neither the argument 
#'  code{path} is set, nor a System Variable called 
#'  \code{rmacrolite_macro_path}, the function will try the 
#'  factory-default path to MACRO In FOCUS (type 
#'  \code{getRmlPar("macro_path_default")} to find out).
#'  
#'  The principle is the same for MACRO executable (argument 
#'  \code{exe}, System Variable \code{rmacrolite_macro_exe}, factory 
#'  default set in \code{getRmlPar("macro_exe_default")}) 
#'  as well as for the exeparfile executable (argument 
#'  \code{exeparfile}, System Variable 
#'  \code{rmacrolite_macro_exeparfile}, factory default set in 
#'  \code{getRmlPar("macro_exeparfile_default")}).
#'
#'
#'@return 
#'  Invisibly returns a \code{\link[base]{list}} with 3 named 
#'  items: \code{path}, \code{exe} and \code{exeparfile}, 
#'  set to the values found out by the function.
#'
#'
#'@seealso \code{\link[rmacrolite:rmacroliteSetModelVar-methods]{rmacroliteSetModelVar}} and 
#'  \code{\link[rmacrolite]{rmacrolite-system-variables}}.
#'
#'
#'@example inst/examples/rmacroliteGetModelVar-example.r
#'
#'@export 
#'
rmacroliteGetModelVar <- function(){  
    #   Set the path to MACRO executable:
    .par <- rmlPar( c( "macro_path", "macro_exe", 
        "macro_exeparfile" ) )
    
    path       <- .par[[ "macro_path" ]][ 1L ] 
                   # [ 1L ] means additional values are ignored
    exe        <- .par[[ "macro_exe" ]][ 1L ]
    exeparfile <- .par[[ "macro_exeparfile" ]][ 1L ] 
    rm( .par )
    
    if( !is.null( path ) ){
        #   Test that the folder exists
        if( !file.exists( path ) ){
            stop( sprintf( 
                "The folder %s could not be found (defined from argument getRmlPar('path'))", 
                path 
            ) )  
        }   
        
    }else{
        path <- Sys.getenv("rmacrolite_macro_path", 
            unset = NA_character_)
        
        if( !is.na( path ) ){
            #   Test that the folder exists
            if( !file.exists( path ) ){
                stop( sprintf( 
                    "The folder %s could not be found (defined from: Sys.getenv('rmacrolite_macro_path'))", 
                    path 
                ) )  
            }   
        
        }else{
            path <- getRmlPar( "macro_path_default" )
            
            if( !file.exists( path ) ){
                stop( sprintf( 
                    "The folder %s could not be found (defined from: getRmlPar('macro_path_default'))", 
                    path 
                ) )  
            }   

        }   
    }       
    
    
    #   Set the name of MACRO and the exeparfile executables
    for( v in c( "exe", "exeparfile" ) ){
        v_value <- get0( x = v )
        
        if( !is.null( v_value ) ){            
            #   Test that the folder exists
            if( !file.exists( file.path( path, v_value ) ) ){
                stop( sprintf( 
                    "The file %s could not be found in %s (defined from getRmlPar('macro_%s'))", 
                    v_value, path, v 
                ) )  
            }   
            
        }else{
            v_value <- Sys.getenv( sprintf( "rmacrolite_%s", v ), 
                unset = NA_character_ )
            
            if( !is.na(v_value) ){
                #   Test that the folder exists
                if( !file.exists( file.path( path, v_value ) ) ){
                    stop( sprintf( 
                        "The file %s folder could not be found in %s (defined from: Sys.getenv('rmacrolite_macro_%s'))", 
                        v_value, path, v 
                    ) ) 
                }   
            
            }else{
                v_value <- getRmlPar( sprintf( "macro_%s_default", v ) )
                
                if( !file.exists( file.path( path, v_value ) ) ){
                    stop( sprintf( 
                        "The file %s folder could not be found in %s (defined from: getRmlPar('macro_%s_default'))", 
                        v_value, path, v 
                    ) )  
                }   

            }   
        }   
        
        assign( x = v, value = v_value )
    }       
    
    

    out <- list( "path" = path, "exe" = exe, "exeparfile" = exeparfile )
    
    return( out )
}   



# .rml_textToPOSIXct ====================================================

# #'Convert a text representation of a date to as POSIXct-class object
# #' 
# #'Convert a text representation of a date to as POSIXct-class object.
# #'  Performs additional checks as compared to 
# #'  \code{\link[base:as.POSIXlt]{as.POSIXct}} (on which the function is build)
# #' 
# #' 
# #'@param x
# #'  See \code{\link[base:as.POSIXlt]{as.POSIXct}}. Can be a vector of character 
# #'  strings
# #' 
# #'@param \dots
# #'  See \code{\link[base:as.POSIXlt]{as.POSIXct}}.
# #' 
# #'@param format
# #'  See \code{\link[base:as.POSIXlt]{as.POSIXct}}.
# #' 
# #'@return 
# #'  Returns the result of 
# #'  \code{\link[base:as.POSIXlt]{as.POSIXct}}\code{(x=x,format=format,tz=tz,...)} 
# #'  and gives warning if thinks the dates are ill-formatted.
# #' 
# #'
# #'@export
# #'
# #'@rdname rml_textToPOSIXct
# #'
# #'@keywords internal
# #' 
# .rml_textToPOSIXct <- function( 
    # x, 
    # format = getRmlPar( "timeFormat" ), 
    # tz     = getRmlPar( "tz" ), 
    # ...
# ){  
    # format <- format[ 1L ]
    
    # #   Find out what is the separator used for the date-template
    # if( grepl( x = format, pattern = "-", fixed = TRUE ) ){ 
        # sep <- "-" 
    # }else if( grepl( x = format, pattern = "/", fixed = TRUE ) ){ 
        # sep <- "/" 
    # }else if( grepl( x = format, pattern = "\\", fixed = TRUE ) ){ 
        # sep <- "\\" 
    # }else if( grepl( x = format, pattern = ".", fixed = TRUE ) ){ 
        # sep <- "." 
    # }else{ 
        # sep <- NULL 
    # }   
    
    
    # #   Find out what is the separator used for the date-values
    # if( !is.null( sep ) ){ 
        # testSep <- grepl( x = x, pattern = sep, fixed = TRUE ) 
        
        # if( any( !testSep ) ){ 
            # if( grepl( x = x, pattern = "-", fixed = TRUE ) ){ 
                # sep2 <- "-" 
            # }else if( grepl( x = x, pattern = "/", fixed = TRUE ) ){ 
                # sep2 <- "/" 
            # }else if( grepl( x = x, pattern = "\\", fixed = TRUE ) ){ 
                # sep2 <- "\\" 
            # }else if( grepl( x = x, pattern = ".", fixed = TRUE ) ){ 
                # sep2 <- "." 
            # }else{ 
                # sep2 <- "not identified" 
            # }   
            
            # warning( sprintf( 
                # "Possible mismatch between date element separator in 'timeFormat' option (?getRmlPar) (%s) and some text dates representation (%s)", 
                # sep, sep2 
            # ) ) 
            
            # rm( sep2 )
        # }   
        
        # rm( testSep )
    # }   
    
    
    # #   Make sure the date and the format are coherent
    # testYear <- grepl( x = format, pattern = "%Y", fixed = TRUE ) 
    
    # if( !is.null( sep ) & testYear ){ 
        # xSplit <- strsplit( x = x, split = sep, fixed = TRUE ) 
        # # split_format <- strsplit( x = format, split = sep ) 
        
        # #   Find out the position of the year in expected date 
        # #   format
        # yearPos <- as.integer( regexec( pattern = "%Y", text = format, 
            # fixed = TRUE ) ) 
        
        # if( yearPos == 1L ){ 
            # #   year comes 1st
            # item <- 1L 
            
        # }else if( yearPos == 4L ){ 
            # #   year comes 2nd
            # item <- 2L 
            
        # }else if( yearPos == 7L ){ 
            # #   year comes 3rd
            # item <- 3L 
            
        # }else{ 
            # #   Strange case
            # item <- NULL 
            
            # warning( "Weird date format in 'timeFormat' option (?getRmlPar)" )
        # }   
        
        
        # if( !is.null( item ) ){ 
            # xSplitLength <- unlist( lapply( 
                # X   = xSplit, 
                # FUN = function(X){ 
                    # length( X )
                # }   
            # ) ) 
            
            # if( any( xSplitLength != 3 ) ){ 
                # warning( 
                    # "Some text dates representation apparently do not have 3 items", 
                    # "\n  (see 'timeFormat' option (?getRmlPar).", 
                    # "\n  Something might be wrong (or go wrong later)" )
            # }else{ 
                # xSplitYearLength <- unlist( lapply( 
                    # X   = xSplit, 
                    # FUN = function(X){ 
                        # X <- X[ item ] 
                        # X <- strsplit( x = X, split = " " )[[ 1L ]][ 1L ] 
                        
                        # nchar( X )
                    # }   
                # ) ) 
                
                # if( any( xSplitYearLength != 4L ) ){ 
                    # warning( 
                        # "Some text dates representation apparently have a 'year' item that are not 4-characters long", 
                        # "\n  or in the wrong place (see 'timeFormat' option (?getRmlPar).", 
                        # "\n  Something might be wrong (or might go wrong later)" )
                # }   
            # }   
        # }   
        
        
        
    # };  rm( testYear )
    
    # if( max( nchar( x ) ) <= 10 ){ 
        # x <- as.POSIXct( 
            # x      = x, 
            # format = substr( x = format, start = 1L, stop = 8 ), 
            # tz     = tz, 
            # ... )
    # }else{ 
        # x <- as.POSIXct( 
            # x      = x, 
            # format = format, 
            # tz     = tz, 
            # ... )
    # }   
    
    # if( any( is.na( x ) ) ){ 
        # warning( "Some converted dates (from text) resulted in NA (missing) values. Most likely a problem occurred." )
    # }   
    
    # return( x ) 
# }   

    # .rml_textToPOSIXct( x = "2014-06-30", format = "%Y-%m-%d", tz = "GMT" ) 
    # .rml_textToPOSIXct( x = "2014/06/30", format = "%Y/%m/%d", tz = "GMT" ) 
    # .rml_textToPOSIXct( x = "2014 06 30", format = "%Y %m %d", tz = "GMT" ) 
    # .rml_textToPOSIXct( x = "30-06-2014", format = "%d-%m-%Y", tz = "GMT" ) 
    # .rml_textToPOSIXct( x = "30-06-2014 12:00", format = "%d-%m-%Y %H:%M", tz = "GMT" ) 
    # # With warnings:
    # .rml_textToPOSIXct( x = "2014/06/30", format = "%Y-%m-%d", tz = "GMT" ) # wrong separator
    # .rml_textToPOSIXct( x = "30/06/2014", format = "%Y/%m/%d", tz = "GMT" ) # wrong position
    # # No warnings, but should:
    # .rml_textToPOSIXct( x = "30-06-2014 12:00", format = "%d-%m-%Y", tz = "GMT" ) # incomplete



#   Version of the function not relying on the system format
.rml_textToPOSIXct2 <- function( 
    x, 
    tz = getRmlPar( "tz" ), 
    ...
){  
    # rm(list=ls(all=TRUE)); x <- c( "1901-01-01 00:00", "1927-01-01 00:00" ); x <- "30.06.2014 12:00"; tz = "GMT"
    
    x_split <- strsplit( x = x, split = " ", fixed = TRUE )
    
    length_x_split <- unlist( lapply( X = x_split, FUN = length ) ) 
    
    if( any( length_x_split > 2L ) ){
        stop( sprintf( 
            "Can't separate date from time in string(s) %s.", 
            paste( which( length_x_split > 2L ), collapse = ", " ) ) )
            
    }else if( all( length_x_split == 2L ) ){
        x0_includes_time <- TRUE 
        
    }else if( all( length_x_split == 1L ) ){
        x0_includes_time <- FALSE 
        
    }else{
        stop( "Unclear if the string(s) contain dates and time or just dates." )
    }   
    
    
    
    x_dates <- unlist( lapply(
        X   = x_split, 
        FUN = function( x_split0 ){
            return( x_split0[ 1L ] )
        }   
    ) ) 
    
    sep_date <- unlist( lapply(
        X   = x_dates, 
        FUN = function( x_dates0 ){
            #   Find out what is the date separator used
            if( grepl( x = x_dates0, pattern = "-", fixed = TRUE ) ){ 
                sep_date <- "-" 
            }else if( grepl( x = x_dates0, pattern = "/", fixed = TRUE ) ){ 
                sep_date <- "/" 
            }else if( grepl( x = x_dates0, pattern = "\\", fixed = TRUE ) ){ 
                sep_date <- "\\" 
            }else if( grepl( x = x_dates0, pattern = ".", fixed = TRUE ) ){ 
                sep_date <- "." 
            }else{ 
                stop( sprintf( 
                    "Can't figure out the date separator in string '%s'.", 
                    x_dates0 ) )
            }   
            
            return( sep_date )
        }   
    ) ) 
    
    if( length( unique( sep_date ) ) > 1L ){
        stop( sprintf( 
            "Identified %s different date-separators for the text-strings: ", 
            length( unique( sep_date ) ), 
            paste( unique( sep_date ), collapse = " " ) 
        ) ) 
    }else{
        sep_date <- unique( sep_date )
    }   
    
    x_chars <- lapply(
        X   = x_split, 
        FUN = function( x_split0 ){
            return( strsplit( x = x_split0, split = "" ) )
        }   
    )   
    
    nb_sep_date <- unlist( lapply(
        X   = x_chars, 
        FUN = function( x_chars0 ){
            return( length( grep( 
                x       = x_chars0[[ 1L ]], 
                pattern = sep_date, 
                fixed   = TRUE ) ) )
        }   
    ) ) 
    
    if( !all( nb_sep_date == 2L ) ){
        stop( sprintf( 
            "Some strings contains a number of date-separators ('%s') different from 2.", 
            sep_date ) )
    }   
    
    #   Try to find if year comes first or last
    year_first <- unlist( lapply(
        X   = strsplit( x = x_dates, split = sep_date, fixed = TRUE ), 
        FUN = function(x_dates0){
            nchar_dates <- nchar( x_dates0 )
            
            if( nchar_dates[ 1L ] == 4L ){
                return( TRUE )
            }else if( nchar_dates[ length(nchar_dates) ] == 4L ){
                return( FALSE )
            }else{
                return( as.logical( NA ) )
            }   
        }   
    ) ) 
    
    if( length( unique( year_first ) ) != 1L ){
        stop( "Unclear position for the year-item in the dates." )
        
    }else{
        year_first <- unique( year_first )
    }   
    
    #   Try to convert the date-string into a Date
    if( is.na( year_first ) ){
        tryFormats_date <- c( "%y/%m/%d", "%d/%m/%y" ) 
        
    }else if( year_first ){
        tryFormats_date <- "%Y/%m/%d" 
        
    }else{
        #   year last
        tryFormats_date <- "%d/%m/%Y" 
    }   
    
    tryFormats_date <- gsub( pattern = "/", replacement = sep_date, 
        x = tryFormats_date, fixed = TRUE )
    
    #   Convert from text to dates
    x_conv <- as.Date( x = x_dates, tryFormats = tryFormats_date )
    
    rm( length_x_split, nb_sep_date, tryFormats_date, x_dates )
    
    
    
    if( x0_includes_time ){
        x_times <- unlist( lapply(
            X   = x_split, 
            FUN = function( x_split0 ){
                return( x_split0[ 2L ] )
            }   
        ) ) 
        
        sep_time <- unlist( lapply(
            X   = x_times, 
            FUN = function( x_times0 ){
                #   Find out what is the date separator used
                if( grepl( x = x_times0, pattern = ":", fixed = TRUE ) ){ 
                    sep_time <- ":" 
                }else if( grepl( x = x_times0, pattern = ".", fixed = TRUE ) ){ 
                    sep_time <- "." 
                }else{ 
                    stop( sprintf( 
                        "Can't figure out the time separator in string '%s'.", 
                        x_times0 ) )
                }   
                
                return( sep_time )
            }   
        ) ) 
        
        if( length( unique( sep_time ) ) > 1L ){
            stop( sprintf( 
                "Identified %s different time-separators for the text-strings: ", 
                length( unique( sep_time ) ), 
                paste( unique( sep_time ), collapse = " " ) 
            ) ) 
        }else{
            sep_time <- unique( sep_time )
        }   
        
        nb_sep_time <- unlist( lapply(
            X   = x_chars, 
            FUN = function( x_chars0 ){
                return( length( grep( 
                    x       = x_chars0[[ 2L ]], 
                    pattern = sep_time, 
                    fixed   = TRUE ) ) )
            }   
        ) ) 
        
        if( !all( nb_sep_time == 1L ) ){
            stop( sprintf( 
                "Some strings contains a number of time-separators ('%s') different from 1.", 
                sep_time ) )
        }   
        
        if( sep_time == sep_date ){
            stop( sprintf( 
                "Date and time separator seems to be identical ('%s').", 
                sep_time ) )
        }   
        
        #   Try to convert the date-string into a Date
        if( is.na( year_first ) ){
            tryFormats_time <- c( "%y/%m/%d %H:%M", 
                "%d/%m/%y %H:%M" ) 
            
        }else if( year_first ){
            tryFormats_time <- "%Y/%m/%d %H:%M" 
            
        }else{
            #   year last
            tryFormats_time <- "%d/%m/%Y %H:%M" 
        }   
        
        #   Try to convert the date-string into a Date
        tryFormats_time <- gsub( pattern = "/", 
            replacement = sep_date, x = tryFormats_time, 
            fixed = TRUE )
        tryFormats_time <- gsub( pattern = ":", 
            replacement = sep_time, x = tryFormats_time, 
            fixed = TRUE )
        
        #   Convert from text to dates
        x_conv <- as.POSIXct( x = x, tryFormats = tryFormats_time, tz = tz )
    }else{
        x_conv <- as.POSIXct( x = x_conv, tz = tz )
    }   
    
    return( x_conv )
}   

    # format( .rml_textToDateTime( x = "2014-06-30", tz = "GMT" ), "%Y-%m-%d %H:%M %Z", tz = "GMT" )
    # format( .rml_textToDateTime( x = "2014/06/30", tz = "GMT" ), "%Y-%m-%d %H:%M %Z", tz = "GMT" )
    # format( .rml_textToDateTime( x = "30-06-2014", tz = "GMT" ), "%Y-%m-%d %H:%M %Z", tz = "GMT" )
    # format( .rml_textToDateTime( x = "30-06-2014 12:00", tz = "GMT" ), "%Y-%m-%d %H:%M %Z", tz = "GMT" )
    # format( .rml_textToDateTime( x = "30.06.2014 12:00", tz = "GMT" ), "%Y-%m-%d %H:%M %Z", tz = "GMT" )
    # format( .rml_textToDateTime( x = "14-06-30", tz = "GMT" ), "%Y-%m-%d %H:%M %Z", tz = "GMT" )
    # #   Return a wrong result:
    # format( .rml_textToDateTime( x = "30-06-14", tz = "GMT" ), "%Y-%m-%d %H:%M %Z", tz = "GMT" )



# rmacroliteSimPeriod ========================================

# #   Patch a bug in R c() that change the time zone when 
# #   concatenating POSIXct times 
# .rml_cPOSIXct <- function( ... ){
    # timeFormat <- getRmlPar( "timeFormat" )
    
    # dotDot <- list( ... )
    
    # #   From POSIXct to characters
    # out <- unlist( lapply(
        # X   = dotDot, 
        # FUN = function(x){
            # return( format.POSIXct( x, format = timeFormat ) )
        # }   
    # ) ) 
    
    # .tz <- format.POSIXct( dotDot[[ 1L ]], format = "%Z" )
    
    # #   And back to POSIXct
    # out <- as.POSIXct( x = out, format = timeFormat, tz = .tz )
    
    # return( out )
# }   

    # # a <- as.POSIXct( "2015-01-01 12:00", format = "%Y-%m-%d %H:%M", 
        # # tz = "GMT" )
    
    # # .rml_cPOSIXct( a, a )
    # # # Note: May loose the seconds, if they exists



#'Fetch the simulation input period (start / stop time) from imported MACRO parameters
#'
#'@description
#'  Fetch the simulation input period (start 
#'  / stop time) from imported MACRO parameters.
#'
#'
#'@param x 
#'  A 'macroParFile' object, such as obtained with 
#'  \code{\link[rmacrolite]{rmacroliteImportParFile-methods}}.
#'
#'@param climate
#'  Single logical value. If \code{TRUE} (the default), the 
#'  time-period for the weather data is also extracted.
#'
#'@param \dots
#'  Not used.
#'
#'@return 
#'  Returns a list with 4 items: \code{sim}, a vector 
#'  of two \code{\link{POSIXct}} time-dates, start and end 
#'  time of the simulation; \code{metPeriod}, a vector 
#'  of two \code{\link{POSIXct}} time-dates, as read from the 
#'  parameter 
#'  \code{METPERIOD}. \code{rainBinPeriod} and \code{metBinPeriod} 
#'  are the same, but read from the rainfall and meteorological 
#'  data directly.
#'
#'
#'@example inst/examples/rmacroliteSimPeriod-example.r
#'
#'@rdname rmacroliteSimPeriod-methods
#'@aliases rmacroliteSimPeriod
#'
#'@export 
#'
#'@docType methods
#'
rmacroliteSimPeriod <- function( 
    x, 
    ... 
){  
    UseMethod( "rmacroliteSimPeriod" )
}   



#'@rdname rmacroliteSimPeriod-methods
#'
#'@method rmacroliteSimPeriod macroParFile
#'@export 
rmacroliteSimPeriod.macroParFile <- function( 
    x, 
    climate = TRUE, 
    # check = TRUE, 
    ... 
){  
    #   Find out date and time format
    .rmlPar <- rmlPar() 
    
    # timeFormat        <- .rmlPar[[ "timeFormat" ]] 
    .tz <- .rmlPar[[ "tz" ]] 
    
    
    
    .start <- rmacroliteGet1Param( 
         x    = x, 
         pTag = "STARTDATE\t%s", 
         type = "SETUP" 
    )   
    
    # if( nchar( .start ) <= 10 ){ 
        # .start <- .rml_textToPOSIXct( 
            # x      = .start, 
            # format = substr( 
                # x     = timeFormat, 
                # start = 1, 
                # stop  = 8 ), 
            # tz     = .tz ) 
    # }else{ 
        # .start <- .rml_textToPOSIXct( 
            # x      = .start, 
            # format = timeFormat, 
            # tz     = .tz ) 
    # }   
    
    .end <- rmacroliteGet1Param( 
         x    = x, 
         pTag = "ENDDATE\t%s", 
         type = "SETUP" 
    )   
    
    # if( nchar( .end ) <= 10 ){ 
        # .end <- .rml_textToPOSIXct( 
            # x      = .end, 
            # format = substr( 
                # x     = timeFormat, 
                # start = 1, 
                # stop  = 8 ), 
            # tz     = .tz ) 
    # }else{ 
        # .end <- .rml_textToPOSIXct( 
            # x      = .end, 
            # format = timeFormat, 
            # tz     = .tz ) 
    # }   
    
    startend <- c( .start, .end )
    
    startend <- .rml_textToPOSIXct2( 
        x      = startend, 
        tz     = .tz ) 
    
    names( startend ) <- c( "start", "end" ) 
    
    
    
    if( climate ){
        # Import and define the Weather period
        .metPeriod <- rmacroliteGet1Param( 
            x    = x, 
            pTag = "METPERIOD\t%s", 
            type = "SETUP" 
        )      
        
        if( grepl( x = .metPeriod, pattern = "- " ) ){
            .metPeriod <- strsplit( x = .metPeriod, split = "- ", 
                fixed = TRUE )[[ 1L ]] 
        }else{
            nchar_metPeriod <- nchar( .metPeriod )
            
            item_size <- nchar_metPeriod %/% 2
            
            if( (item_size * 2 + 1L) != nchar_metPeriod ){
                stop( sprintf( 
                    "Unable to split METPERIOD ('%s') in two equal size strings separated by 1 character.", 
                    .metPeriod ) ) 
            }   
            
            test_sep <- substr( x =  .metPeriod, 
                start = item_size + 1L, stop = item_size + 1L )
            test_sep <- test_sep == "-"
            
            if( !test_sep ){
                stop( sprintf( 
                    "The character identified as start/end separator in METPERIOD ('%s') is not a minus sign ('-').", 
                    .metPeriod ) ) 
            }   
            
            .metPeriod <- c( 
                substr( x = .metPeriod, start = 1L, 
                    stop = item_size ), 
                substr( x = .metPeriod, start = item_size + 2L, 
                    stop = nchar( .metPeriod ) ) ) 
        }   
        
        # if( nchar( .metPeriod[ 1L ] ) <= 10 ){ 
            # .metPeriod <- .rml_textToPOSIXct( 
                # x      = .metPeriod, 
                # format = substr( 
                    # x     = timeFormat, 
                    # start = 1, 
                    # stop  = 8 ), 
                # tz     = .tz ) 
        # }else{ 
            # .metPeriod <- .rml_textToPOSIXct( 
                # x      = .metPeriod, 
                # format = timeFormat, 
                # tz     = .tz ) 
        # }   
        
        .metPeriod <- .rml_textToPOSIXct2( 
            x      = .metPeriod, 
            tz     = .tz ) 
        
        names( .metPeriod ) <- c( "start", "end" )
        
        #   Format output
        out <- list(
            "sim"           = startend, 
            "metPeriod"     = .metPeriod ) 
    }else{
        out <- list(
            "sim"           = startend ) 
    }   
    
    return( out ) 
}   



# rmacroliteImportParFile ============================================

#'Imports parameters from one or several MACRO parameter-file(s)
#'
#'Imports parameters from one or several MACRO parameter-file(s)
#'
#'
#'@param file 
#'  Single character string or vector of character strings.
#'  Name(s) of the MACRO In FOCUS parameter file(s) (par-file) 
#'  to be imported. The file(s) should be located in the same 
#'  folder as the MACRO In FOCUS executable 
#'  (see \code{\link[rmacrolite:rmacroliteSetModelVar-methods]{rmacroliteSetModelVar}}), or in a subfolder
#'  in this folder (in this case indicate the relative path, 
#'  not the full path).
#'  
#'  Notice that R file separator is a slash (\code{/}), or a double 
#'  slash, but not a single backslash (although double backslash 
#'  would work).
#'
#'@param verbose
#'  Single integer value. If set to a value \code{< 1}, 
#'  the program is silent (except on errors or warnings). If 
#'  set to \code{1}, the program outputs messages. Values 
#'  \code{> 1} may also activate messages from lower level 
#'  functions (for debugging purpose).
#'
#'@param climate
#'  Single logical value. If \code{TRUE} (the default), the 
#'  function checks that the climate files exists and stop 
#'  if they don't.
#'
#'@param \dots
#'  Additional parameters passed to \code{\link[base]{readLines}}.
#'
#'@return 
#'  Returns a 'macroParFile' object
#'
#'
#'@example inst/examples/rmacroliteImportParFile-example.r
#'
#'@rdname rmacroliteImportParFile-methods
#'@aliases rmacroliteImportParFile
#'
#'@export 
#'
#'@docType methods
#'
rmacroliteImportParFile <- function( 
    file, 
    climate = TRUE, 
    verbose = 1L, 
    ... 
){  
    UseMethod( "rmacroliteImportParFile" )
}   



#'@importFrom macroutils2 macroReadBin
#'@importFrom utils read.table
.rmacroliteImportParFile <- function( 
    file, 
    climate, 
    verbose, 
    ... 
){  
    #   Test if the folder exists
    fTestExists <- file.exists( file )
    if( !all( fTestExists ) ){ 
        stop( sprintf( 
            "Could not find the par file (%s)", 
            paste( file[ !fTestExists ], collapse = "; " ) 
        ) ) 
    };  rm( fTestExists )
    
    
    # verbose <- getRmlPar( "verbose" )
    log_width <- getRmlPar( "log_width" )
    
    # ====== Import the par file ================================
    .rml_logMessage( m = "Importing MACRO parameter file (.par)", 
        verbose = verbose, log_width = log_width )
    
    encoding <- getRmlPar( "encoding" )[1L]
    
    parData <- readLines( con = file[ 1L ], 
        encoding = encoding ) 
    
    n <- length(parData)
    
    
    
    bom_mark <- iconv("\ufeff", to = ifelse( 
        test = tolower( encoding ) == "utf-8-bom", 
        yes  = "UTF-8", 
        no   = encoding ) )
    
    has_bom_mark <- substr( 
        x     = parData[ 1L ], 
        start = 1L, 
        stop  = nchar( bom_mark ) ) == bom_mark
    
    if( has_bom_mark ){  
        parData[ 1L ] <- substr( 
            x     = parData[ 1L ], 
            start = nchar( bom_mark ) + 1L, 
            stop  = nchar( parData[ 1L ] ) )
    }   
    
    rm( has_bom_mark )
    
    
    
    # ====== Find parameter categories  =========================
    .rml_logMessage( m = "Formatting parameters", 
        verbose = verbose, log_width = log_width )
    
    catLimits  <- grep( x = parData, pattern = "******", fixed = TRUE ) 
    catHeaders <- parData[ catLimits + 1 ]
    
    catLimits  <- c( 1, catLimits ) 
    catHeaders <- c( "HEAD", catHeaders ) 
    nCat       <- length(catLimits) 
    
    categories <- unlist( lapply( 
        X   = 1:nCat, 
        FUN = function(X){ 
            
            if( X != nCat ){ 
                out <- rep( x = catHeaders[X], 
                    times = length( catLimits[X]:(catLimits[X+1]-1) ) ) 
            }else{ 
                out <- rep( x = catHeaders[X], 
                    times = length( catLimits[X]:n ) ) 
            }   
            
            return( out ) 
        }   
    ) ) 
    
    parData <- data.frame( 
        "parFile"  = parData, 
        "category" = categories, 
        stringsAsFactors = FALSE 
    )       
    
    
    
    # ====== Output parameters ==================================
    
    .rml_logMessage( m = "Formatting output", verbose = verbose, 
        log_width = log_width )
    
    out <- list( 
        "par"           = parData, 
        # "rainfallData"  = rainfall, 
        # "metData"       = met, 
        "file"          = file 
    )   
    
    class( out ) <- "macroParFile"
    
    
    
    # ====== Fetch start and end dates ==========================
    
    # .rml_logMessage( m = "Fetching start- and end-times of the simulation", 
        # verbose = verbose, log_width = log_width )
    
    # sp <- rmacroliteSimPeriod( x = out )
    
    
    # #   Find out date and time format
    # timeFormat        <- getRmlPar( "timeFormat" )
    # .tz               <- getRmlPar( "tz" ) 
    
        
    
    # startEnd <- sp[[ "sim" ]] 
    
    
    
    # attr( x = out, which = "timePeriods" ) <- sp 
    
    # if( any( is.na( sp[[ "sim" ]] ) ) ){ 
        # warning( "Something went wrong when converting start and end simulation dates. NA value(s). This can cause further problems" )
    # }   
    
    
    
    # Check climate files ==================================
    
    .rml_logMessage( 
        m = "Check if rainfall and weather files exists", 
        verbose = verbose, log_width = log_width )
    
    climate <- rmacroliteClimateFiles( x = out, check = climate )
    
    return( out ) 
}   



#'@rdname rmacroliteImportParFile-methods
#'
#'@method rmacroliteImportParFile default
#'@export 
rmacroliteImportParFile.default <- function( 
    file, 
    climate = TRUE, 
    verbose = 1L, 
    ... 
){  
    # verbose          <- getRmlPar( "verbose" ) 
    log_width <- getRmlPar( "log_width" )
    macro_path <- getRmlPar( "macro_path" )
    
    out <- lapply( 
        X   = file, 
        FUN = function( .file ){ 
            .rml_logMessage( m = "Importing MACRO parameter file %s", 
                verbose = verbose, log_width = log_width, 
                values = list( .file[ 1L ] ) ) 
            
            out <- .rmacroliteImportParFile( 
                file    = .file, 
                climate = climate, 
                verbose = verbose - 1L, 
                ... 
            )   
            
            return( out )
        }   
    )   
    
    if( length( file ) == 1L ){ 
        out <- out[[ 1L ]] 
        
    }else{ 
        names( out ) <- as.character( 1:length( out ) ) 
        
        class( out ) <- "macroParFileList"
        
        attr( x = out, which = "parameterTable" ) <- data.frame() 
        
    }   
    
    return( out )
}   



# .rml_testMacroFilePath =========================================

## # Test a path for compatibility with MACRO command line modules.
## # 
## # Internal, utility function. It checks that the number of character 
## #  in a path does not exceed a certain value accepted by MACRO command 
## #  line tools, and give an error (stop) if they do. The path is 
## #  normalised and only one folder separator is used (i.e. /, not \\).
## # 
## # 
## #@param path 
## #  Single character string. Path whose length must be checked.
## # 
## #@param errorOp
## #  Single logical. If set to TRUE, will give an error if the path 
## #  length is > nchar( path ). If FALSE, will just give a warning.
## # 
## # 
## #@return 
## #  The function returns (invisibly) a sanitised file path
## # 
## # 
.rml_testMacroFilePath <- function(
    path,
    errorOp = TRUE
){  # Automatically set the default parameters that are NULL:
    maxPathLength <- getRmlPar( "maxPathLength" ) 
    
    path <- unlist( lapply(
        X   = path, 
        FUN = function(p){
            for( i in 1:2 ){ 
                p <- gsub(
                    pattern     = "\\", 
                    replacement = "/", 
                    x           = p,
                    ignore.case = FALSE, 
                    # extended  = TRUE, # Defunct? 2010-04-27 
                    perl        = FALSE, 
                    fixed       = TRUE, 
                    useBytes    = FALSE
                )   
            }   
            
            n <- nchar( p ) 
            
            if( n > maxPathLength ){ 
                msg <- sprintf( "Path too long, > %s characters (%s)", maxPathLength, p )
                
                if( errorOp ){   
                    stop( msg ) 
                    
                }else{ 
                    message( msg ) 
                    
                }   
            }   
            
            return( p ) 
        }   
    ) ) 
    
    return( invisible( path ) ) 
}   



# rmacroliteGet1Param ================================================

#'Find the value of one parameter from an imported MACRO parameter file (PAR)
#'
#'Find the value of one parameter from an imported MACRO 
#'  parameter file (PAR). If several rows are found that match 
#'  the parameter tag, all values are returned.
#'
#'
#'@param x 
#'  A \code{macroParFile} object.
#'
#'@param pTag 
#'  Single character string. Text string containing the parameter 
#'  value that should be searched and replaced in the PAR file, 
#'  with the parameter value replaced by the string \code{\%s}.
#'  NOTE: CASE SENSITIVE!
#'
#'@param type 
#'  Single character string. Parameter category (category's header 
#'  in the PAR file)
#'
#'
#'@return 
#'  Returns vector of \bold{character} strings.
#'
#'
#'@rdname rmacroliteGet1Param
#'
#'@example inst/examples/rmacroliteGet1Param-example.r
#'
#'@export
#'
rmacroliteGet1Param <- function( 
    x, 
    pTag, 
    type = NULL 
){  
    if( length( pTag ) != 1L ){ 
        stop( "'pTag' must be a character string of length 1" ) 
    }   
    
    
    #   Fetch the 1st part of the tag
    pTag1 <- strsplit( x = pTag, split = "%s", fixed = TRUE )[[ 1L ]] 
    
    lTag <- length( pTag1 ) 
    
    if( lTag == 1 ){ 
        pTag2 <- NA_character_ 
    }else if( lTag == 2 ){ 
        pTag2 <- pTag1[ 2 ] 
        pTag1 <- pTag1[ 1 ] 
    }else{ 
        stop( "Can't handle tags that can be split in more than 2 pieces" )
    }   
    
    
    
    #   Find rows matching the tag
    sRow <- substr( x = tolower( x[[ "par" ]][, "parFile" ] ), 
        start = 1, stop = nchar( pTag1 ) ) == tolower( pTag1 ) 
    # sRow <- grepl( x = tolower( x[[ "par" ]][, "parFile" ] ), pattern = tolower( pTag1 ), 
        # fixed = TRUE ) 
    
    # & (x[[ "par" ]][, "category" ] == "HEAD")
    if( !is.null( type ) ){ 
        sRow <- sRow & (x[[ "par" ]][, "category" ] == type)
    }   
    
    if( sum( sRow ) == 0 ){ 
        stop( sprintf( 
            "No row found matching the PAR tag (type:'%s', tag:'%s')", 
            type, pTag1 
        ) ) 
    }   
    
    #   Fetch only the relevant part
    out <- x[[ "par" ]][ sRow, "parFile" ]
    
    
    #   Delete Markup around the parameter value
    out <- gsub( pattern = pTag1, replacement = "", x = out, 
        fixed = TRUE ) # , ignore.case = TRUE
    
    if( !is.na( pTag2 ) ){ 
        out <- strsplit( x = out, split = pTag2, fixed = TRUE ) 
        
        layers <- unlist( lapply( X = out, FUN = function( X ){ X[ 1 ] } ) ) 
        
        out <- unlist( lapply( X = out, FUN = function( X ){ X[ 2 ] } ) ) 
        
        attr( x = out, which = "layer" ) <- layers 
        
        # out <- gsub( pattern = pTag2, replacement = "", x = out, 
            # fixed = TRUE ) # , ignore.case = TRUE
    }   
    
    attr( x = out, which = "index" ) <- which( sRow ) 
    
    
    
    return( out ) 
}   
    
    # # Retrieve ALBEDO
    # rmacroliteGet1Param( x = pr, pTag = "ALBEDO\t%s" )
    
    # # Retrieve ALPHA from layer 1
    # rmacroliteGet1Param( x = pr, pTag = "ALPHA\t1\t%s" )
    # # Alternatively
    # rmacroliteGet1Param( x = pr, pTag = "ALPHA\t%s\t%s", type = "PHYSICAL PARAMETERS" )



# rmacroliteChange1Param ================================================

#'Change the value of one parameter from an imported MACRO parameter file (PAR)
#'
#'Change the value of one parameter from an imported MACRO 
#'  parameter file (PAR). If several rows are found that match 
#'  the parameter tag, all values are changed.
#'
#'
#'@param x 
#'  A \code{macroParFile} object, as obtained with 
#'  \code{\link[rmacrolite]{rmacroliteImportParFile-methods}}.
#'
#'@param pTag 
#'  Single character string. Text string containing the parameter 
#'  value that should be searched and replaced in the PAR file, 
#'  with the parameter value replaced by the string \code{\%s}.
#'  NOTE: CASE SENSITIVE!
#'
#'@param tagNb 
#'  Vector of integer values. If several rows have the same 
#'  \code{pTag}, indicates which one to change (1 for the 1st, 2
#'  for the 2nd, etc.).
#'
#'@param values 
#'  New values for the parameter in \code{pTag}. Single or vector 
#'  of integer or real or character (etc.) value(s).
#'
#'@param type 
#'  Single character string. Parameter category (category's header 
#'  in the PAR file).
#'
#'
#'@return 
#'  Returns vector of \bold{character} strings.
#'
#'
#'@example inst/examples/rmacroliteChange1Param-example.r
#'
#'@export
#'
#'@keywords internal
#'
rmacroliteChange1Param <- function( 
    x, 
    pTag, 
    type = NULL, 
    value, 
    tagNb = NA_integer_ 
){  
    #   Fetch the 1st part of the tag
    pTag1 <- strsplit( x = pTag, split = "%s", fixed = TRUE ) 
    
    pTag2 <- unlist( lapply( X = pTag1, FUN = function(X){ X[2] } ) ) 
    pTag1 <- unlist( lapply( X = pTag1, FUN = function(X){ X[1] } ) ) 
    
    
    
    #   Find rows matching the tag
    sRow <- grepl( x = tolower( x[[ "par" ]][, "parFile" ] ), pattern = tolower( pTag1 ), 
        fixed = TRUE ) 
    
    # & (x[[ "par" ]][, "category" ] == "HEAD")
    if( !is.null( type ) ){ 
        sRow <- sRow & (x[[ "par" ]][, "category" ] == type)
    }   
    
    if( sum( sRow ) == 0 ){ 
        stop( sprintf( 
            "No row found matching the PAR tag (type:'%s', tag:'%s')", 
            ifelse( is.null( type ), "NULL", type ), pTag1 
        ) ) 
    }   
    
    if( sum( sRow ) > 1 ){ 
        #   Refine the parameter search for exact matches
        # pTag1b <- paste0( tolower( pTag1 ), "\t" )
        
        sRowExact <- tolower( pTag1 ) == substr( 
            x     = tolower( x[[ "par" ]][, "parFile" ] ), 
            start = 1, 
            stop  = nchar( pTag1 ) 
        )   
        
        if( !is.null( type ) ){ 
            sRowExact <- sRowExact & (x[[ "par" ]][, "category" ] == type)
        }   
        
        if( sum( sRowExact ) == 0 ){
            sRowExact <- sRow 
        }   
        
        if( sum( sRowExact ) > 1L ){
            if( any( is.na( tagNb ) ) ){ 
                stop( sprintf( 
                    "Found more than one row matching the PAR tag, while 'tagNb' is NA (type:'%s', tag:'%s')", 
                    ifelse( is.null( type ), "NULL", type ), 
                    pTag1 
                ) ) 
                
            }else{ 
                if( ifelse( any( is.na( tagNb ) ), FALSE, max( tagNb ) > sum( sRow ) ) ){ # previous bug sum( sRow ) > max( tagNb ) 
                    stop( sprintf( 
                        "max( tagNb ) is higher than the number of row matching the PAR tag (type:'%s'; tag:'%s'; max(tagNb):%s; nb rows: %s)", 
                        ifelse( is.null( type ), "NULL", type ), 
                        pTag1, 
                        max( tagNb ), 
                        sum( sRow )
                    ) ) 
                    
                }else{ 
                    if( (length( value ) == 1) & (length( tagNb ) > 1) ){ 
                        value <- rep( value, times = length( tagNb ) ) 
                        
                    }else if( length( value ) != length( tagNb ) ){ 
                        stop( sprintf( 
                            "length( tagNb ) and length( value ) differ (type:'%s', tag:'%s')", 
                            ifelse( is.null( type ), "NULL", type ), 
                            pTag1, 
                        ) ) 
                    }   
                    
                    sRow <- which( sRow ) 
                    sRow <- sRow[ tagNb ] 
                }   
            }   
        }else{
            sRow <- which( sRowExact ) 
            # sRow <- sRow[ tagNb ]
        }   
    }   
    
    x[[ "par" ]][ sRow, "parFile" ] <- sprintf( pTag, value ) 
    
    
    
    return( x ) 
}   



# rmacroliteRunId ==========================================

#'Fetch or set the simulation ID (RUNID) of one or more imported MACRO simulation parameter sets
#'
#'Fetch or set the simulation ID (RUNID) of one or more imported 
#'  MACRO simulation parameter sets
#'
#'
#'@param x 
#'  A \code{macroParFile} object, containing one simulations 
#'  whose simulation ID (RUNID) should be fetched or set.
#'
#'@param \dots
#'  Additional parameters passed to specific methods.
#'
#'@param value 
#'  A single or vector of integer. New value(s) for the RUNID
#'
#'
#'@return 
#'  WRITE DESCRIPTION HERE.
#'
#'
#'@example inst/examples/rmacroliteRunId-example.r
#'
#'
#'@rdname rmacroliteRunId-methods
#'@aliases rmacroliteRunId
#'
#'@export 
#'
#'@docType methods
#'
rmacroliteRunId <- function( x, ... ){ 
    UseMethod( "rmacroliteRunId" )
}   



#'@rdname rmacroliteRunId-methods
#'
#'@method rmacroliteRunId macroParFile
#'@export 
rmacroliteRunId.macroParFile <- function( x, ... ){ 
    runId <- rmacroliteGet1Param( x = x, pTag = "RUNID\t%s", type = "HEAD" ) 
    
    return( as.integer( runId ) ) 
}   



#'@rdname rmacroliteRunId-methods
#'
#'@method rmacroliteRunId macroParFileList
#'@export 
rmacroliteRunId.macroParFileList <- function( x, ... ){ 
    runId <- unlist( lapply( 
        X   = x, 
        FUN = function(x){ 
            return( rmacroliteRunId.macroParFile( x = x ) )  
        }   
    ) ) 
    
    return( as.integer( runId ) ) 
}   



#'@rdname rmacroliteRunId-methods
#'
#'@usage rmacroliteRunId( x, ... ) <- value
#'
#'@export
#'
`rmacroliteRunId<-` <- function( x, ..., value ){ 
    UseMethod( "rmacroliteRunId<-" )
}   



#'@rdname rmacroliteRunId-methods
#'
#'@method rmacroliteRunId<- macroParFile
#'@export 
#'
#'@usage \method{rmacroliteRunId}{macroParFile}(x, ...) <- value
#'
`rmacroliteRunId<-.macroParFile` <- function( x, ..., value ){ 
    
    x <- rmacroliteChange1Param( 
        x     = x, 
        pTag  = "RUNID\t%s", 
        type  = "HEAD", 
        value = value ) 
    
    #   Format the RunId with leading 0 (ex 001 instead of 1)
    value0 <- formatC( 
        x     = value, 
        width = getRmlPar( "idWidth" ), 
        flag  = "0" )
    
    x <- rmacroliteChange1Param( 
        x     = x, 
        pTag  = "OUTPUTFILE\t%s", 
        type  = "SETUP", 
        value = sprintf( "macro%s.bin", value0 ) )
    
    #   Change the INFORMATION-section as well, if it exists
    if( "INFORMATION" %in% x[["par"]][, "category" ] ){
        x <- rmacroliteChange1Param( 
            x     = x, 
            pTag  = "Output File = %s", 
            type  = "INFORMATION", 
            value = sprintf( "macro%s.bin", value0 ) )
    }   
    
    return( x ) 
}   



#'@rdname rmacroliteRunId-methods
#'
#'@method rmacroliteRunId<- macroParFileList
#'@export 
#'
#'@usage \method{rmacroliteRunId}{macroParFileList}(x, ...) <- value
#'
`rmacroliteRunId<-.macroParFileList` <- function( x, ..., value ){ 
    if( length(x) != length(value) ){ 
        stop( sprintf(
            "There must be the same number of value(s) in 'value' (%s) than simulations in 'x' (%s)", 
            length(value), 
            length(x)
        ) ) 
    }   
    
    newX <- lapply( 
        X   = 1:length(x), 
        FUN = function(i){ 
            return( rmacroliteRunId( x = x[[ i ]] ) <- value[ i ] )  
        }   
    )   
    
    attributes( newX ) <- attributes( x ) 
    
    class( newX ) <- class( x )
    
    return( newX ) 
}   



# rmacroliteExportParFile =========================================

.rml_set_parfile_name <- function(x,f){
    
    # modelVar <- rmacroliteGetModelVar() 
    # where <- modelVar[[ "path" ]]
    
    if( is.null( f ) ){ 
        
        if( !"list" %in% class(x) ){
            x <- list( x ) 
        }   
        
        idWidth <- getRmlPar( "idWidth" )
        
        fileNameTemplate <- getRmlPar( "fileNameTemplate" )
        
        set_parfile_name0 <- function(x0){
            runId <- rmacroliteRunId( x = x0 ) 
            
            #   Simulation ID with trailing 0
            simId0 <- formatC( x = runId, width = idWidth, 
                flag = "0" )
            
            #   Name of the parameter file to be exported:
            f0 <- sprintf( 
                fileNameTemplate[[ "r" ]], 
                formatC( x = runId, width = idWidth, flag = "0" ), 
                "par" ) 
            
            return( f0 )
        }   
        
        if( ("list" %in% class( x )) ){
            f <- unlist( lapply(
                X   = x, 
                FUN = set_parfile_name0 ) ) 
        }else{
            f <- set_parfile_name0( x0 = x ) 
        }   
        
    }   
    
    f <- .rml_testMacroFilePath( path = f ) 
    
    return( f )
}   


#'Export parameters for one or several MACRO simulations
#'
#'Export parameters for one or several MACRO simulations
#'
#'
#'@seealso \code{\link[rmacrolite:rmacroliteExport-methods]{rmacroliteExport}}.
#'
#'
#'@param x 
#'  A \code{macroParFile} object, containing one simulations 
#'  to be exported
#'
#'@param f 
#'  Single character string. Name of, and optionally path to, 
#'  the par-file where the simulations par-file should be 
#'  written. If \code{NULL}, a name will be attributed using 
#'  the template given by 
#'  \code{\link[rmacrolite]{getRmlPar}("fileNameTemplate")} 
#'  and the RUNID contained in the par-file.
#'
#'@param verbose 
#'  Single integer value. If set to a value \code{< 1}, 
#'  the program is silent (except on errors or warnings). If 
#'  set to \code{1}, the program outputs messages. Values 
#'  \code{> 1} may also activate messages from lower level 
#'  functions (for debugging purpose).
#'
#'@param \dots
#'  Additional parameters passed to specific methods.
#'
#'@return 
#'  WRITE DESCRIPTION HERE.
#'
#'
#'@importFrom macroutils2 macroWriteBin
#'
#'@rdname rmacroliteExportParFile-methods
#'@aliases rmacroliteExportParFile
#'
#'@example inst/examples/rmacroliteExportParFile-example.r
#'
#'@export 
#'
#'@docType methods
#'
rmacroliteExportParFile <- function( 
    x, 
    f = NULL, 
    verbose = 1L, 
    ... 
){ 
    UseMethod( "rmacroliteExportParFile" )
}   



#'@rdname rmacroliteExportParFile-methods
#'
#'@method rmacroliteExportParFile macroParFile
#'@export 
#'
#'@importFrom utils write.table
#'@importFrom tools showNonASCII
rmacroliteExportParFile.macroParFile <- function(
    x, 
    f = NULL, 
    verbose = 1L, 
    ... 
 ){ 
    f <- .rml_set_parfile_name( x = x, f = f ) 
    
    # EXPORT THE PARAMETER FILE
    # ===========================================================
    
    log_width <- getRmlPar( "log_width" ) 
    
    .rml_logMessage( m = "* Exporting the par-file to: %s", 
        values = list( f ), verbose = verbose, 
        log_width = log_width ) 
    
    encoding1 <- getRmlPar( "encoding" )[1L]
    encoding2 <- getRmlPar( "encoding" )[2L]
    
    
    
    encoding_is_utf8_bom <- tolower( encoding2 ) == "utf-8-bom"
    encoding2 <- "UTF-8"
    
    f_con <- file( description = f, open = "wt", 
        encoding = encoding2 ) 
    
    on.exit( close( f_con ) ) 

    if( encoding_is_utf8_bom ){  
        writeChar(iconv( "\ufeff", to = ifelse( 
            test = tolower( encoding1 ) == "utf-8-bom", 
            yes  = "UTF-8", 
            no   = encoding1 ) ), 
            con = f_con, eos = NULL )
    }   
    
    writeLines( text = x[[ "par" ]][, "parFile" ], 
        con = f_con )
    
    close( f_con ); on.exit( NULL )
    rm( encoding_is_utf8_bom, f_con )
    
    
    
    # ====== Final output =======================================
    out <- list( "f" = f, "macroParFile" = x ) 
    
    return( invisible( out ) ) 
}   

    # pr <- rmacroliteImportParFile( 
        # file = "D:/Users/username/Documents/_WORKS/_PROJECTS/r_packages/perform/pkg/rmacro/inst/test.par" ) 
    
    # rmacroliteRunId( pr ) 
    # rmacroliteRunId( pr ) <- 1L 
    
    # out <- rmacroliteExportParFile( x = pr )



# rmacroliteChangeParam ==============================================

# ## \code{tagNb} is an integer 
# ## value. If several rows have the same \code{pTag}, indicates 
# ## which one to change (1 for the 1st, 2 for the 2nd, etc.). 
# ## \code{values} is the new value for the parameter in 
# ## \code{pTag}. Single or vector of integer or real or character 
# ## (etc.) value(s). 

#'Change parameters in a MACRO simulation 
#'
#'Change parameters in a MACRO simulation, one by one or 
#'  several at a time, and generate a new list of MACRO Simulation 
#'  parameters.
#'
#'
#'@param x 
#'  A \code{macroParFile} object, containing one simulations 
#'  whose parameters should be changed.
#'
#'@param p 
#'  A \code{\link[base]{data.frame}} with one row per parameter 
#'  (value), with the following columns: 
#'  \itemize{ 
#'      \item \code{tag}: a text (character) string containing the 
#'          parameter value that should be searched and replaced in the 
#'          PAR file, with the parameter value replaced by the string 
#'          \code{\%s}. NOTE: CASE SENSITIVE!
#'      \item \code{values}: The new value of the parameter (one 
#'          value per row in \code{p}).
#'      \item \code{type}: a character string. Parameter category 
#'          (category's header in the PAR file).
#'      \item \code{set_id}: the simulation identifier: all 
#'          parameters (rows) that have the same \code{set_id} will 
#'          be changed simultaneously, while parameters (rows) that 
#'          have a different \code{set_id} will be in different 
#'          simulations (parameter sets). If \code{set_id} is 
#'          missing, it is assumed that all rows in \code{p} are 
#'          different simulations (they will be attributed different 
#'          \code{set_id}). Notice that \code{set_id} is not the 
#'          RUNID and the RUNID in the simulation will 
#'          therefore not be changed to the value of \code{set_id}.
#'      \item \code{tagNb} (optional): an integer values. If several 
#'          rows have the same \code{pTag} (without an index), 
#'          indicates which one to change (1 for the 1st, 2 for 
#'          the 2nd, etc.). Typically needed for some irrigation 
#'          parameters in MACRO.
#'  }   
#'
#'@param \dots
#'  Additional parameters passed to specific methods.
#'
#'
#'@return 
#'  Returns a list of \code{macroParFile} objects, with class 
#'  \code{macroParFileList}.
#'
#'
#'@example inst/examples/rmacroliteChangeParam-example.r
#'
#'
#'@rdname rmacroliteChangeParam-methods
#'@aliases rmacroliteChangeParam
#'
#'@export 
#'
#'@docType methods
#'
rmacroliteChangeParam <- function( x, p, ... ){ 
    UseMethod( "rmacroliteChangeParam" )
}   


#'@rdname rmacroliteChangeParam-methods
#'
#'@method rmacroliteChangeParam macroParFile
#'@export 
rmacroliteChangeParam.macroParFile <- function( x, p, ... ){ 
    
    # ====== Test that the parameter table is conform ===========
    if( !"data.frame" %in% class( p ) ){
        stop( sprintf( "Argument 'p' should be a data.frame (now class: %s)", 
            paste( class( p ), collapse = ", " ) ) )
    }   
    
    pCol      <- colnames( p ) 
    expectCol <- c( "tag", "values", "type" ) 
    testCol   <- expectCol %in% pCol 
    if( any( !testCol ) ){ 
        stop( sprintf( "Some columns in 'p' are missing (%s)", 
            paste( expectCol[ !testCol ], collapse = ", " ) ) )
    }   
    
    if( !("set_id" %in% pCol) ){ 
        p[, "set_id" ] <- 1:nrow( p ) 
    }   
    
    if( !("tagNb" %in% pCol) ){ 
        p[, "tagNb" ] <- NA_integer_ 
    }   
    
    p <- p[, c( "set_id", expectCol, "tagNb" ) ] 
    rm( expectCol, testCol )
    
    
    if( is.factor( p[, "tag" ] ) ){ 
        p[, "tag" ] <- as.character( p[, "tag" ] ) 
    }   
    
    
    # ====== List runs & loop over them =========================
    uniqueRunId <- unique( p[, "set_id" ] ) 
    
    
    parList <- lapply( 
        X   = uniqueRunId, 
        FUN = function(.set_id){ 
            # set_id <- 1
            x0 <- x 
            
            #   Select only the relevant set_id
            p2 <- subset( x = p, subset = eval( quote( set_id == .set_id ) ) ) 
            
            #   Loop over the parameter variations
            for( i in 1:nrow( p2 ) ){ 
                # i <- 1
                
                x0 <- rmacroliteChange1Param( 
                    x     = x0, 
                    pTag  = p2[ i, "tag" ], 
                    type  = p2[ i, "type" ], 
                    value = p2[ i, "values" ], 
                    tagNb = p2[ i, "tagNb" ]  
                )   
            }   
            
            return( x0 ) 
        }   
    )   
    
    names( parList ) <- as.character( uniqueRunId ) 
    
    class( parList ) <- "macroParFileList"
    
    attr( x = parList, which = "parameterTable" ) <- p 
    
    return( invisible( parList ) ) 
}   



# rmacroliteRun ======================================================

# rmacroliteSystemCheck

## # INTERNAL: Search the output of a call to system2() for error message.
## #
## # Internal use only. This function is used to check that there 
## #  was no error message outputed by Windows shell or any command 
## #  called from the shell during a call with 'shell()'. If an 
## #  error is detected (i.e. if the shell output contains the word 
## #  'error'), a file is outputted that contains the full message 
## #  of the shell and the function is stopped. All characters in 
## #  'getRmlPar("errorKeywords")' or 'shellRes' are converted to 
## #  lowercase before error are searched (so the function is not case 
## #  sensitive). See getRmlPar("optionname") to get the (real) default 
## #  values of the options that are set to NULL.
## # 
## # 
## #@param shellRes 
## #  Vector of character strings. Outputted by shell() as the result 
## #  of a shell command call from R.
## # 
## #@param shellDir  
## #  Path of the folder in which the shell output must be written 
## #  in case errors are found.
## # 
## #
## #@return
## #  The function does not returns anything, but stop if an error 
## #  is detected.
## #
## # 
## #@export 
## #
rmacroliteSystemCheck <- function( 
    shellRes,  
    shellDir = getwd() 
){  
    # Convert the shell output to lowercase:
    shellRes2 <- tolower( shellRes )
    
    # Search the outputted message for the word 'error' 
    # in order to catch eventual errors!
    catch.err <- unlist( 
        lapply( 
            X   = tolower( getRmlPar( "errorKeywords" ) ), 
            FUN = function(X){ 
                length( 
                    grep( 
                        pattern = X, 
                        x       = shellRes2 
                    )   
                )   
            }   
        )   
    )   
    
    # Stops and returns an error if an error was detected
    if( any( catch.err ) != 0 ){   
        errorFileName <- file.path( 
            normalizePath( shellDir ), 
            "SHELL_OUTPUT_LOG_ERROR.TXT" ) 
        
        writeLines( 
            text = shellRes, 
            con  = errorFileName, 
            sep  = "\n"  
        )   
        
        stop( 
            paste( 
                sep = "", 
                "Something probably went wrong when running a shell command.\n", 
                "  Some error kewords (see 'getRmlPar(\"errorKeywords\")') were detected in the shell output.\n", 
                "  Shell log file saved in ", errorFileName, "\n"
            )   
        )   
    }   
}   



#'Run one or several MACRO Simulation(s)
#'
#'Run one or several MACRO Simulation(s)
#'
#'
#'@param x 
#'  A \code{macroParFile}- or a \code{macroParFileList}-object, 
#'  containing one or several simulations to be ran. Alternatively, 
#'  a single character string giving the name of and optionally 
#'  the path to a par-file to be imported and simulated. When 
#'  \code{x} is a single character string and \code{export} 
#'  is \code{FALSE}, \code{f} should be \code{NULL} or identical 
#'  to \code{x}, for consistency. When \code{x} is a single 
#'  character string and \code{export} is \code{TRUE}, 
#'  \code{f} should be \code{NULL} or set 
#'  to a name different than the one in \code{x}, in order 
#'  not to overwrite the original file. 
#'
#'@param f 
#'  Single character string. When \code{export} is \code{TRUE}, 
#'  name (without path) of the par-file where the simulations 
#'  parameters should be written. 
#'  If \code{NULL}, a name will be attributed using 
#'  the template given by 
#'  \code{\link[rmacrolite]{getRmlPar}("fileNameTemplate")} 
#'  and the RUNID contained in the par-file. When \code{export} 
#'  is \code{FALSE}, name (without path) of an existing par-file 
#'  that should be used to run the simulation. In all cases 
#'  the location of the par-file is the directory in which 
#'  MACRO is installed, as given by 
#'  \code{\link[rmacrolite]{rmacroliteGetModelVar}()[["path"]]}.
#'
#'@param export
#'  Single logical value. If \code{TRUE} (the default), the 
#'  par-file is exported prior to the simulation. If 
#'  \code{FALSE}, it is assumed that the par-file to be simulated 
#'  already exists and its name and optionally location is 
#'  indicated by 
#'
#'@param verbose
#'  Single integer value. If set to a value \code{< 1}, 
#'  the program is silent (except on errors or warnings). If 
#'  set to \code{1}, the program outputs messages. Values 
#'  \code{> 1} may also activate messages from lower level 
#'  functions (for debugging purpose).
#'
#'@param indump
#'  Single logical value. If \code{TRUE} (the default), 
#'  the so called \code{indump.tmp} parameter file is produced. 
#'  Must be \code{TRUE} when \code{run} is \code{TRUE}.
#'
#'@param run
#'  Single logical value. If \code{TRUE} (the default), the 
#'  simulation is run. If \code{FALSE}, only the par-file is 
#'  exported (when \code{export} is \code{TRUE}) but the 
#'  simulation is not run.
#'
#'
#'@param rename
#'  Single logical value. If \code{TRUE}, the bin-file output 
#'  by MACRO is renamed automatically, using the template 
#'  in \code{\link[rmacrolite]{getRmlPar}("fileNameTemplate")}.
#'  If \code{FALSE} (the default), the bin-file output 
#'  by MACRO is not renamed.
#'
#'@param \dots
#'  Additional parameters passed to specific methods.
#'
#'
#'@return 
#'  Returns a \code{\link{data.frame}} with the simulation results, 
#'  a column \code{date} and if relevant a column \code{runId}.
#'
#'
#'@importFrom macroutils2 getMuPar
#'
#'@example inst/examples/rmacroliteRun-example.r
#'
#'@rdname rmacroliteRun-methods
#'@aliases rmacroliteRun
#'
#'@export 
#'
#'@docType methods
#'
rmacroliteRun <- function(
    x, 
    f = NULL, 
    export = TRUE, 
    verbose = 1L, 
    ... 
){ 
    .rml_logMessage( m = "Check input arguments", 
        verbose = verbose, values = list( x ) ) 
    
    x_is_character <- is.character( x )
    
    f_is_null <- is.null( f ) 
    
    if(       export & x_is_character & f_is_null ){
        # OK
        
    }else if( export & x_is_character & (!f_is_null) ){
        if( normalizePath( x, mustWork = FALSE ) == normalizePath( f, mustWork = FALSE ) ){
            stop( "'f' is equal or equivalent to 'x'. The imported par-file shall not be overwritten." )
        }   
        
    }else if( export & (!x_is_character) & f_is_null ){
        # OK, the name will be defined internally
        
    }else if( export & (!x_is_character) & (!f_is_null) ){
        # OK
        
    }else if( (!export) & x_is_character & f_is_null ){
        # OK
        f <- x 
        
    }else if( (!export) & x_is_character & (!f_is_null) ){
        stop( "When 'export' is FALSE and 'x' a character string, 'f' must be NULL." )
        
    }else if( (!export) & (!x_is_character) ){
        stop( "When 'export' is FALSE, 'x' must be a character string." )
        
    }   
    
    
    UseMethod( "rmacroliteRun" )
}   



#'@rdname rmacroliteRun-methods
#'
#'@method rmacroliteRun character
#'
#'@export 
#'
rmacroliteRun.character <- function( 
    x, 
    f = NULL, 
    export = TRUE, 
    verbose = 1L, 
    ... 
){  
    log_width <- getRmlPar( "log_width" )
    
    .rml_logMessage( m = "Importing the par-file: %s", 
        verbose = verbose, log_width = log_width, 
        values = list( x ) ) 
    
    x_original <- x 
    
    if( is.null( f ) & (!export) ){
        f <- x_original
    }   
    
    n <- length( x_original )
    
    x <- rmacroliteImportParFile( file = x_original, 
        verbose = verbose - 1L )
    
    if( !is.null( f ) ){
        if( n != length(f) ){
            stop( sprintf( 
                "'x' and 'f' should have the same length." ) )
        }   
        
        if( any( duplicated( f ) ) ){
            stop( sprintf( 
                "Some values in 'f' are duplicated: %s", 
                paste( f[ duplicated( f ) ], collapse = "; " ) ) )
        }   
    }else{
        if( export ){
            f <- .rml_set_parfile_name( x = x, f = f ) 
            
            if( any( duplicated( f ) ) ){
                stop( sprintf( 
                    "Some values attributed to 'f' are duplicated, probably because of duplicated RUNID in 'x': %s", 
                    paste( f[ duplicated( f ) ], collapse = "; " ) ) )
            }   
        }   
    }   
    
    if( export & (!is.null( f )) ){
        test_x_f <- normalizePath(x_original,mustWork=FALSE) == 
            normalizePath(f,mustWork=FALSE)
        
        if( any( test_x_f ) ){
            stop( sprintf( 
                "When a character string, 'x' shall not be equal to 'f': %s", 
                paste(f[test_x_f],collapse="; ") ) )
        }   
    }   
    
    if( n <= 1L ){
        return( rmacroliteRun.macroParFile( x = x, 
            f = f, export = export, verbose = verbose, ... ) )
        
    }else{
        return( rmacroliteRun.macroParFileList( x = x, 
            f = f, export = export, verbose = verbose, ... ) )
    }      
}   



#'@rdname rmacroliteRun-methods
#'
#'@method rmacroliteRun macroParFile
#'
#'@export 
#'
#'@importFrom utils compareVersion
#'@importFrom utils read.table
#'@importFrom utils write.table
rmacroliteRun.macroParFile <- function( 
    x, 
    f = NULL, 
    export = TRUE, 
    verbose = 1L, 
    indump = TRUE, 
    run = TRUE, 
    rename = FALSE, 
    ... 
){  
    timeStart <- Sys.time() 
    # verbose <- getRmlPar( "verbose" ) 
    log_width <- getRmlPar( "log_width" )
    
    modelVar <- rmacroliteGetModelVar()
        
    where <- modelVar[[ "path" ]]
    
    #   Test the path length
    where <- .rml_testMacroFilePath( path = where )
    
    if( is.null( f ) ){
        f <- .rml_set_parfile_name( x = x, f = f )
        
        f <- file.path( where, f )
    }   
    
    
    #   Check the system setting
    # .rml_testDateFormat( verbose = verbose )
    # .rml_testDecimalSymbol() 
    
    
    
    # EXPORT THE SIMULATION PARAMETER AND FILES
    # ===========================================================
    if( export ){ 
        .rml_logMessage( m = "Exporting MACRO parameters.", 
            verbose = verbose, log_width = log_width ) 
        
        rmacroliteExportParFile( x = x, f = f, 
            verbose = verbose - 1L ) 
    }   
    
    
    
    # RUN MACRO
    # ===========================================================
    
    if( run & !indump ){
        stop("Argument 'indump' must be TRUE when 'run' is TRUE.")
    }   
    
    if( indump ){
        f_without_path <- strsplit( x = normalizePath( f, 
            mustWork = FALSE ), split = "\\\\" )[[ 1L ]]
        f_without_path <- f_without_path[ length( f_without_path ) ]
        
        .args <- sprintf( "%s %s", f_without_path, "/r" ) 
        rm( f_without_path )
        
        macro.exe <- modelVar[[ "exe" ]]
        
        macro.exeparfile <- modelVar[[ "exeparfile" ]]
        
        .rml_logMessage( m = "Exporting the indump.tmp (parameter file)", 
            verbose = verbose, log_width = log_width ) 
        
        oldWd <- getwd(); setwd( where ); on.exit( setwd( oldWd ) )
        
        exeparOutputMessage <- system2(
            command = macro.exeparfile, 
            args    = .args,
            stdout  = TRUE, 
            stderr  = TRUE ) 
        
        #   Check for errors in MACRO run
        rmacroliteSystemCheck( 
            shellRes = exeparOutputMessage,  
            shellDir = where 
        )   
        
        if( run ){
            
            .rml_logMessage( m = "Run the simulation", 
                verbose = verbose, log_width = log_width ) 
            
            timeStart2 <- Sys.time() 
            
            macroOutputMessage <- system2(
                command = macro.exe, 
                # args    = character(),
                stdout  = TRUE, 
                stderr  = TRUE ) 
            
            timeEnd2  <- Sys.time() 
            
            setwd( oldWd ); on.exit( NULL )
            
            #   Check for errors in MACRO run
            rmacroliteSystemCheck( 
                shellRes = macroOutputMessage,  
                shellDir = where 
            )   
            
            
            
            runId <- rmacroliteRunId( x = x ) 
            
            .width <- getRmlPar( "idWidth" )
            
            resFileName <- file.path( 
                where, 
                sprintf( 
                    getRmlPar( "fileNameTemplate" )[[ "macro" ]], 
                    formatC( runId, width = .width, flag = "0" ), 
                    "BIN" ) ) 
            
            if( rename ){
                resFileNewName <- file.path( 
                    where, 
                    sprintf( 
                        getRmlPar( "fileNameTemplate" )[[ "r" ]], 
                        formatC( runId, width = .width, flag = "0" ), 
                        "bin" ) ) 
                rm( .width )
                
                
                
                .rml_logMessage( m = "Renaming MACRO simulation output", 
                    verbose = verbose, log_width = log_width ) 
                
                if( !file.rename( from = resFileName, to = resFileNewName ) ){
                    warning( sprintf(
                        "Unable to rename the file (from %s to %s)", 
                        resFileName, 
                        resFileNewName 
                    ) ) 
                    
                    resFileNewName <- resFileName
                }   
                
                rm( resFileName )
            }else{
                resFileNewName <- resFileName
            }   
            
            
            
            .rml_logMessage( m = "Importing MACRO simulation results", 
                verbose = verbose, log_width = log_width ) 
            
            simRes <- macroReadBin( f = resFileNewName )
            
            attr( x = simRes, which = "macroOutputMessage" ) <- macroOutputMessage 
            
            
            
            .rml_logMessage( m = "Importing MACRO water and solute balance result", 
                verbose = verbose, log_width = log_width ) 
            
            balanceFile <- file.path( where, getRmlPar( "balanceFile" ) )
            #   "balance.txt"
            
            balanceRes <- readLines( con = balanceFile, n = 2L ) 
            balanceRes <- as.numeric( balanceRes ) # text -> numeric
            names( balanceRes ) <- c( "waterBalanceFInput", "soluteBalanceFApplied" ) 
            
            attr( x = simRes, which = "waterSoluteBalance" ) <- balanceRes 
            
            
            
            class( simRes ) <- c( "macroSimResults", "macroTimeSeries", "data.frame" )
        }else{
            simRes <- f 
        }   
    }else{
        simRes <- f
    }   
    
    # timeStart <- Sys.time(); Sys.sleep(1L)
    timeEnd  <- Sys.time() 
    
    duration  <- as.numeric( difftime( timeEnd,  timeStart,  units = "mins" ) ) 
    
    if( run ){
        duration2 <- as.numeric( difftime( timeEnd2, timeStart2, units = "mins" ) ) 
        
        .rml_logMessage( m = "MACRO runtime %s min; total runtime %s min", 
            verbose = verbose, log_width = log_width, 
            values = list( round( duration2, 3 ), 
            round( duration, 3 ) ) )
        
        attr( x = simRes, which = "macro_runtime" ) <- duration2 
    }else{
        .rml_logMessage( m = "Total runtime %s min", 
            verbose = verbose, log_width = log_width, 
            values = list( round( duration, 3 ) ) )
        
        attr( x = simRes, which = "macro_runtime" ) <- NA_real_ 
    }   
    
    return( simRes )
}   



.rmacroliteRunEH <- function( 
    x, 
    ... 
){  
    #   Expression to be evaluated with error handling
    myExpr <- expression( { out <- rmacroliteRun( x = x, ... ) } )
    
    
    # Run the code, with error handling
    catchRes <- tryCatch( 
        expr = eval( myExpr ), 
        
        # What to do with an eventual error message catched (theError)?
        error = function(theError){ 
            theError # just return it.
        }   
    )   
    
    # Test if an error was found
    testError <- any( class(catchRes) %in% 
        c( "simpleError", "error", "condition" ) ) 
    
    # If an error was found, give a message
    if( testError ){
        warning( catchRes ) 
        
        out <- data.frame() 
        
        attr( x = out, which = "error" ) <- TRUE 
        attr( x = out, which = "tryCatch" ) <- catchRes 
    }else{ 
        attr( x = out, which = "error" ) <- FALSE 
        attr( x = out, which = "tryCatch" ) <- NULL   
    }   
    
    
    return( out )
}   



#'@rdname rmacroliteRun-methods
#'
#'@method rmacroliteRun macroParFileList
#'@export 
rmacroliteRun.macroParFileList <- function( 
    x, 
    f = NULL, 
    export = TRUE, 
    verbose = 1L, 
    ... 
){  
    timeStart <- Sys.time() 
    # verbose <- getRmlPar( "verbose" )
    handleErrors <- getRmlPar( "handleErrors" )
    log_width <- getRmlPar( "log_width" )
    
    if( handleErrors ){ 
        .rml_logMessage( m = "Error handling is ON (see getRmlPar( \"handleErrors\" ))", 
            verbose = verbose, log_width = log_width ) 
        
        macroFun <- .rmacroliteRunEH 
        
    }else{ 
        .rml_logMessage( m = "Error handling is OFF (see getRmlPar( \"handleErrors\" ))", 
            verbose = verbose, log_width = log_width ) 
        
        macroFun <- rmacroliteRun
    }   
    
    
    #   If the parameters don't have names, attribute names
    if( is.null( names( x ) ) ){ 
        names( x ) <- as.character( 1:length(x) ) 
    }   
    
    #   Name of the files in which the simulations are saved
    rdsFileNames <- sprintf( 
        "rmacroSimRes%s-%s.rds", 
        format( Sys.time(), "%Y-%m-%d_%H%M%S" ), 
        # Time flag is important, otherwise it will read old simultions!
        names( x ) ) 
    
    
    .rml_logMessage( m = "Running macroParFileList (%s simulations)", 
        verbose = verbose, log_width = log_width, 
        values = list( length(x) ) ) 
    
    #   Run the simulations and save them into files
    error <- lapply(
        X   = 1:length(x), 
        FUN = function(i){ 
            out <- macroFun( x = x[[ i ]], ... ) 
            
            saveRDS( object = out, file = rdsFileNames[ i ] ) 
            
            return( attr( x = out, which = "error" ) )
        }   
    )   
    
    
    .rml_logMessage( m = "Re-importing simulation results", 
        verbose = verbose, log_width = log_width ) 
    
    #   Re-import the simulation results
    simRes <- lapply(
        X   = 1:length(x), 
        FUN = function(i){ 
            out <- readRDS( file = rdsFileNames[ i ] ) 
            
            return( out )
        }   
    )   
    
    names( simRes ) <- names( x )
    class( simRes ) <- c( "macroSimResultsList", "macroTimeSeriesList", "list" )
    
    
    # timeStart <- Sys.time(); Sys.sleep(1L)
    timeEnd  <- Sys.time() 
    duration <- as.numeric( difftime( timeEnd, timeStart, units = "mins" ) ) 
    
    .rml_logMessage( m = "TOTAL run-time: %s min", 
        verbose = verbose, log_width = log_width, 
        values = list( round( duration, 3 ) ) )
    
    
    return( simRes )
}   



# rmacroliteExport ===================================================

#' Export MACRO parameter sets or MACRO simulation results
#'
#' Export MACRO parameter sets or MACRO simulation results
#'
#'
#'@seealso \code{\link[rmacrolite:rmacroliteExportParFile-methods]{rmacroliteExportParFile}}.
#'
#'
#'@param x 
#'  A \code{macroSimResults}, \code{macroSimResultsList} or 
#'  \code{macroParFile}-object, 
#'  containing one (or several) simulation parameter set(s) or 
#'  result(s). \code{macroParFileList} are not supported yet.
#'
#'@param where 
#'  Single character string. If \code{x} is a 
#'  \code{macroParFile}-object, absolute or relative path 
#'  of the folder in which the simulation-parameter(s) should be 
#'  written. If \code{where} is \code{NULL}, a temporary working 
#'  directory will be created where the simulations will be written. 
#'  If \code{x} is a \code{macroSimResults} \code{macroSimResultsList}, 
#'  absolute or relative path AND file name prefix (without 
#'  extension) of the file in which the simulations shall be 
#'  exported.
#'
#'@param overwrite
#'  Single logical value. If \code{TRUE}, will overwrite the files 
#'  if they already exist.
#'
#'@param \dots
#'  Additional parameters passed to \code{\link[utils:write.table]{write.csv}} 
#'  or \code{\link[rmacrolite:rmacroliteExportParFile-methods]{rmacroliteExportParFile}}.
#'
#'
#'@return 
#'  TO DO: COMPLETE THIS.
#'
#'
#'@rdname rmacroliteExport-methods
#'@aliases rmacroliteExport
#'
#'@export 
#'
#'@docType methods
#'
rmacroliteExport <- function( x, where = NULL, ... ){ 
    UseMethod( "rmacroliteExport" )
}   



#'@rdname rmacroliteExport-methods
#'
#'@method rmacroliteExport macroParFile
#'@export 
rmacroliteExport.macroParFile <- function( 
    x, 
    where = NULL, 
    ...
){  
    # fun <- getAnywhere( "rmacroliteExportParFile.macroParFile" ) 
    
    return( rmacroliteExportParFile( x = x, f = where, ... ) )
}   



#'@rdname rmacroliteExport-methods
#'
#'@method rmacroliteExport macroSimResults
#'@export 
#'
#'@importFrom utils write.csv
rmacroliteExport.macroSimResults <- function( 
    x, 
    where = NULL, 
    overwrite = FALSE, 
    ...
){  
    if( is.null( where ) ){ 
        stop( "Parameter 'where' is NULL (file name prefix)" ) 
        
    }else if( length( where ) != 1 ){ 
        stop( "'where' must be length 1" ) 
    }   
    
    where <- paste0( where, ".csv" ) 
    
    testF <- file.exists( where )
    
    if( testF & !overwrite ){ 
        stop( sprintf( "The file 'where' (%s) already exists", where ), 
            "\n  Consider setting 'overwrite' to TRUE" ) 
    };  rm( testF )
    
    out <- utils::write.csv( x = unclass( x ), file = where, 
        row.names = FALSE, ... ) 
    
    return( invisible( out ) ) 
}   



#'@rdname rmacroliteExport-methods
#'
#'@method rmacroliteExport macroSimResultsList
#'@export 
#'
#'@importFrom utils write.csv
rmacroliteExport.macroSimResultsList <- function( 
    x, 
    where = NULL, 
    overwrite = FALSE, 
    ...
){  
    if( is.null( where ) ){ 
        stop( "Parameter 'where' is NULL (file name prefix)" ) 
        
    }else if( length( where ) != 1 ){ 
        stop( "'where' must be length 1" ) 
    }   
    
    
    nb <- formatC(
        x     = 1:length(x),
        flag  = "0",
        width = max( nchar( 1:length(x) ) ) ) 
    
    
    where <- sprintf( "%s_%s.csv", where, nb ) 
    
    testF <- file.exists( where )
    
    if( any( testF ) & !overwrite ){ 
        stop( sprintf( 
            "Some files in 'where' already exist (%s)", 
            paste( where[ testF ], collapse = "; " ) ), 
            "\n  Consider setting 'overwrite' to TRUE" ) 
    };  rm( testF )
    
    
    out <- lapply( 
            X   = 1:length(x), 
            FUN = function(i){ 
                out <- utils::write.csv( 
                    x         = x[[ i ]], 
                    file      = where[ i ], 
                    row.names = FALSE, 
                    ... ) 
                
                return( out )
        }   
    )   
    
    
    return( invisible( out ) ) 
}   



# ==================== .rml_testDateFormat ==========================

# ## # Find out what the date format is on the host computer
# ## # 
# ## # Find out what the date format is on the host computer, 
# ## #    and set the corresponding rmacro option (see timeFormat 
# ## #    in \code{\link{rmlPar}})
# ## # 
# ## # 
# ## #@param error 
# ## #    If \code{TRUE}, an error is generated if the date format 
# ## #    could not be defined. Otherwise gives a warning.
# ## #
# .rml_testDateFormat <- function( 
    # error     = FALSE, 
    # verbose   = 1L, 
    # log_width = 60L 
# ){  
    # # Get the time, with milliseconds
    # sysdate <- shell( cmd = "echo %DATE%", intern = TRUE )[ 1L ] 
    # systime <- shell( cmd = "echo %TIME%", intern = TRUE )[ 1L ] 
    
    # #   Identify the date separator
    # if( grepl( x = sysdate, pattern = "-", fixed = TRUE ) ){ 
        # sep <- "-" 
        
    # }else if( grepl( x = sysdate, pattern = "/", fixed = TRUE ) ){ 
        # sep <- "/" 
        
    # }else if( grepl( x = sysdate, pattern = ".", fixed = TRUE ) ){ 
        # sep <- "." 
        
    # }else{ 
        # msg <- sprintf( 
            # "The system date-format (separator) could not be defined. 'echo %sDATE%s' returned %s", 
            # "%", "%s", sysdate ) 
        
        # if( error ){ stop( msg ) }else{ warning( msg ) }   
    # }   
    
    
    # #   Identify if year comes first or last
    # sSysdate <- strsplit( x = sysdate, split = sep, fixed = TRUE )[[ 1L ]]
    
    # if( length( sSysdate ) != 3 ){ 
        # msg <- sprintf( 
            # "The system date-format could not be defined (split error). 'echo %sDATE%s' returned %s", 
            # "%", "%s", sysdate ) 
        
        # if( error ){ stop( msg ) }else{ warning( msg ) } 
        
    # }else if( nchar( sSysdate[ 1L ] ) == 4L ){ 
        # yearFirst <- TRUE 
        
    # }else if( nchar( sSysdate[ 3L ] ) == 4L ){ 
        # yearFirst <- FALSE 
        
    # }else{ 
        # msg <- sprintf( 
            # "The system date-format could not be defined (position of the year). 'echo %sDATE%s' returned %s", 
            # "%", "%s", sysdate ) 
        
        # if( error ){ stop( msg ) }else{ warning( msg ) } 
    # }   
    
    
    # #   Check the time separator
    # if( !grepl( x = systime, pattern = ":", fixed = TRUE ) ){ 
        # msg <- sprintf( 
            # "The system time-format could not be defined (time separator is not ':'). 'echo %sTIME%s' returned %s", 
            # "%", "%s", systime ) 
        
        # if( error ){ stop( msg ) }else{ warning( msg ) } 
    # }
    
    
    # #   Prepare the date format and set the package option
    # if( yearFirst ){ 
        # timeFormat <- sprintf(  # Year comes 1st
            # "%s%s%s%s%s %s:%s", 
            # "%Y", sep, "%m", sep, "%d", "%H", "%M"
        # )   
    # }else{ 
        # timeFormat <- sprintf(  # Year comes last
            # "%s%s%s%s%s %s:%s", 
            # "%d", sep, "%m", sep, "%Y", "%H", "%M"
        # )   
    # }   
    
    # # rmlPar( "timeFormat" = timeFormat ) 
    
    # .rml_logMessage( 
        # "System date-time format identified as %s", 
        # verbose = verbose, 
        # values = list( timeFormat ), 
        # log_width = log_width
    # ) 
    
    # return( invisible( timeFormat ) ) 
# }   



# # .rml_testDecimalSymbol =======================================

# ## # Test if the system decimal symbol is correctly set 
# ## # 
# ## # Test if the system decimal symbol is correctly set, and 
# ## #    gives a warning or an error otherwise
# ## # 
# ## # 
# ## #@param error 
# ## #    If \code{TRUE}, an error is generated if the decimal 
# ## #    sysmbol is not a point (as needed). Otherwise gives 
# ## #    a warning.
# ## #
# .rml_testDecimalSymbol <- function( error = FALSE ){ 
    # # Get the time, with milliseconds
    # decSym <- shell( cmd = "echo %TIME%", intern = TRUE ) 
    
    # #   If the command is run while R working directory 
    # #   is in a network folder, Windows command prompt adds 
    # #   extra comment-lines to complain and only put the 
    # #   output at the end of the multi-line output.
    # decSym <- decSym[ decSym != "" ] 
    # decSym <- decSym[ length( decSym ) ] 
    
    # decSym <- substr( x = decSym, start = 9, stop = 9 ) 
    
    # if( decSym == "," ){ 
        # msg <- sprintf( 
            # "The system decimal symbol should be '.' (currently '%s')", 
            # decSym ) 
        
        # if( error ){ 
            # stop( msg )
        # }else{ 
            # warning( msg ) 
        # }   
        
    # }else if( decSym != "." ){ 
        # warning( "Could not determine the system decimal symbol. Make sure it is a '.'" ) 
    # }   
# }   



# rmacroliteSorption =======================================

#' Fetch or set normalised Freundlich adsorption coefficients (Kfoc, Nf) in an imported MACRO par-file
#'
#' Fetch or set normalised Freundlich adsorption coefficients 
#'  (Kfoc, Nf) in an imported MACRO par-file. The function 
#'  fetches both the Kf (non-normalised coefficient; ZKD in 
#'  MACRO par-files) and the percentage of organic carbon 
#'  (ORGC in MACRO par-files), recalculates the Kfoc from 
#'  these values and fetches the Freundlich exponent Nf 
#'  (also known as 1/n exponent; FREUND in MACRO par-files).
#'
#'
#'@param x
#'  A \code{macroParFile}, as imported with 
#'  \code{\link[rmacrolite]{rmacroliteImportParFile-methods}}
#'
#'@param value
#'  Vector of two numeric-value, optionally with the named 
#'  "kfoc" and "nf" (observe the lowercase). 
#'  Value of the Kfoc-coefficient and Nf-exponent to be set 
#'  in the par-file \code{x}, in [m3/kg] and [-], respectively.
#'
#'@param \dots
#'  Additional parameters passed to specific methods. 
#'  Currently not used.
#' 
#'
#'@return
#'  Returns a list with two items, \code{"layer"} 
#'  (layer-specific parameters) and \code{"site"} 
#'  (site-specific parameters, i.e. parameters that do 
#'  not vary with depth or with the crop). \code{"layer"} is 
#   a \code{\link[base]{data.frame}} with the following 
#'  columns \code{layer_no}, \code{oc_pc}, \code{kf}, 
#'  \code{kfoc}, \code{nf}: the layer number, 
#'  Kf coefficient [m3/kg], the percentage of organic carbon 
#'  [% dry mass] , the recalculated Kfoc coefficient [m3/kg], 
#'  the Nf exponent [-], respectively. 
#'  \code{"site"} is a vector of named numeric-values, with 
#'  the following item: \code{koc}, the the site-wide 
#'  non-recalculated kfoc (KOC in MACRO par-files), 
#'  respectively.
#' 
#'
#'@example inst/examples/rmacroliteSorption-example.r
#' 
#'
#'@rdname rmacroliteSorption-methods
#'@aliases rmacroliteSorption
#'
#'@export 
#'
#'@docType methods
#'
rmacroliteSorption <- function( x, ... ){ 
    UseMethod( "rmacroliteSorption" )
}   



#'@rdname rmacroliteSorption-methods
#'
#'@method rmacroliteSorption macroParFile
#'
#'@export 
#'
rmacroliteSorption.macroParFile <- function( 
    x,  
    ... 
){  
    oc_pc <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "ORGC\t%s\t%s", 
        type = "PROPERTIES" ) )
    
    kf <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "ZKD\t%s\t%s", 
        type = "SOLUTE PARAMETERS" ) )
    
    kfoc <- kf / (oc_pc / 100)
    
    nf <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "FREUND\t%s\t%s", 
        type = "SOLUTE PARAMETERS" ) )
    
    koc <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "KOC\t%s", 
        type = "SOLUTE PARAMETERS" ) )
    
    out <- list(
        "layer" = data.frame(
            "layer_no" = 1:length( oc_pc ), 
            "oc_pc"    = oc_pc, 
            "kf"       = kf, 
            "kfoc"     = kfoc, 
            "nf"       = nf 
        ),  
        "site" = c( "koc" = koc ) )   
    
    return( out ) 
}   



#'@rdname rmacroliteSorption-methods
#'
#'@usage rmacroliteSorption( x, ... ) <- value
#'
#'@export
#'
`rmacroliteSorption<-` <- function( x, ..., value ){ 
    UseMethod( "rmacroliteSorption<-" )
}   



#'@rdname rmacroliteSorption-methods
#'
#'@method rmacroliteSorption<- macroParFile
#'
#'@export 
#'
#'@usage \method{rmacroliteSorption}{macroParFile}(x, ...) <- value
#'
`rmacroliteSorption<-.macroParFile` <- function( x, ..., value ){ 
    value_names <- c( "kfoc", "nf" )
    
    if( !is.numeric( value ) ){
        stop( sprintf( 
            "Argument 'value' should be a numeric-vector of length %s (kfoc and nf; now class %s)", 
            length( value_names ), 
            paste( class( value ), collapse = ", " ) ) )
    }   
    
    if( length( value ) != length( value_names ) ){
        stop( sprintf( 
            "Argument 'value' should be a numeric-vector of length %s (kfoc and nf; now length %s)", 
            length( value_names ), length( value ) ) )
    }   
    
    if( is.null( names( value ) ) ){
        names( value ) <- value_names
        
    }else{
        if( !all( value_names %in% names( value )) ){
            stop( sprintf( 
                "Argument 'value': names(value) does not contain (all) the expected labels (expect: %s; current names: %s)", 
                paste( value_names, collapse = ", " ), 
                paste( names( value ), collapse = ", " ) 
            ) )
        }   
    }   
    
    # value <- value[ value_names ]
    
    oc_pc <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "ORGC\t%s\t%s", 
        type = "PROPERTIES" ) )
    
    kf <- value[ "kfoc" ] * (oc_pc / 100)
    
    # x <- rmacroliteChange1Param( 
        # x     = x, 
        # pTag  = "ZKD\t%s\t%s", 
        # type  = "SOLUTE PARAMETERS", 
        # tagNb = 1:length( oc_pc ), 
        # value = kf ) 
    
    n <- length( oc_pc )
    
    x <- rmacroliteChangeParam( x = x, p = data.frame( 
        "tag"    = sprintf( "ZKD\t%s\t%s", 1:n, "%s" ), 
        "values" = kf, 
        "type"   = "SOLUTE PARAMETERS", 
        "set_id" = rep( 1L, n ), 
        # "tagNb"  = rep( 1L, n ), 
        stringsAsFactors = FALSE ) )
    
    x <- rmacroliteChangeParam( 
        x = x[[ 1L ]], p = data.frame( 
            "tag"    = sprintf( "FREUND\t%s\t%s", 1:n, "%s" ), 
            "values" = as.numeric( value[ "nf" ] ), 
            "type"   = "SOLUTE PARAMETERS", 
            "set_id" = rep( 1L, n ), 
            # "tagNb"  = rep( 1L, n ), 
            stringsAsFactors = FALSE ) ) 
    
    x <- rmacroliteChangeParam( 
        x = x[[ 1L ]], p = data.frame( 
            "tag"    = "KOC\t%s", 
            "values" = as.numeric( value[ "kfoc" ] ), 
            "type"   = "SOLUTE PARAMETERS", 
            # "set_id" = 1L, 
            # "tagNb"  = rep( 1L, n ), 
            stringsAsFactors = FALSE ) ) 
    
    x <- x[[ 1L ]]
    
    return( x ) 
}   



# rmacroliteDegradation ====================================

#' Fetch or set substance degradation paremeters in an imported MACRO par-file
#'
#' Fetch or set substance degradation paremeters in an imported 
#'  MACRO par-file: DT50 [days], reference temperature at 
#'  which the DT50 was measured [degrees Celcius], pF at which 
#'  the DT50 was measured [log10(cm)], the exponent of the 
#'  temperature response and the exponent of the moisture 
#'  response (The DT50 is assigned to four parameters DEGMAL, 
#'  DEGMAS, DEGMIL and DEGMIS, and the other parameters are 
#'  TREF, PF1, TRESP and EXPB in MACRO par-files, respectively). 
#'  The function either fetches all these parameters or set 
#'  them all at once.
#'
#'
#'@param x
#'  A \code{macroParFile}, as imported with 
#'  \code{\link[rmacrolite]{rmacroliteImportParFile-methods}}
#'
#'@param dt50_depth_f
#'  A vector of numeric-values with as many values as layers 
#'  in the imported par-file \code{x}. When not \code{NULL}, 
#'  the dt50 of each layer will be set as dt50 * \code{dt50_depth_f} 
#'  corresponding to the layer, where \code{dt50} is the value 
#'  of the substance half life as given in the parameter 
#'  \code{value}. If \code{NULL} (the default), 
#'  \code{dt50_depth_f} is calculated internally from the 
#'  original degradation parameters in \code{x}. Please notice 
#'  that when calculated internally, \code{dt50_depth_f} is 
#'  rounded to 4-digits, to avoid problems of numerical 
#'  accuracy.
#'
#'@param value
#'  Vector of five numeric-value, optionally with named 
#'  "dt50", "dt50_ref_temp", "dt50_pf", "exp_temp_resp" and 
#'  "exp_moist_resp" (observe the lowercase). 
#'  Value of the degradation parameters (see above) to be set 
#'  in the par-file \code{x}.
#'
#'@param \dots
#'  Additional parameters passed to specific methods. 
#'  Currently not used.
#' 
#'
#'@return
#'  Returns a list with two items, \code{"layer"} 
#'  (layer-specific parameters) and \code{"site"} 
#'  (site-specific parameters, i.e. parameters that do 
#'  not vary with depth or with the crop). \code{"layer"} is 
#'  a \code{\link[base]{data.frame}} with the following 
#'  columns \code{layer_no}, \code{dt50} and 
#'  \code{dt50_depth_f},  (Layer number, DT50 [days] and the 
#'  depth factor of the DT50 (DT50 horizon/ DT50 first horizon), 
#'  respectively). \code{dt50_depth_f} is 
#'  not a MACRO input parameters and is here calculated from 
#'  the \code{dt50} of different horizons.
#'  \code{"site"} is a vector of named numeric values with 
#'  the follwing items \code{dt50_ref_temp}, 
#'  \code{dt50_pf}, \code{exp_temp_resp} and  
#'  \code{exp_moist_resp}  
#'  (reference temperature at which the DT50 was measured 
#'  [degrees Celcius], pF at which the DT50 was measured 
#'  [log10(cm)], the exponent of the temperature response, 
#'  the exponent of the moisture response)
#'
#'
#'@rdname rmacroliteDegradation-methods
#'@aliases rmacroliteDegradation
#'
#'@export 
#'
#'@docType methods
#'
rmacroliteDegradation <- function( x, ... ){ 
    UseMethod( "rmacroliteDegradation" )
}   



#'@rdname rmacroliteDegradation-methods
#'
#'@method rmacroliteDegradation macroParFile
#'
#'@export 
#'
rmacroliteDegradation.macroParFile <- function( 
    x,  
    ... 
){  
    #   Number of digits when rounding the depth-factor 
    #   of the half life:
    dt50_depth_f_digits <- getRmlPar( "digits_dt50_depth_f" ) 
    
    dt50 <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "DEGMAL\t%s\t%s", 
        type = "SOLUTE PARAMETERS" ) )
    
    #   Convert to a data.frame
    dt50 <- data.frame(
        "DEGMAL" = dt50
    )   
    # dt50[, "DEGMAL" ] <- logb( 2 ) / dt50[, "DEGMAL" ]
    
    #   Fetch the other dt50
    dt50[, "DEGMAS" ] <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "DEGMAS\t%s\t%s", 
        type = "SOLUTE PARAMETERS" ) )
    # dt50[, "DEGMAS" ] <- logb( 2 ) / dt50[, "DEGMAS" ]
    
    dt50[, "DEGMIL" ] <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "DEGMIL\t%s\t%s", 
        type = "SOLUTE PARAMETERS" ) )
    # dt50[, "DEGMIL" ] <- logb( 2 ) / dt50[, "DEGMIL" ]
    
    dt50[, "DEGMIS" ] <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "DEGMIS\t%s\t%s", 
        type = "SOLUTE PARAMETERS" ) )
    # dt50[, "DEGMIS" ] <- logb( 2 ) / dt50[, "DEGMIS" ]
    
    #   Check if the different DT50 are equal:
    dt50_equal <- apply( X = dt50, MARGIN = 1, FUN = function(y){
        return( length( unique( y ) ) == 1L )
    } )
    
    if( any( !dt50_equal ) ){
        message( 
            "Found non-identical DT50 (DEGMAL, DEGMAS, DEGMIL, DEGMIS) for the same horizon in the par-file" )
        
        dt50_k <- dt50
        dt50   <- logb( 2 ) / dt50 
        colnames( dt50 ) <- sprintf( "%s_dt50", 
            colnames( dt50 ) )
        
        out_layer <- data.frame( 
            "layer_no" = 1:nrow(dt50), 
            dt50_k, 
            dt50 )
        # rm( dt50 )
        
    }else{
        out_layer <- data.frame( 
            "layer_no" = 1:nrow(dt50), 
            "dt50"     = (logb( 2 ) / dt50[, "DEGMAL" ]), 
            "k"        = dt50[, "DEGMAL" ] )
        # rm( dt50 ) 
    }   
    
    dt50_ref_temp <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "TREF\t%s", 
        type = "SOLUTE PARAMETERS" ) )
    
    dt50_pf <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "PF1\t%s", 
        type = "SOLUTE PARAMETERS" ) )
    
    exp_temp_resp <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "TRESP\t%s", 
        type = "SOLUTE PARAMETERS" ) )
    
    exp_moist_resp <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "EXPB\t%s", 
        type = "SOLUTE PARAMETERS" ) )
    
    out_site <- c( "dt50_ref_temp" = dt50_ref_temp, 
        "dt50_pf" = dt50_pf, "exp_temp_resp" = exp_temp_resp, 
        "exp_moist_resp" = exp_moist_resp )
    
    rm( dt50_ref_temp, dt50_pf, exp_temp_resp, exp_moist_resp )
    
    
    
    if( any( !dt50_equal ) ){
        dt50_depth_f <- dt50[ 1L, ] / dt50
        dt50_depth_f <- round( 
            x      = dt50_depth_f, 
            digits = dt50_depth_f_digits )
        
        colnames( dt50_depth_f ) <- sprintf( "%s_f", 
            colnames( dt50 ) )
        
        out_layer <- data.frame( out_layer, dt50_depth_f )
        rm( dt50_depth_f )
        
    }else{
        out_layer[, "dt50_depth_f" ] <- 
            out_layer[ 1L, "dt50" ] / out_layer[, "dt50" ]
        out_layer[, "dt50_depth_f" ] <- round(
            x      = out_layer[, "dt50_depth_f" ], 
            digits = dt50_depth_f_digits ) 
        
    }   
    
    
    
    return( list( "layer" = out_layer, "site" = out_site ) ) 
}   



#'@rdname rmacroliteDegradation-methods
#'
#'@usage 
#'  rmacroliteDegradation( x, dt50_depth_f = NULL, ... ) <- value
#'
#'@export
#'
`rmacroliteDegradation<-` <- function( 
    x, 
    dt50_depth_f = NULL, 
    ..., 
    value 
){ 
    UseMethod( "rmacroliteDegradation<-" )
}   



#'@rdname rmacroliteDegradation-methods
#'
#'@method rmacroliteDegradation<- macroParFile
#'
#'@export 
#'
#'@usage \method{rmacroliteDegradation}{macroParFile}(x, dt50_depth_f = NULL, ...) <- value
#'
`rmacroliteDegradation<-.macroParFile` <- function( 
    x, 
    dt50_depth_f = NULL, 
    ..., 
    value 
){  
    value_names <- c( "dt50", "dt50_ref_temp", "dt50_pf", 
        "exp_temp_resp", "exp_moist_resp" ) 
    
    if( !is.numeric( value ) ){
        stop( sprintf( 
            "Argument 'value' should be a numeric-vector of length %s (now class: %s)", 
            length( value_names ), 
            paste( class( value ), collapse = ", " ) ) )
    }   
    
    if( length( value ) != length( value_names ) ){
        stop( sprintf( 
            "Argument 'value' should be a numeric-vector of length %s (now length: %s)", 
            length( value_names ), length( value ) ) )
    }   

    
    if( is.null( names( value ) ) ){
        names( value ) <- value_names
        
    }else{
        if( !all( value_names %in% names( value )) ){
            stop( sprintf( 
                "Argument 'value': names(value) does not contain (all) the expected labels (expect: %s; current names: %s)", 
                paste( value_names, collapse = ", " ), 
                paste( names( value ), collapse = ", " ) 
            ) )
        }   
    }   
    
    
    
    #   Number of digits when rounding the depth-factor 
    #   of the half life:
    digits_parfile_k <- getRmlPar( "digits_parfile_k" ) 
    # is_metabolite <- rmacroliteSimType( x = x )[[ "type" ]] %in% 3L:4L
    # digits_parfile_k <- ifelse( 
        # test = is_metabolite, 
        # yes  = digits_parfile_k["metabolite"], 
        # no   = digits_parfile_k["parent"] )
    
    #   Fetch current degradation parameters
    dt50_x <- rmacroliteDegradation( x = x )
    n      <- nrow( dt50_x[[ "layer" ]] ) 
    
    #   Check or fetch the depth factors for dt50 decrease
    if( is.null( dt50_depth_f ) ){
        dt50_depth_f <- 
            dt50_x[[ "layer" ]][, "dt50_depth_f" ]
    }else{
        if( !is.numeric( dt50_depth_f ) ){
            stop( sprintf( 
                "'dt50_depth_f' should be a vector of numerical values (now class: %s)", 
                paste( class( dt50_depth_f ), collapse = "; " ) ) )
        }   
        
        if( length( dt50_depth_f ) != n ){
            stop( sprintf( 
                "length( dt50_depth_f ) (now %s) should be equal to the number of layers in the par-file (%s)", 
                length( dt50_depth_f ), n ) ) 
        }   
    }   
    
    # value <- value[ value_names ]
    
    for( nm in c( "DEGMAL", "DEGMAS", "DEGMIL", "DEGMIS" ) ){
        x <- rmacroliteChangeParam( x = x, p = data.frame( 
            "tag"    = sprintf( "%s\t%s\t%s", nm, 1:n, "%s" ), 
            "values" = signif( x = ( logb( 2 ) / 
                as.numeric(value[[ "dt50" ]]) ) * 
                dt50_depth_f, digits = digits_parfile_k ), 
            "type"   = "SOLUTE PARAMETERS", 
            "set_id" = rep( 1L, n ), 
            stringsAsFactors = FALSE ) )[[ 1L ]]
        
    };  rm( nm )
    
    x <- rmacroliteChangeParam( 
        x = x, p = data.frame( 
            "tag"    = "TREF\t%s", 
            "values" = as.numeric( value[ "dt50_ref_temp" ] ), 
            "type"   = "SOLUTE PARAMETERS", 
            stringsAsFactors = FALSE ) )[[ 1L ]]
    
    x <- rmacroliteChangeParam( 
        x = x, p = data.frame( 
            "tag"    = "PF1\t%s", 
            "values" = as.numeric( value[ "dt50_pf" ] ), 
            "type"   = "SOLUTE PARAMETERS", 
            stringsAsFactors = FALSE ) )[[ 1L ]]
    
    x <- rmacroliteChangeParam( 
        x = x, p = data.frame( 
            "tag"    = "TRESP\t%s", 
            "values" = as.numeric( value[ "exp_temp_resp" ] ), 
            "type"   = "SOLUTE PARAMETERS", 
            stringsAsFactors = FALSE ) )[[ 1L ]]
    
    x <- rmacroliteChangeParam( 
        x = x, p = data.frame( 
            "tag"    = "EXPB\t%s", 
            "values" = as.numeric( value[ "exp_moist_resp" ] ), 
            "type"   = "SOLUTE PARAMETERS", 
            stringsAsFactors = FALSE ) )[[ 1L ]]    
    
    return( x ) 
}   



# rmacroliteLayers =======================================

#' Fetch information (depths, thicknesses) on the layers in an imported MACRO par-file
#'
#' Fetch information on the layers (i.e "horizons") in an 
#'  imported MACRO par-file: thicknesses [cm], start and end 
#'  dephs [cm], number of numerical layers. These values are 
#'  based on, or corresponds to the parameters NHORIZON 
#'  (par-file header), NHORIZON (PROPERTIES section), NLAYER 
#'  (par-file header), HTICK and NLAYER (PROPERTIES ROPERTIES 
#'  section) in MACRO par-files. 
#'
#'
#'@param x
#'  A \code{macroParFile}, as imported with 
#'  \code{\link[rmacrolite]{rmacroliteImportParFile-methods}}
#'
#'@param \dots
#'  Additional parameters passed to specific methods. 
#'  Currently not used.
#' 
#'
#'@return
#'  Returns a list with two items, \code{"layer"} 
#'  (layer-specific parameters) and \code{"site"} 
#'  (site-specific parameters, i.e. parameters that do 
#'  not vary with depth or with the crop). \code{"layer"} is 
#'  a \code{\link[base]{data.frame}} with the following 
#'  columns \code{layer_no}, \code{thick_cm}, 
#'  \code{depth_from_cm}, \code{depth_to_cm} and  
#'  \code{nb_num_layers} (the thickness [cm], the upper and 
#'  lower depths [cm] and the number of numerical layers, 
#'  respectively). \code{"site"} is a vector of named 
#'  numeric values, with the following itemps 
#'  \code{nb_horizons1} (the number of layers/ horizons; 
#'  parameter NHORIZON, header of the par-file), 
#'  \code{nb_horizons2} 
#'  (parameter NHORIZONS, PHYSICAL PARAMETERS section of the par-file) 
#'  \code{nb_horizons3} 
#'  (parameter NHORIZONS, PROPERTIES section of the par-file) 
#'  and \code{nb_num_layers} (the total number number of 
#'  numerical layers; parameter NLAYER in the par-file 
#'  header).
#'  
#'  
#' 
#'
#'@rdname rmacroliteLayers-methods
#'@aliases rmacroliteLayers
#'
#'@export 
#'
#'@docType methods
#'
rmacroliteLayers <- function( x, ... ){ 
    UseMethod( "rmacroliteLayers" )
}   



#'@rdname rmacroliteLayers-methods
#'
#'@method rmacroliteLayers macroParFile
#'
#'@export 
#'
rmacroliteLayers.macroParFile <- function( 
    x,  
    ... 
){      
    nb_horizons1 <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "NHORIZON\t%s", 
        type = "HEAD" ) )
    
    nb_horizons2 <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "NHORIZONS\t%s", 
        type = "PHYSICAL PARAMETERS" ) )
    
    nb_horizons3 <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "NHORIZONS\t%s", 
        type = "PROPERTIES" ) )
    
    if( nb_horizons1 != nb_horizons2 ){
        warning( 
            "In the par-file, NHORIZON is different from NHORIZONS in section PHYSICAL PARAMETERS." )
    }   
    
    if( nb_horizons1 != nb_horizons2 ){
        warning( 
            "In the par-file, NHORIZON is different from NHORIZONS in section PROPERTIES." )
    }   
    
    nb_num_layers <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "NLAYER\t%s", 
        type = "HEAD" ) )
    
    out_site <- c( 
        "nb_horizons1" = nb_horizons1, 
        "nb_horizons2" = nb_horizons2, 
        "nb_horizons3" = nb_horizons3, 
        "nb_num_layers" = nb_num_layers )
    
    thick_cm <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "HTICK\t%s\t%s", 
        type = "PROPERTIES" ) )
    
    layer_no <- 1:length( thick_cm )
    
    nb_num_layers2 <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "NLAYER\t%s\t%s", 
        type = "PROPERTIES" ) )
    
    depth_to_cm   <- cumsum( thick_cm ) 
    depth_from_cm <- c( 0, depth_to_cm[ -length( depth_to_cm ) ] )
    
    if( nb_horizons1 != length(thick_cm) ){
        warning( 
            "In the par-file, NHORIZON is different from the number of horizons/ layers in section PROPERTIES." )
    }   
    
    if( nb_num_layers != sum(nb_num_layers2) ){
        warning( 
            "The total number of numerical layers (NLAYER in par-file header: %s) is different from the sum of the numerical layers' number in each horizon (NLAYER in par-file PROPERTIES section: %s).",
            nb_num_layers, sum(nb_num_layers2) )
    }   
    
    out_layer <- data.frame(
        "layer_no" = layer_no, 
        "thick_cm" = thick_cm, 
        "depth_from_cm" = depth_from_cm, 
        "depth_to_cm"   = depth_to_cm, 
        "nb_num_layers" = nb_num_layers2 
    )   
    
    return( list( "layer" = out_layer, "site" = out_site ) ) 
}   






# rmacroliteCropUptF =======================================

#' Fetch or set the crop uptake factor in an imported MACRO par-file
#'
#' Fetch or set the crop uptake factor in an imported MACRO 
#'  par-file. The crop uptake factor is FSTAR in MACRO 
#'  par-files.
#'
#'
#'@param x
#'  A \code{macroParFile}, as imported with 
#'  \code{\link[rmacrolite]{rmacroliteImportParFile-methods}}
#'
#'@param value
#'  Single numeric-value. New value of the crop uptake factor 
#'  to be set in the imported par-file (\code{x}).
#'
#'@param \dots
#'  Additional parameters passed to specific methods. 
#'  Currently not used.
#' 
#'
#'@return
#'  Single numeric-value. Current value of the crop uptake 
#'  factor in the imported par-file (\code{x}).
#' 
#'
#'@rdname rmacroliteCropUptF-methods
#'@aliases rmacroliteCropUptF
#'
#'@example inst/examples/rmacroliteCropUptF-example.r
#'
#'@export 
#'
#'@docType methods
#'
rmacroliteCropUptF <- function( x, ... ){ 
    UseMethod( "rmacroliteCropUptF" )
}   



#'@rdname rmacroliteCropUptF-methods
#'
#'@method rmacroliteCropUptF macroParFile
#'
#'@export 
#'
rmacroliteCropUptF.macroParFile <- function( 
    x,  
    ... 
){  
    cuf <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "FSTAR\t%s", 
        type = "SOLUTE PARAMETERS" ) )
    
    
    return( cuf ) 
}   



#'@rdname rmacroliteCropUptF-methods
#'
#'@usage rmacroliteCropUptF( x, ... ) <- value
#'
#'@export
#'
`rmacroliteCropUptF<-` <- function( x, ..., value ){ 
    UseMethod( "rmacroliteCropUptF<-" )
}   



#'@rdname rmacroliteCropUptF-methods
#'
#'@method rmacroliteCropUptF<- macroParFile
#'
#'@export 
#'
#'@usage \method{rmacroliteCropUptF}{macroParFile}(x, ...) <- value
#'
`rmacroliteCropUptF<-.macroParFile` <- function( x, ..., value ){ 
    # value_names <- c( "kfoc", "nf" )
    
    if( !is.numeric( value ) ){
        stop( sprintf( 
            "Argument 'value' should be a numeric-vector of length 1 (now class: %s)", 
            # length( value_names ), 
            paste( class( value ), collapse = ", " ) ) )
    }   
    
    if( length( value ) != 1 ){
        stop( sprintf( 
            "Argument 'value' should be a numeric-vector of length 1 (now length: %s)", 
            length( value ) ) )
    }   
    
    x <- rmacroliteChangeParam( x = x, p = data.frame( 
        "tag"    = "FSTAR\t%s", 
        "values" = value, 
        "type"   = "SOLUTE PARAMETERS", 
        stringsAsFactors = FALSE ) )[[ 1L ]]
    
    return( x ) 
}   



# rmacroliteVapourPres =====================================

#   vapour_pres, vapour_pres_ref_temp

#   IRRELEVANT NOT A PARAMETER IN MACRO (ALTHOUGH GIVEN 
#   IN MACRO IN FOCUS GUI)



# rmacroliteDiffCoef =======================================

#' Fetch or set the substance diffusion coefficient [m2/s] in an imported MACRO par-file
#'
#' Fetch or set the substance diffusion coefficient [m2/s] 
#'  in an imported MACRO par-file. The diffusion coefficient 
#'  is DIFF in MACRO par-files.
#'
#'
#'@param x
#'  A \code{macroParFile}, as imported with 
#'  \code{\link[rmacrolite]{rmacroliteImportParFile-methods}}
#'
#'@param value
#'  Single numeric-value. New value of the substance diffusion 
#'  coefficient [m2/s] to be set in the imported par-file 
#'  (\code{x}).
#'
#'@param \dots
#'  Additional parameters passed to specific methods. 
#'  Currently not used.
#' 
#'
#'@return
#'  Single numeric-value. Current value of the substance 
#'  diffusion coefficient [m2/s] in the imported par-file 
#'  (\code{x}).
#' 
#'
#'@rdname rmacroliteDiffCoef-methods
#'@aliases rmacroliteDiffCoef
#'
#'@example inst/examples/rmacroliteDiffCoef-example.r
#'
#'@export 
#'
#'@docType methods
#'
rmacroliteDiffCoef <- function( x, ... ){ 
    UseMethod( "rmacroliteDiffCoef" )
}   



#'@rdname rmacroliteDiffCoef-methods
#'
#'@method rmacroliteDiffCoef macroParFile
#'
#'@export 
#'
rmacroliteDiffCoef.macroParFile <- function( 
    x,  
    ... 
){  
    diff_c <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "DIFF\t%s", 
        type = "SOLUTE PARAMETERS" ) )
    
    
    return( diff_c ) 
}   



#'@rdname rmacroliteDiffCoef-methods
#'
#'@usage rmacroliteDiffCoef( x, ... ) <- value
#'
#'@export
#'
`rmacroliteDiffCoef<-` <- function( x, ..., value ){ 
    UseMethod( "rmacroliteDiffCoef<-" )
}   



#'@rdname rmacroliteDiffCoef-methods
#'
#'@method rmacroliteDiffCoef<- macroParFile
#'
#'@export 
#'
#'@usage \method{rmacroliteDiffCoef}{macroParFile}(x, ...) <- value
#'
`rmacroliteDiffCoef<-.macroParFile` <- function( x, ..., value ){ 
    if( !is.numeric( value ) ){
        stop( sprintf( 
            "Argument 'value' should be a numeric-vector of length 1 (now class: %s)", 
            # length( value_names ), 
            paste( class( value ), collapse = ", " ) ) )
    }   
    
    if( length( value ) != 1 ){
        stop( sprintf( 
            "Argument 'value' should be a numeric-vector of length 1 (now length: %s)", 
            length( value ) ) )
    }   
    
    x <- rmacroliteChangeParam( x = x, p = data.frame( 
        "tag"    = "DIFF\t%s", 
        "values" = gsub( 
            x           = as.character( value ), 
            pattern     = "e", 
            replacement = "E", 
            fixed       = TRUE ), 
        "type"   = "SOLUTE PARAMETERS", 
        stringsAsFactors = FALSE ) )[[ 1L ]]
    
    return( x ) 
}   



# rmacroliteInfo ===========================================

#' Fetch or set the INFORMATION section at the end of an imported MACRO par-file
#'
#' Fetch or set the INFORMATION section at the end of an 
#'  imported MACRO par-file. Only relevant for par-files 
#'  produced by MACRO In FOCUS.
#'
#'
#'@param x
#'  A \code{macroParFile}, as imported with 
#'  \code{\link[rmacrolite]{rmacroliteImportParFile-methods}}, and 
#'  preferably produced by MACRO In FOCUS.
#'
#'@param value
#'  Vector of character strings named \code{"output_file"}, 
#'  \code{"type"} and \code{"compound"}. Name and path of 
#'  the output file, type of simulation ("parent"; 
#'  "parent, intermediate", "metabolite" or 
#'  "metabolite, intermediate") and name of the compound 
#'  parametrised in the par-file, respectively
#'
#'@param \dots
#'  Additional parameters passed to specific methods. 
#'  Currently not used.
#' 
#'
#'@return
#'  Vector of character strings named \code{"output_file"}, 
#'  \code{"type"} and \code{"compound"}. Name and path of 
#'  the output file, type of simulation ("parent"; 
#'  "parent, intermediate", "metabolite" or 
#'  "metabolite, intermediate") and name of the compound 
#'  parametrised in the par-file, respectively
#' 
#'
#'@rdname rmacroliteInfo-methods
#'@aliases rmacroliteInfo
#'
#'@example inst/examples/rmacroliteInfo-example.r
#'
#'@export 
#'
#'@docType methods
#'
rmacroliteInfo <- function( x, ... ){ 
    UseMethod( "rmacroliteInfo" )
}   



#'@rdname rmacroliteInfo-methods
#'
#'@method rmacroliteInfo macroParFile
#'
#'@export 
#'
rmacroliteInfo.macroParFile <- function( 
    x,  
    ... 
){  
    x_info <- x[[ "par" ]]
    x_info <- x_info[ 
        x_info[, "category" ] == "INFORMATION", 
        "parFile" ]
    
    #   Split by colon ": "
    x_info_colon <- strsplit(
        x     = x_info, 
        split = ": ", 
        fixed = TRUE 
    )   
    
    #   Split by colon "= "
    x_info_equal <- strsplit(
        x     = x_info, 
        split = "= ", 
        fixed = TRUE 
    )   
    
    #   Fetch "Output File"
    output_file <- grepl( 
        pattern = tolower( "Output File" ), 
        x       = tolower( x_info ), 
        fixed   = TRUE )
    
    if( !any( output_file ) ){
        stop( "The tag 'Output File' could not be found." )
    }   
    output_file <- x_info_equal[ output_file ][[ 1L ]]
    output_file <- output_file[ length( output_file ) ] 
    
    #   Fetch "Type of compound"
    type <- grepl( 
        pattern = tolower( "Type of compound" ), 
        x       = tolower( x_info ), 
        fixed   = TRUE )
    if( !any( type ) ){
        stop( "The tag 'Type of compound' could not be found." )
    }   

    type <- x_info_equal[ type ][[ 1L ]]
    type <- type[ length( type ) ] 
    
    #   Fetch "Compound"
    compound <- grepl( 
        pattern = tolower( "Compound" ), 
        x       = tolower( x_info ), 
        fixed   = TRUE ) & !grepl( 
        pattern = tolower( "Type of compound" ), 
        x       = tolower( x_info ), 
        fixed   = TRUE )
    if( !any( compound ) ){
        stop( "The tag 'Compound' could not be found." )
    }   
    compound <- x_info_colon[ compound ][[ 1L ]]
    compound <- compound[ length( compound ) ] 
    
    out <- c( "output_file" = output_file, "type" = type,  
        "compound" = compound )
    
    return( out ) 
}   



#'@rdname rmacroliteInfo-methods
#'
#'@usage rmacroliteInfo( x, ... ) <- value
#'
#'@export
#'
`rmacroliteInfo<-` <- function( x, ..., value ){ 
    UseMethod( "rmacroliteInfo<-" )
}   



#'@rdname rmacroliteInfo-methods
#'
#'@method rmacroliteInfo<- macroParFile
#'
#'@export 
#'
#'@usage \method{rmacroliteInfo}{macroParFile}(x, ...) <- value
#'
`rmacroliteInfo<-.macroParFile` <- function( x, ..., value ){ 
    oldInfo <- rmacroliteInfo( x = x ) 
    
    if( !("INFORMATION" %in% x[[ "par" ]][, "category" ]) ){
        warning( "'x' does not contain any INFORMATION section. Information in 'value' could not be set." )
        
    }else{
        value_expect <- data.frame( 
            "name"    = c(    "output_file",                "type",            "compound" ), 
            "tag"     = c( "Output File = ", "Type of compound = ",         "Compound : " ), 
            "not_tag" = c(    NA_character_,         NA_character_,    "Type of compound" ), 
            stringsAsFactors = FALSE 
        )   
        
        if( !is.character( value ) ){
            stop( sprintf( 
                "Argument 'value' should be a character-vector (now class: %s)", 
                paste( class( value ), collapse = ", " ) ) )
        }   
        
        if( !any( value_expect[, "name" ] %in% names( value ) ) ){
            stop( sprintf( 
                "Argument 'value' should contain at least one of the following labels: %s", 
                paste( class( value_expect[, "name" ] ), collapse = "; " ) ) )
        }   
        
        for( i in 1:nrow( value_expect ) ){
            if( value_expect[ i, "name" ] %in% names( value ) ){
                sel_row <- x[[ "par" ]][, "category" ] == "INFORMATION" 
                sel_row <- sel_row & grepl( 
                    pattern = value_expect[ i, "tag" ], 
                    x       = x[[ "par" ]][, "parFile" ], 
                    fixed   = TRUE 
                )   
                
                if( !is.na( value_expect[ i, "not_tag" ] ) ){
                    sel_row <- sel_row & !grepl( 
                        pattern = value_expect[ i, "not_tag" ], 
                        x       = x[[ "par" ]][, "parFile" ], 
                        fixed   = TRUE 
                    )   
                }   
                
                if( any( sel_row ) ){
                    x[[ "par" ]][ sel_row, "parFile" ] <- 
                        paste( value_expect[ i, "tag" ], 
                        as.character( value[ value_expect[ i, "name" ] ] ), 
                        sep = "" )
                }else{
                    stop( sprintf( 
                        "Can't find the tag '%s' in 'x'%s.", 
                        value_expect[ i, "tag" ], 
                        ifelse( 
                            is.na( value_expect[ i, "not_tag" ] ), 
                            "", 
                            sprintf( " (not matching '%s')", 
                                value_expect[ i, "not_tag" ] ) ) ) )
                }   
            }   
        }   
        
        if( "compound" %in% names( value ) ){
            sel_row <- x[[ "par" ]][, "category" ] == "INFORMATION" 
            sel_row <- sel_row & grepl( 
                pattern = "Application ", 
                x       = x[[ "par" ]][, "parFile" ], 
                fixed   = TRUE 
            )   
            
            if( any( sel_row ) ){
                # x[[ "par" ]][ sel_row, "parFile" ] <- gsub( 
                    # pattern     = as.character( oldInfo[ "compound" ] ), 
                    # replacement = as.character( value[ "compound" ] ), 
                    # x           = x[[ "par" ]][ sel_row, "parFile" ], 
                    # fixed       = TRUE 
                # )   
                
                #   Fetch info on applications
                appln <- rmacroliteApplications( x = x )
                appln <- unique( appln )
                
                if( (nrow( appln ) > 1L) & (!all(appln[, "g_as_per_ha" ] == 0)) ){
                    appln <- appln[ appln[, "g_as_per_ha" ] != 0, ]
                }   
                
                # if( nrow( appln ) > 1L ){
                    # appln <- appln[ 1L, ] 
                    # # TO DO: insert multiple rows below
                    # #   when multiple non-0 applications
                # }   
                
                application_info <- sprintf(
                    "Application %s : %s g/ha of %s on day %s", 
                    1:nrow(appln), 
                    appln[, "g_as_per_ha" ], 
                    as.character( value[ "compound" ] ), 
                    appln[, "app_j_day" ] )
                
                application_info <- data.frame(
                    "parFile"  = application_info, 
                    "category" = "INFORMATION", 
                    stringsAsFactors = FALSE 
                )   
                
                if( max(which(sel_row)) == nrow(x[[ "par" ]]) ){
                    x[[ "par" ]] <- rbind(
                        x[[ "par" ]][ 
                            1L:(min(which(sel_row))-1L), ], 
                        application_info ) 
                    
                }else{
                    x[[ "par" ]] <- rbind(
                        x[[ "par" ]][ 
                            1L:(min(which(sel_row))-1L), ], 
                        application_info, 
                        x[[ "par" ]][ 
                            (max(which(sel_row))+1L):nrow(x[[ "par" ]]), ] )   
                }   
                
                rownames( x[[ "par" ]] ) <- NULL 
            }   
        }   
    }   
    
    return( x ) 
}   



# rmacroliteSimType ========================================

#   parent, intermediate, metabolite

#' Fetch or set the type of simulation in an imported MACRO par-file (parent or metabolite; intermediate or not)
#'
#' Fetch or set the type of simulation in an imported MACRO 
#'  par-file (parent or metabolite; intermediate or not). 
#'  See the description of the argument \code{value} 
#'  and \code{return} below for a description of the 
#'  different simulation types. Here, the term the "parent" 
#'  should be understood as a substance directly applied on 
#'  the field, not a primary or secondary (etc) metabolite. 
#'  An intermediate simulation is a simulation that outputs 
#'  only information on the mass of substance is degraded 
#'  at each time step and in each numerical layer. 
#'  Such intermediate simulation is then used as input for 
#'  simulating the fate of the degradation product(s) of this 
#'  substance. 
#'
#'
#'@param x
#'  A \code{macroParFile}, as imported with 
#'  \code{\link[rmacrolite]{rmacroliteImportParFile-methods}}. This 
#'  file MUST be from a parent substance, and not an 
#'  intermediate simulation output. 
#'
#'@param \dots
#'  Additional parameters passed to specific methods. 
#'  Currently not used.
#'
#'@param value
#'  A \code{\link[base]{list}} with two items. The items 
#'  can be named \code{"type"} and \code{"drivingfile"}. 
#'  The first item (\code{"type"}) should be a single 
#'  integer-value indicating the type of simulation and the 
#'  second item (\code{"drivingfile"}) should be a 
#'  single character-string indicating the name of the 
#'  bin-file to be read-in when simulating a metabolite. 
#'  This file should therefore be an intemediate simulation 
#'  output. When \code{"type"} is set to \code{1} or \code{2} 
#'  (ie a parent substance), the second item can be skipped 
#'  entirely.
#'  The first item (\code{"type"}) indicates the type of 
#'  simulation \code{x} should be changed to: \code{1} is for 
#'  setting the simulation to a parent substance, not intermediate 
#'  simulation output, \code{2} is for changing to a parent 
#'  substance, intermediate simulation output, \code{3} is 
#'  for changing to a metabolite, not intermediate simulation 
#'  output and \code{4} is for changing to a metabolite, 
#'  intermediate simulation output. In practice, as \code{x} 
#'  must be a type \code{1} (parent, not intermediate 
#'  simulation output), setting \code{value} to \code{1} is 
#'  pointless.
#'
#'
#'@return
#'  A \code{\link[base]{list}} with two named items.
#'  The first item (\code{"type"}) is a single integer-value 
#'  indicating the type of simulation and the second item 
#'  (\code{"drivingfile"}) is a single character-string 
#'  indicating the name of the bin-file to be read-in when 
#'  simulating a metabolite. This file is therefore be an 
#'  intemediate simulation output. 
#'  The first item (\code{"type"}) indicates the type of 
#'  simulation \code{x} presumably contains: \code{1} is for 
#'  a parent substance, not intermediate simulation output, 
#'  \code{2} is a parent substance, intermediate simulation 
#'  output, \code{3} is a metabolite, not intermediate 
#'  simulation output and \code{4} is a metabolite, intermediate 
#'  simulation output. 
#' 
#'
#'@rdname rmacroliteSimType-methods
#'@aliases rmacroliteSimType
#'
#'@export 
#'
#'@docType methods
#'
rmacroliteSimType <- function( x, ... ){ 
    UseMethod( "rmacroliteSimType" )
}   



#'@rdname rmacroliteSimType-methods
#'
#'@method rmacroliteSimType macroParFile
#'
#'@export 
#'
rmacroliteSimType.macroParFile <- function( 
    x,  
    ... 
){  
    driving <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "DRIVING\t%s", 
        type = "OPTIONS" ) )
    
    metabolite <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "METABOLITE\t%s", 
        type = "OPTIONS" ) )
    
    if( metabolite == 0 ){
        isIntermediate <- FALSE 
        
    }else if( metabolite == 1 ){
        isIntermediate <- TRUE 
        
    }else{
        stop( sprintf(
            "Unknown value for parameter METABOLITE in 'x' (par-file): %s (expects 0 or 1)", 
            metabolite ) ) 
    }   
    
    if( driving == 0 ){
        isParent <- TRUE 
        
    }else if( driving == 1 ){
        isParent <- FALSE 
        
    }else{
        stop( sprintf(
            "Unknown value for parameter DRIVING in 'x' (par-file): %s (expects 0 or 1)", 
            driving ) ) 
    }   
    
    drivingfile <- as.character( rmacroliteGet1Param( 
        x    = x, 
        pTag = "DRIVINGFILE\t%s", 
        type = "SETUP" ) )
    
    if( isParent & (!isIntermediate) ){
        return( list( "type" = 1L, "drivingfile" = drivingfile ) )
        
    }else if( isParent & isIntermediate ){
        return( list( "type" = 2L, "drivingfile" = drivingfile ) )
        
    }else if( (!isParent) & (!isIntermediate) ){
        return( list( "type" = 3L, "drivingfile" = drivingfile ) )
        
    }else{
        return( list( "type" = 4L, "drivingfile" = drivingfile ) )
    }   
}   



#'@rdname rmacroliteSimType-methods
#'
#'@usage rmacroliteSimType( x, ... ) <- value
#'
#'@export
#'
`rmacroliteSimType<-` <- function( x, ..., value ){ 
    UseMethod( "rmacroliteSimType<-" )
}   



#'@rdname rmacroliteSimType-methods
#'
#'@method rmacroliteSimType<- macroParFile
#'
#'@export 
#'
#'@usage \method{rmacroliteSimType}{macroParFile}(x, ...) <- value
#'
`rmacroliteSimType<-.macroParFile` <- function( x, ..., value ){ 
    x_type <- rmacroliteSimType( x = x )[['type']]
    
    if( x_type != 1L ){
        stop( sprintf(
            "'x' must be a parent and non intermediate simulation par-file (ie rmacroliteSimType(x)[['type']] should return 1; now %s)", 
            x_type ) ) 
    }   
    
    
    if( !("list" %in% class( value )) ){
        stop( sprintf( 
            "Argument 'value' should be a list (now class: %s)", 
            paste( class( value ), collapse = ", " ) ) )
    }   
    
    # value_names <- c( "type", "drivingfile" ) 
    
    if( "type" %in% names( value ) ){
        type <- value[[ "type" ]]
    }else{
        type <- value[[ 1L ]]
    }   
    if( !is.integer( type ) ){
        stop( sprintf( 
            "Argument value[[ 'type' ]] or value[[ 1 ]] should be a integer-vector of length 1 (now class: %s)", 
            paste( class( type ), collapse = ", " ) ) )
    }   
    
    if( length( type ) != 1 ){
        stop( sprintf( 
            "Argument value[[ 'type' ]] or value[[ 1 ]] should be a integer-vector of length 1 (now length: %s)", 
            length( type ) ) )
    }   
    
    
    
    if( "drivingfile" %in% names( value ) ){
        drivingfile <- value[[ "drivingfile" ]]
    }else{
        if( length( value ) > 1L ){
            drivingfile <- value[[ 2L ]]
        }else{
            if( type %in% 3:4 ){
                stop( "Item 'drivingfile' not given in 'value' while 'type' is 3 or 4." )
            }else{
                drivingfile <- "" 
            }   
        }   
    }   
    
    if( !is.character( drivingfile ) ){
        stop( sprintf( 
            "Argument value[[ 'drivingfile' ]] or value[[ 2 ]] should be a character-vector of length 1 (now class: %s)", 
            paste( class( drivingfile ), collapse = ", " ) ) )
    }   
    
    if( length( drivingfile ) != 1 ){
        stop( sprintf( 
            "Argument value[[ 'drivingfile' ]] or value[[2]] should be a character-vector of length 1 (now length: %s)", 
            length( drivingfile ) ) )
    }   
    
    
    
    if( "f_conv" %in% names( value ) ){
        f_conv <- value[[ "f_conv" ]]
    }else{
        if( length( value ) > 2L ){
            f_conv <- value[[ 3L ]]
        }else{
            if( type %in% 3:4 ){
                stop( "Item 'f_conv' not given in 'value' while 'type' is 3 or 4." )
            }else{
                f_conv <- 0 
            }   
        }   
    }   
    
    if( (type %in% 1:2) & (f_conv != 0) ){
        stop( sprintf( 
            "Item value[['f_conv']] or value[[3]] is not 0 (%s) while 'type' is 1 or 2.", 
            f_conv ) )
    }   
    
    if( !is.numeric( f_conv ) ){
        stop( sprintf( 
            "Argument value[['f_conv']] or value[[3]] should be a numeric-vector of length 1 (now class: %s)", 
            paste( class( f_conv ), collapse = ", " ) ) )
    }   
    
    if( length( f_conv ) != 1 ){
        stop( sprintf( 
            "Argument value[[ 'f_conv' ]] or value[[3]] should be a numeric-vector of length 1 (now length: %s)", 
            length( f_conv ) ) )
    }   
    
    
    
    #   Prepare a function that change a par-file from 
    #   non intermediate to intermediate. The function 
    #   first set all the variables to not exported and 
    #   then only sets the one releant for an intermediate 
    #   output as exported
    output_to_inter <- function(x){
        #   Category in correct order, as factor
        fc <- factor( 
            x       = x[[ "par" ]][, "category" ], 
            levels  = unique( x[[ "par" ]][, "category" ] ), 
            ordered = TRUE )
        
        #   Split all parameters by category
        par_split <- split( 
            x = x[[ "par" ]], 
            f = fc )
                
        #   Split the output field after tab (\t)
        #   to distinguish between site-output and output 
        #   by numerical layer
        output_split <- strsplit( 
            x     = par_split[[ "OUTPUTS" ]][, "parFile" ], 
            split = "\t", fixed = TRUE )
        
        #   Number of items per output row
        #   output_nb_items = 1, 2 is meta-information (?)
        #   output_nb_items = 3 is output by numerical layer
        #   output_nb_items = 4 is site-output or "header" for 
        #   output by numerical layer
        output_nb_items <- unlist( lapply(
            X   = output_split, 
            FUN = length ) )  
        
        #   Initiate a new output list
        output_new <- par_split[[ "OUTPUTS" ]][, "parFile" ]
        
        #   Remove all output by numerical layer
        output_new      <- output_new[ output_nb_items != 3L ]
        output_split    <- output_split[ output_nb_items != 3L ]
        output_nb_items <- output_nb_items[ output_nb_items != 3L ]
        
        #   Set all output where output_nb_items = 4 to 
        #   not exported
        output_new[ output_nb_items == 4L ] <- unlist( lapply(
            X   = output_split[ output_nb_items == 4L ], 
            FUN = function( y ){ 
                y[ 3L ] <- "0" 
                return( paste( y, collapse = "\t" ) )
            } ) )  
        
        rm( output_split, output_nb_items, fc )
        
        #   Add new output with the the other parameters 
        #   (replace the old one)
        par_split[[ "OUTPUTS" ]] <- data.frame(
            "parFile"  = output_new, 
            "category" = "OUTPUTS", 
            stringsAsFactors = FALSE 
        )   
        
        #   Re-format the par-file data
        x[[ "par" ]] <- do.call( 
            what = "rbind", 
            args = par_split )
        
        rownames( x[[ "par" ]] ) <- NULL 
        
        rm( par_split, output_new )
        
        #   Now add the outputs specific to the intermediate 
        #   file (DEGMIC and DEGMAC)
        
        #   Fetch info on (numerical) layers
        layers_site <- rmacroliteLayers( x = x )[[ "site" ]]
        
        #   Number of numerical layers
        nb_num_layers <- as.integer( 
            layers_site[ "nb_num_layers" ] )
        
        degmic_index <- attr( x = rmacroliteGet1Param( 
            x       = x, 
            pTag    = "DEGMIC\t-1\t%s\t%s", 
            type    = "OUTPUTS" 
        ), "index" ) 
        
        # x <- rmacroliteChangeParam( x = x, p = data.frame( 
            # "tag"    = c( "DEGMIC\t-1\t%s\tG", "DEGMAC\\t-1\t%s\tG" ), 
            # "values" = c(                   0,                    0 ), 
            # "type"   = c(           "OUTPUTS",            "OUTPUTS" ), 
            # "set_id" = c(                  1L,                   1L ), 
            # stringsAsFactors = FALSE ) )[[ 1L ]]
        
        x[[ "par" ]][ degmic_index, "parFile" ] <- 
            "DEGMIC\t-1\t1\tG"
        
        x[[ "par" ]] <- rbind( 
            x[[ "par" ]][ 1:degmic_index, ], 
            data.frame( 
                "parFile"  = sprintf( 
                    "%s\t1\t9998", 
                    1:nb_num_layers ), 
                "category" = "OUTPUTS", 
                stringsAsFactors = FALSE ), 
            x[[ "par" ]][ (degmic_index+1L):nrow(x[[ "par" ]]), ], 
            stringsAsFactors = FALSE 
        )   
        
        degmac_index <- attr( x = rmacroliteGet1Param( 
            x       = x, 
            pTag    = "DEGMAC\t-1\t%s\t%s", 
            type    = "OUTPUTS" 
        ), "index" ) 
        
        x[[ "par" ]][ degmac_index, "parFile" ] <- 
            "DEGMAC\t-1\t1\tG"
        
        x[[ "par" ]] <- rbind( 
            x[[ "par" ]][ 1:degmac_index, ], 
            data.frame( 
                "parFile"  = sprintf( 
                    "%s\t1\t9999", 
                    1:nb_num_layers ), 
                "category" = "OUTPUTS", 
                stringsAsFactors = FALSE ), 
            x[[ "par" ]][ (degmac_index+1L):nrow(x[[ "par" ]]), ], 
            stringsAsFactors = FALSE 
        )   
        
        return( x )
    }   
    
    
    
    if( type == 1L ){          
        # Parent, not intermediate -------------------------
        
        #   Set DRIVING to 0, ie. it is a parent substance
        #   Set METABOLITE to 0, ie. not intermediate output
        x <- rmacroliteChangeParam( x = x, p = data.frame( 
            "tag"    = c( "DRIVING\t%s", "METABOLITE\t%s",      "FCONVERT\t%s" ), 
            "values" = c(             0,                0,              f_conv ), 
            "type"   = c( "OPTIONS",            "OPTIONS", "SOLUTE PARAMETERS" ), 
            "set_id" = c(        1L,                   1L,                  1L ), 
            stringsAsFactors = FALSE ) )[[ 1L ]]
        
        rmacroliteInfo( x = x ) <- c( "type" = "parent" )
        
    }else if( type == 2L ){    
        # Parent, intermediate -----------------------------
        
        #   Set DRIVING to 0, ie. it is a parent substance
        #   Set METABOLITE to 1, ie. intermediate output
        x <- rmacroliteChangeParam( x = x, p = data.frame( 
            "tag"    = c( "DRIVING\t%s", "METABOLITE\t%s",      "FCONVERT\t%s" ), 
            "values" = c(             0,                1,              f_conv ), 
            "type"   = c( "OPTIONS",            "OPTIONS", "SOLUTE PARAMETERS" ), 
            "set_id" = c(        1L,                   1L,                  1L ), 
            stringsAsFactors = FALSE ) )[[ 1L ]]
        
        x <- output_to_inter( x = x ) 
        
        rmacroliteInfo( x = x ) <- c( "type" = "parent, intermediate" )
        
    }else if( type == 3L ){    
        # Metabolite, not intermediate ---------------------
        
        #   Set DRIVING to 1, ie. it is a metabolite
        #   Set METABOLITE to 0, ie. not intermediate output
        x <- rmacroliteChangeParam( x = x, p = data.frame( 
            "tag"    = c( "DRIVING\t%s", "METABOLITE\t%s",        "CANDEG\t%s",      "FCONVERT\t%s" ), 
            "values" = c(             1,                0,                   0,              f_conv ), 
            "type"   = c( "OPTIONS",            "OPTIONS", "SOLUTE PARAMETERS", "SOLUTE PARAMETERS" ), 
            "set_id" = c(        1L,                   1L,                  1L,                  1L ), 
            stringsAsFactors = FALSE ) )[[ 1L ]]
        
        #   Determine how many irrigation events there is:
        n_irr <- length( rmacroliteGet1Param( 
            x    = x, 
            pTag = "CONCI\t%s", 
            type = "IRRIGATION PARAMETERS" ) )    
        
        #   Set the concentration to 0 for all irrigation events
        x <- rmacroliteChangeParam( x = x, p = data.frame( 
            "tag"    = rep( "CONCI\t%s", n_irr ), 
            "values" = rep( 0, n_irr ), 
            "type"   = rep( "IRRIGATION PARAMETERS", n_irr ), 
            "set_id" = rep( 1L, n_irr ), 
            "tagNb"  = 1:n_irr, 
            stringsAsFactors = FALSE ) )[[ 1L ]]
        
        #   Set DRIVINGFILE
        x <- rmacroliteChangeParam( x = x, p = data.frame( 
            "tag"    = "DRIVINGFILE\t%s", 
            "values" = drivingfile, 
            "type"   = "SETUP", 
            stringsAsFactors = FALSE ) )[[ 1L ]]
        
        #   Set layered output parameter tag that 
        #   for some reason changes from 1 to -1 with 
        #   metabolites.
        new_output <- data.frame(
            "from"  = c( "DEGMIC\t-1\t0\tG", "DEGMAC\t-1\t0\tG" ), 
            "to"    = c( "DEGMIC\t1\t0\tG", "DEGMAC\t1\t0\tG" ), 
            stringsAsFactors = FALSE ) 
        
        for( i in 1:nrow( new_output ) ){
            x[[ "par" ]][ 
                x[[ "par" ]][ , "parFile" ] == new_output[ i, "from" ], 
                "parFile" ] <- new_output[ i, "to" ]
        }   
        
        rmacroliteInfo( x = x ) <- c( "type" = "metabolite" )
        
    }else if( type == 4L ){    
        # Metabolite, intermediate -------------------------
                  
        #   Set DRIVING to 1, ie. it is a metabolite
        #   Set METABOLITE to 1, ie. intermediate output
        x <- rmacroliteChangeParam( x = x, p = data.frame( 
            "tag"    = c( "DRIVING\t%s", "METABOLITE\t%s",        "CANDEG\t%s",      "FCONVERT\t%s" ), 
            "values" = c(             1,                1,                   0,              f_conv ), 
            "type"   = c( "OPTIONS",            "OPTIONS", "SOLUTE PARAMETERS", "SOLUTE PARAMETERS" ), 
            "set_id" = c(        1L,                   1L,                  1L,                  1L ), 
            stringsAsFactors = FALSE ) )[[ 1L ]]
        
        #   Determine how many irrigation events there is:
        n_irr <- length( rmacroliteGet1Param( 
            x    = x, 
            pTag = "CONCI\t%s", 
            type = "IRRIGATION PARAMETERS" ) )    
        
        #   Set the concentration to 0 for all irrigation events
        x <- rmacroliteChangeParam( x = x, p = data.frame( 
            "tag"    = rep( "CONCI\t%s", n_irr ), 
            "values" = rep( 0, n_irr ), 
            "type"   = rep( "IRRIGATION PARAMETERS", n_irr ), 
            "set_id" = rep( 1L, n_irr ), 
            "tagNb"  = 1:n_irr, 
            stringsAsFactors = FALSE ) )[[ 1L ]]
        
        #   Set DRIVINGFILE
        x <- rmacroliteChangeParam( x = x, p = data.frame( 
            "tag"    = "DRIVINGFILE\t%s", 
            "values" = drivingfile, 
            "type"   = "SETUP", 
            stringsAsFactors = FALSE ) )[[ 1L ]]
        
        x <- output_to_inter( x = x ) 
        
        rmacroliteInfo( x = x ) <- c( "type" = "metabolite, intermediate" )
        
    }else{
        stop( sprintf( 
            "Argument value[['type']] or value[[ 1 ]] should be 1, 2, 3 or 4 (now: %s)", 
            type ) )
    }   
    
    return( x ) 
}   



# rmacroliteClimateFiles ===================================

#   Fetch paths only
#   Use when importing a par-file to check that the climate 
#   file exists.


#' Fetch the name and path of the rainfall and weather data bin-files in an imported MACRO par-file
#'
#' etch the name and path of the rainfall and weather data 
#'  bin-files in an imported MACRO par-file
#'
#'
#'@param x
#'  A \code{macroParFile}, as imported with 
#'  \code{\link[rmacrolite]{rmacroliteImportParFile-methods}}
#'
#'@param check
#'  Single logical values. If \code{TRUE} (the default), 
#'  the function checks that the two files 
#'  exists and stops when they don't.
#'
#'@param \dots
#'  Additional parameters passed to specific methods. 
#'  Currently not used.
#' 
#'
#'@return
#'  A vector with two named character strings. \code{rain}, 
#'  the name and path to the rainfall file, and \code{met}, 
#'  the name and path to the weather file.
#' 
#'
#'@rdname rmacroliteClimateFiles-methods
#'@aliases rmacroliteClimateFiles
#'
#'@export 
#'
#'@docType methods
#'
rmacroliteClimateFiles <- function( x, ... ){ 
    UseMethod( "rmacroliteClimateFiles" )
}   



#'@rdname rmacroliteClimateFiles-methods
#'
#'@method rmacroliteClimateFiles macroParFile
#'
#'@export 
#'
rmacroliteClimateFiles.macroParFile <- function( 
    x, 
    check = TRUE, 
    ... 
){  
    rain <- rmacroliteGet1Param( 
        x    = x, 
        pTag = "RAINFALLFILE\t%s", 
        type = "SETUP" )
    
    met <- rmacroliteGet1Param( 
        x    = x, 
        pTag = "METFILE\t%s", 
        type = "SETUP" ) 
    
    out <- c( "rain" = rain, "met" = met )
    
    if( check ){
        check_files <- file.exists( out ) 
        
        if( !all( check_files ) ){
            stop( sprintf( 
                "Some climate files could not be found: %s", 
                paste( out[ !check_files ], collapse = ", " )
            ) ) 
        }   
    }   
    
    return( out ) 
}   



# rmacroliteApplications ===================================

#   g_as_per_ha, app_j_day, L_sprayer_per_ha
#   Based on AMIR, CONCI, IRRDAY and MASSUNITS 
#  (1 = micrograms, 2 = milligrams, 3 = grams, 4 = kilograms)

#' Fetch or set the substance application rate and julian day in an imported MACRO par-file
#'
#' Fetch or set the substance application rate [g as/ ha], 
#'  julian day, sprayer volume [L liquid/ha] and 
#'  fraction of sprayed quantity intercepted on the crop canopy 
#'  [g as intercepted/ g as sprayed] in an imported MACRO 
#'  par-file. Calculated from MACRO parameters AMIR, CONCI, 
#'  IRRDAY and MASSUNITS.
#'
#'
#'@param x
#'  A \code{macroParFile}, as imported with 
#'  \code{\link[rmacrolite]{rmacroliteImportParFile-methods}}
#'
#'@param keep0conc
#'  A single logical value. When equal to \code{TRUE}, the 
#'  default irrigation events with a zero concentration in 
#'  the original par-file (\code{x}) will be kept as zero 
#'  concentration and their application date and 
#'  crop interception will not be altered either. 
#'  When set to \code{FALSE} even irrigation events 
#'  with a zero concentration in the original par-file are 
#'  modified according to \code{value}.
#'
#'@param focus_mode
#'  A single character string. Currently, possible values are 
#'  \code{"no"} (the default), or \code{"gw"}. When 
#'  \code{focus_mode = "no"} FOCUS-mode is not activated 
#'  and nothing special is done, that is only the relevant 
#'  parameters (as given in \code{value}) are modified in the 
#'  template par-file \code{x}. When \code{focus_mode = "gw"}, 
#'  the so called IRRIGATION PARAMETERS are entirely replaced 
#'  by new one, as would be done by MACRO In FOCUS. 
#'  Setting \code{focus_mode = "gw"} is especially relevant 
#'  to skip using a template par-file with the relevant number 
#'  of substance application per application-year and the number 
#'  of year intervals in between application-years. A template 
#   with the right scenario and crop is enough.
#'
#'@param \dots
#'  Additional parameters passed to specific methods. 
#'  Currently not used.
#'
#'@param value
#'  List with 3 named items: \code{g_as_per_ha}, 
#'  \code{app_j_day} and \code{f_int}. New values of the substance 
#'  application rate [g as/ ha], application time in Julian 
#'  day and fraction of the sprayed quantity intercepted by 
#'  the crop canopy [g as intercepted/ g as sprayed] to be 
#'  set in the imported par-file (\code{x}). Here is an example 
#'  R code to convert a date 
#'  to Julian days: \code{format(as.Date("1901-10-01"),"\%j")}.
#'  Each item should be a single numeric value or a vector of 
#'  numeric values. When a single numeric value is passed, 
#'  all relevant irrigation events are attributed the same 
#'  parameter-value. When a vector of numeric values, it can 
#'  either be as many values as relevant irrigations over the 
#'  whole simulation period, or a number of irrigations that 
#'  can be recycled over the whole simulation period. By 
#'  relevant irrigation is meant irrigation events that 
#'  have a non-zero concentration when \code{keep0conc} is 
#'  \code{TRUE}, or all irrigation events when \code{keep0conc} 
#'  is \code{FALSE}.
#'
#'
#'@return
#'  A \code{\link{data.frame}} with 4 columns (all 
#'  numeric-values): \code{g_as_per_ha}, 
#'  \code{app_j_day}, \code{f_int} and \code{L_sprayer_per_ha}. 
#'  New value of the substance application rate [g as/ ha], the 
#'  application Julian day, fraction of the sprayed quantity 
#'  intercepted by the crop canopy [g as intercepted/ 
#'  g as sprayed] and the sprayer volume [L liquid/ha].
#' 
#'
#'@rdname rmacroliteApplications-methods
#'@aliases rmacroliteApplications
#'
#'@example inst/examples/rmacroliteApplications-example.r
#'
#'@export 
#'
#'@docType methods
#'
rmacroliteApplications <- function( x, ... ){ 
    UseMethod( "rmacroliteApplications" )
}   



#'@rdname rmacroliteApplications-methods
#'
#'@method rmacroliteApplications macroParFile
#'
#'@export 
#'
rmacroliteApplications.macroParFile <- function( 
    x,  
    ... 
){  
    #   Mass unit: 1 = micrograms, 2 = milligrams, 3 = grams, 
    #   4 = kilograms
    massunits <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "MASSUNITS\t%s", 
        type = "OPTIONS" ) )
    
    #   Coefficient to convert to g active substance per ha
    if( massunits == 1L ){
        g_per_massunit <- 1/1000000 
        
    }else if( massunits == 2L ){
        g_per_massunit <- 1/1000
        
    }else if( massunits == 3L ){
        g_per_massunit <- 1
        
    }else if( massunits == 4L ){
        g_per_massunit <- 1000
        
    }else{
        stop( sprintf( 
            "Unknown value for MASSUNITS (%s) in the par file. Expects 1, 2, 3 or 4.", 
            massunits
        ) ) 
    }   
    
    #   Irrigation amount [mm]
    amir <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "AMIR\t%s", 
        type = "IRRIGATION PARAMETERS" ) ) 
    
    #   Solute concentration in irrigation water [massunits/m3]
    conci <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "CONCI\t%s", 
        type = "IRRIGATION PARAMETERS" ) ) 
    
    if( length( amir ) != length( conci ) ){
        stop( sprintf( 
            "Different number of AMIR (%s) and CONCI (%s) in the par-file", 
            length( amir ), length( conci )
        ) )
    }   
    
    #   Solute concentration in irrigation water [massunits/m3]
    irrday <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "IRRDAY\t%s", 
        type = "IRRIGATION PARAMETERS" ) ) 
    
    if( length( amir ) != length( irrday ) ){
        stop( sprintf( 
            "Different number of AMIR (%s) and IRRDAY (%s) in the par-file", 
            length( amir ), length( irrday )
        ) )
    }   
    
    zfint <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "ZFINT\t%s", 
        type = "IRRIGATION PARAMETERS" ) ) 
    
    if( length( amir ) != length( zfint ) ){
        stop( sprintf( 
            "Different number of AMIR (%s) and ZFINT (%s) in the par-file", 
            length( amir ), length( zfint )
        ) )
    }   
    
    #   Sprayer volume/ ha treated [L]
    L_sprayer_per_ha <- ((amir / 1000) * 10000)*1000
    # ((amir [mm] / 1000 [mm/m]) * 10000 [m2/ha]) * 1000 [L/m3]
    
    #   Calculate application rate:
    g_as_per_ha <- ((conci * g_per_massunit) / 1000) * L_sprayer_per_ha 
    # ((conci [massunits/m3] * g_per_massunit [g/massunits]) / 1000 [L/m3]) * L_sprayer_per_ha [L/ha]
    
    #   Format the output:
    out <- data.frame(
        "g_as_per_ha"      = g_as_per_ha, 
        "app_j_day"        = irrday, 
        "L_sprayer_per_ha" = L_sprayer_per_ha, 
        "f_int"            = zfint 
    )   
    
    # if( nrow( unique( out ) ) == 1L ){
        # out <- unlist( unique( out ) )
    # }   
    
    return( out ) 
}   



#'@rdname rmacroliteApplications-methods
#'
#'@usage rmacroliteApplications( x, keep0conc = TRUE, focus_mode = "no", ... ) <- value
#'
#'@export
#'
`rmacroliteApplications<-` <- function( 
    x, 
    keep0conc = TRUE, 
    focus_mode = "no", 
    ..., 
    value 
){  
    UseMethod( "rmacroliteApplications<-" )
}   



#'@rdname rmacroliteApplications-methods
#'
#'@method rmacroliteApplications<- macroParFile
#'
#'@export 
#'
#'@usage \method{rmacroliteApplications}{macroParFile}(x, keep0conc = TRUE, focus_mode = "no", ...) <- value
#'
`rmacroliteApplications<-.macroParFile` <- function( 
    x, 
    keep0conc = TRUE, 
    focus_mode = "no", 
    ..., 
    value 
){  
    value_expect <- c( "g_as_per_ha", "app_j_day", 
        "f_int" ) # "L_sprayer_per_ha"
    
    if( !(focus_mode %in% c( "no", "gw" )) ){
        stop( sprintf( 
            "Argument 'focus_mode' can either be 'no' or 'gw' (currently %s)", 
            focus_mode ) )
    }   
    if( !("list" %in% class( value )) ){
        stop( sprintf( 
            "Argument 'value' should be a list (now class: %s)", 
            paste( class( value ), collapse = ", " ) ) )
        
    }   
    
    value_is_numeric <- unlist( lapply( X = value, 
        FUN = is.numeric ) )
    
    if( !all( value_is_numeric ) ){
        stop( sprintf( 
            "Each item in 'value' should be numeric value(s) (not the case for item %s)", 
            paste( (1:length( value ))[ !value_is_numeric ], 
            collapse = " and " ) ) )
    }   
    
    rm(value_is_numeric)
        
    if( focus_mode == "gw" ){
        value_expect <- c( value_expect, "years_interval" )
        
        if( !("years_interval" %in% names( value )) ){
            value <- c( value, list( "years_interval" = 1L ) )
        }   
    }   
    
    if( length( value ) != length( value_expect ) ){
        stop( sprintf( 
            "Argument 'value' should be a numeric-vector of length %s (now length: %s)", 
            length( value_expect ), length( value ) ) )
    }   
    
    test_value_expect <- value_expect %in% names( value )
    
    if( any( !test_value_expect ) ){
        stop( sprintf( 
            "The following labels are missing in 'value': %s", 
            paste( value_expect[ !test_value_expect ], 
                collapse = ", " ) ) )
    }   
    
    if( !is.logical( keep0conc ) ){
        stop( sprintf(
            "'keep0conc' should be a single logical value. Now class %s", 
            paste( class( keep0conc ), collapse = ", " ) 
        ) ) 
    }   
    
    if( !(length( keep0conc ) == 1L) ){
        stop( sprintf(
            "'keep0conc' should be a single logical value. Now length %s", 
            length( keep0conc ) 
        ) ) 
    }   
    
    
    
    #   Mass unit: 1 = micrograms, 2 = milligrams, 3 = grams, 
    #   4 = kilograms
    massunits <- as.numeric( rmacroliteGet1Param( 
        x    = x, 
        pTag = "MASSUNITS\t%s", 
        type = "OPTIONS" ) )
    
    #   Coefficient to convert to g active substance per ha
    if( massunits == 1L ){
        massunit_per_g <- 1000000 
        
    }else if( massunits == 2L ){
        massunit_per_g <- 1000
        
    }else if( massunits == 3L ){
        massunit_per_g <- 1
        
    }else if( massunits == 4L ){
        massunit_per_g <- 1/1000
        
    }else{
        stop( sprintf( 
            "Unknown value for MASSUNITS (%s) in the par file. Expects 1, 2, 3 or 4.", 
            massunits
        ) ) 
    }   
    
    
    
    if( focus_mode == "gw" ){
        
        if( length( value[[ "years_interval" ]] ) != 1L ){
            stop( sprintf( 
                "Length of value[[ 'years_interval' ]] should be 1. Now %s.", 
                length( value[[ "years_interval" ]] ) ) )
        }   
        
        if( (value[[ "years_interval" ]] %% 1) != 0 ){
            stop( sprintf( 
                "value[[ 'years_interval' ]] should be an integer. Now %s.", 
                value[[ "years_interval" ]] ) )
        }   
        
        if( value[[ "years_interval" ]] < 1L ){
            stop( sprintf( 
                "value[[ 'years_interval' ]] should be >= 1. Now %s.", 
                value[[ "years_interval" ]] ) )
        }   
        
        if( value[[ "years_interval" ]] > 3L ){
            warning( sprintf( 
                "value[[ 'years_interval' ]] is > 3 (%s). This is not supported and error may (silently) occur.", 
                value[[ "years_interval" ]] ) )
        }   
        
        tot_nb_yrs <- 
            6L + 20L * value[[ "years_interval" ]] 
        
        nb_irr_per_yr <- max( c( 
            length( value[[ "g_as_per_ha" ]] ), 
            length( value[[ "app_j_day" ]] ), 
            length( value[[ "f_int" ]] ) ) )
        
        for( v in c( "g_as_per_ha", "app_j_day", "f_int" ) ){
            if( length( value[[ v ]] ) != nb_irr_per_yr ){
                if( length( value[[ v ]] ) == 1L ){
                    value[[ v ]] <- rep( 
                        x     = value[[ v ]], 
                        times = nb_irr_per_yr )
                }else{
                    stop( sprintf( 
                        "Length of value[[ '%s' ]] should 1 or %s (the number of irrigation per year deduced from 'value'. Now length is %s.", 
                        v, nb_irr_per_yr, length( value[[ v ]] ) ) )
                }   
            }   
        }   
        
        #   Irrigation amount [mm]
        amir <- unique( as.numeric( rmacroliteGet1Param( 
            x    = x, 
            pTag = "AMIR\t%s", 
            type = "IRRIGATION PARAMETERS" ) ) )
        
        if( length( amir ) != 1L ){
            stop( sprintf(
                "'x' contains more than 1 unique value (%s) for the irrigation amount, AMIR. AMIR should be the same for all irrigation events when 'focus_mode' is 'gw'", 
                length( amir )
            ) ) 
        }   
        
        #   Convert [g as/ ha] to [massunits/m3]
        conci <- (value[[ "g_as_per_ha" ]] * massunit_per_g) / (10000 * (amir/1000))
        # (g_as_per_ha [g/ha] * massunit_per_g [massunits/g]) / (10000 [m2/ha] * (amir [mm]/1000 [mm/m]))
        
        
        
        #   Fetch the current end- and start-dates
        #   Replace the end-date in ENDDATE and METPERIOD and DRIVINGPERIOD
        sim_period <- rmacroliteSimPeriod( x = x )
        
        year_start <- format.POSIXct( 
            x      = sim_period[[ "sim" ]][ "start" ], 
            format = "%Y" )
        
        year_end <- format.POSIXct( 
            x      = sim_period[[ "sim" ]][ "end" ], 
            format = "%Y" )
        
        year_end_met <- format.POSIXct( 
            x      = sim_period[[ "metPeriod" ]][ "end" ], 
            format = "%Y" )
        
        year_end_new <- as.integer( year_start ) + tot_nb_yrs
        
        .end <- rmacroliteGet1Param( 
             x    = x, 
             pTag = "ENDDATE\t%s", 
             type = "SETUP" 
        )   
        
        if( !grepl( x = .end, pattern = year_end, fixed = TRUE ) ){
            stop( sprintf( 
                "Cannot find the estimated end-year (%s) in ENDDATE ('%s')", 
                year_end, .end ) ) 
        }else{
            .end <- gsub( x = .end, pattern = year_end, 
                replacement = as.character( year_end_new ), 
                fixed = TRUE )
        }   
        
        x <- rmacroliteChange1Param( 
            x     = x, 
            pTag  = "ENDDATE\t%s", 
            type  = "SETUP", 
            value = .end ) 
        
        metperiod <- rmacroliteGet1Param( 
             x    = x, 
             pTag = "METPERIOD\t%s", 
             type = "SETUP" 
        )   
        
        if( !grepl( x = metperiod, pattern = year_end_met, fixed = TRUE ) ){
            stop( sprintf( 
                "Cannot find the estimated end-year (%s) in METPERIOD ('%s')", 
                year_end_met, metperiod ) ) 
        }else{
            metperiod <- gsub( x = metperiod, 
                pattern = year_end_met, 
                replacement = as.character( year_end_new ), 
                fixed = TRUE )
        }   
        
        x <- rmacroliteChange1Param( 
            x     = x, 
            pTag  = "METPERIOD\t%s", 
            type  = "SETUP", 
            value = metperiod ) 
        
        #   Change CHAPAR
        #   CHAPAR to 0 when same irrigation every year
        #   CHAPAR to 1 when different irrigation
        if( value[[ "years_interval" ]] == 1L ){
            x <- rmacroliteChange1Param( 
                x     = x, 
                pTag  = "CHAPAR\t%s", 
                type  = "OPTIONS", 
                value = 0 ) 
        }else{
            x <- rmacroliteChange1Param( 
                x     = x, 
                pTag  = "CHAPAR\t%s", 
                type  = "OPTIONS", 
                value = 1 ) 
        }   
        
        
        
        #   Find out which years the substance is applied
        year_with_appln <- c( TRUE, rep( x = FALSE, 
            times = (value[[ "years_interval" ]] - 1L) ) )
        
        year_with_appln <- rep( x = year_with_appln, 
            times = ceiling( tot_nb_yrs / 
                value[[ "years_interval" ]] ) )
        
        year_with_appln <- year_with_appln[ 1:tot_nb_yrs ] 
        
        
        
        #   Format the new irrigation parameters for GW
        irrigation <- c(
            "*******************************", 
            "IRRIGATION PARAMETERS", 
            sprintf( "IRRSAME\t%s", ifelse( 
                test = value[[ "years_interval" ]] == 1L, 
                yes  = "True", 
                no   = "False" ) ), 
            "CRITDEF\t-1", 
            sprintf( "IRRYEARS\t%s", tot_nb_yrs ), 
            unlist( lapply(
                X   = 1L:tot_nb_yrs, 
                FUN = function(i){
                    return( c(
                        sprintf( "IRRYEAR\t%s", i ), 
                        sprintf( "NIRRIGATIONS\t%s", nb_irr_per_yr ), 
                        unlist( lapply(
                            X   = 1L:nb_irr_per_yr, 
                            FUN = function(j){ 
                                if( year_with_appln[ i ] ){
                                    conci_j  <- conci[ j ]
                                    irrday_j <- value[[ "app_j_day" ]][ j ] 
                                }else{
                                    conci_j  <- 0 
                                    irrday_j <- 1 
                                }   
                                
                                return( c(
                                    sprintf( "IRRIGNO\t%s", j ),   
                                    sprintf( "IRRDAY\t%s", irrday_j ), 
                                    "IRRSTART\t9", 
                                    "IRREND\t9.2", 
                                    sprintf( "AMIR\t%s", amir ), 
                                    sprintf( "CONCI\t%s", conci_j ), 
                                    sprintf( "ZFINT\t%s", value[[ "f_int" ]][ j ] ) 
                                ) ) 
                            } 
                        ) ) 
                    ) ) 
                    
                } 
            ) ) 
        )   
        
        irrigation <- data.frame(
            "parFile"   = irrigation, 
            "category"  = "IRRIGATION PARAMETERS", 
            stringsAsFactors = FALSE 
        )   
        
        #   Replace the old irrigation parameters in 'x' 
        #   by the new one
        is_irr_par <- x[[ "par" ]][, "category" ] == "IRRIGATION PARAMETERS"
        
        x[[ "par" ]] <- rbind(
            x[[ "par" ]][ 1L:(min(which(is_irr_par))-1L), ], 
            irrigation, 
            x[[ "par" ]][ (max(which(is_irr_par))+1L):nrow(x[[ "par" ]]), ] 
        )   
        
    }else{
        conc_in_irr <- as.numeric( rmacroliteGet1Param( 
            x    = x, 
            pTag = "CONCI\t%s", 
            type = "IRRIGATION PARAMETERS" ) ) 
        
        conc_is_zero <- conc_in_irr == 0
            
        # if( keep0conc ){
            # n_expected <- length( conc_in_irr[ !conc_is_zero ] )
        # }else{
            # n_expected <- length( conc_in_irr )
        # }   
        
        n_expected <- length( conc_in_irr )
        
        for( v in value_expect ){
            n_provided <- length( value[[ v ]] )
            
            if( (n_expected %% n_provided) != 0 ){
                stop( sprintf(
                    "value[['%s']] should be a multiple of the number of relevant irrigation events, %s%s", 
                    v, n_expected, 
                    ifelse( 
                        test = keep0conc, 
                        yes  = sprintf( 
                            "(number of irr minus number of zero-conc irr; %s - %s)", 
                            length( conc_in_irr ), 
                            sum(conc_is_zero) ), 
                        no   = sprintf( 
                            "(number of irr; %s)", 
                            length( conc_in_irr ) ) ) ) ) 
            }else{
                value[[ v ]] <- rep( x = value[[ v ]], 
                    times = (n_expected %/% n_provided) )
            }   
        }   
        
        #   Irrigation amount [mm]
        amir <- as.numeric( rmacroliteGet1Param( 
            x    = x, 
            pTag = "AMIR\t%s", 
            type = "IRRIGATION PARAMETERS" ) ) 
        
        #   Convert [g as/ ha] to [massunits/m3]
        conci <- (value[[ "g_as_per_ha" ]] * massunit_per_g) / (10000 * (amir/1000))
        # (g_as_per_ha [g/ha] * massunit_per_g [massunits/g]) / (10000 [m2/ha] * (amir [mm]/1000 [mm/m]))
        
        conci <- as.numeric( conci ) 
        
        if( keep0conc & any( conc_is_zero ) ){
            conci[ conc_is_zero ] <- rep( 0, 
                times = length( conci[ conc_is_zero ] ) ) 
        }   
        
        n_irr <- length( amir )
        
        #   Set the concentration to 'conci' for all irrigation events
        x <- rmacroliteChangeParam( x = x, p = data.frame( 
            "tag"    = rep( "CONCI\t%s", n_irr ), 
            "values" = gsub( 
                x           = format( conci, scientific = FALSE ), 
                pattern     = " ", 
                replacement = "", 
                fixed       = TRUE ),  
            "type"   = rep( "IRRIGATION PARAMETERS", n_irr ), 
            "set_id" = rep( 1L, n_irr ), 
            "tagNb"  = 1:n_irr, 
            stringsAsFactors = FALSE ) )[[ 1L ]]
        
        if( keep0conc & any( conc_is_zero ) ){
            irrday0 <- as.numeric( rmacroliteGet1Param( 
                x    = x, 
                pTag = "IRRDAY\t%s", 
                type = "IRRIGATION PARAMETERS" ) ) 
                
            zfint0 <- as.numeric( rmacroliteGet1Param( 
                x    = x, 
                pTag = "ZFINT\t%s", 
                type = "IRRIGATION PARAMETERS" ) ) 
            
            value[[ "app_j_day" ]][ conc_is_zero ] <- 
                irrday0[ conc_is_zero ] 
            
            value[[ "f_int" ]][ conc_is_zero ] <- 
                zfint0[ conc_is_zero ] 
            
            rm( irrday0, zfint0 )
        }   
        
        #   Set the irrigation Julian day for all irrigation events
        x <- rmacroliteChangeParam( x = x, p = data.frame( 
            "tag"    = rep( "IRRDAY\t%s", n_irr ), 
            "values" = as.numeric( value[[ "app_j_day" ]] ), 
            "type"   = rep( "IRRIGATION PARAMETERS", n_irr ), 
            "set_id" = rep( 1L, n_irr ), 
            "tagNb"  = 1:n_irr, 
            stringsAsFactors = FALSE ) )[[ 1L ]]
        
        #   Set the fraction intercepted for all irrigation events
        x <- rmacroliteChangeParam( x = x, p = data.frame( 
            "tag"    = rep( "ZFINT\t%s", n_irr ), 
            "values" = as.numeric( value[[ "f_int" ]] ), 
            "type"   = rep( "IRRIGATION PARAMETERS", n_irr ), 
            "set_id" = rep( 1L, n_irr ), 
            "tagNb"  = 1:n_irr, 
            stringsAsFactors = FALSE ) )[[ 1L ]]
    }   
    
    return( x ) 
}   




# rmacroliteMacroVersion ===================================

#' Fetch MACRO version from the folder where it is installed
#'
#' Fetch MACRO version from the folder where it is installed
#'
#'
#'@param path
#'  A single character-strings. Path to the directory where 
#'  MACRO (or MACRO In FOCUS) is installed and where the 
#'  model version is to be found. When \code{path} equal to 
#'  \code{character(0)} (the default), it is retrieved 
#'  automatically using 
#'  \code{\link[rmacrolite]{rmacroliteGetModelVar}[["path"]]}.
#'
#'@param \dots
#'  Additional parameters passed to specific methods. 
#'  Currently not used.
#' 
#'
#'@return
#'  TO BE WRITTEN
#' 
#'
#'@rdname rmacroliteMacroVersion-methods
#'@aliases rmacroliteMacroVersion
#'
#'@example inst/examples/rmacroliteMacroVersion-example.r
#'
#'@export 
#'
#'@docType methods
#'
rmacroliteMacroVersion <- function( path = character(0), ... ){ 
    UseMethod( "rmacroliteMacroVersion", path )
}   



#'@rdname rmacroliteMacroVersion-methods
#'
#'@method rmacroliteMacroVersion character
#'
#'@export 
#'
rmacroliteMacroVersion.character <- function( 
    path = character(0),  
    ... 
){  if( length( path ) == 0L ){
        path <- rmacroliteGetModelVar()[[ "path" ]]
        
    }else if( length( path ) > 1L ){ 
        stop( sprintf( 
            "'path' should be a single character string. Now length %s.", 
            length( path ) 
        ) )
    }   
    
    if( !file.exists( path ) ){
        stop( sprintf( 
            "The folder indicated in 'path' does not exists (%s).", 
            path 
        ) )
    }   
    
    expected_files <- c( "versionnum.dat", 
        "MACRO 5.2.exe.config" ) 
    
    expected_files_exists <- file.exists( file.path( path, 
        expected_files ) ) 
    
    names( expected_files_exists ) <- expected_files 
    
    if( expected_files_exists[ "versionnum.dat" ] ){
        version_file <- readLines( con = file.path( path, 
            "versionnum.dat" ) )
        
        version_file <- strsplit( x = version_file, 
            split = " = " )
        
        names( version_file ) <- unlist( lapply(
            X   = version_file, 
            FUN = function( v ){ return( v[ 1L ] ) }
        ) ) 
        
        out <- c(
            "name"       = version_file[[ 1L ]], 
            "model_v"    = version_file[[ "Model" ]][ 2L ], 
            "shell_v"    = version_file[[ "Shell" ]][ 2L ], 
            "database_v" = version_file[[ "Database" ]][ 2L ] ) 
        
    }else if( expected_files_exists[ "MACRO 5.2.exe.config" ] ){
        out <- c(
            "name"    = "MACRO", 
            "model_v" = "5.2" ) 
        
    }else{
        out <- NA_character_ 
    }   
    
    return( out ) 
}   

    # rmacroliteMacroVersion( path = "C:/Program Files (x86)/MACRO52" )
    # rmacroliteMacroVersion( path = "C:/swash/macro" )
    # rmacroliteMacroVersion()

