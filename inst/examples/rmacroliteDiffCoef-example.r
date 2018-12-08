
library( "rmacrolite" ) 

# # Setup MACRO directory (if needed)
# rmacroliteSetModelVar( "C:/swash/macro" )



#   Path to an example par-file
par_file_path <- system.file( "par-files", 
    "chat_winCer_GW-X_900gHa_d182.par", 
    package = "rmacrolite" ) 

#   Import the example par-file
par_file <- rmacroliteImportParFile( 
    file = par_file_path ) 



#   Fetch the current parametrization
rmacroliteDiffCoef( x = par_file ) 
    # [1] 5.2e-10

#   Modify the parameter
par_file2 <- par_file
rmacroliteDiffCoef( x = par_file2 ) <- 5.00E-10

#   Check the result
dc <- rmacroliteDiffCoef( x = par_file2 )
dc 
    # [1] 5e-10

#   Internal control
if(dc != 5.00E-10 ){ 
    stop( "Test of rmacroliteDiffCoef() failed" ) } 

#   Clean-up
rm( par_file_path, par_file, par_file2, dc  )

