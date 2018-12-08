
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
rmacroliteCropUptF( x = par_file ) 
    # [1] 0.9

#   Modify the parameter
par_file2 <- par_file
rmacroliteCropUptF( x = par_file2 ) <- 0.5 

#   Check the result
cuf <- rmacroliteCropUptF( x = par_file2 )
cuf 
    # [1] 0.5

#   Internal control
if(cuf != 0.5 ){ 
    stop( "Test of rmacroliteCropUptF() failed" ) } 

#   Clean-up
rm( par_file_path, par_file, par_file2, cuf  )

