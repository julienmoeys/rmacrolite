
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
rmacroliteRunId( x = par_file ) 
    # [1] 1

#   Modify the parameter
par_file2 <- par_file
rmacroliteRunId( x = par_file2 ) <- 555 

#   Check the result
rid <- rmacroliteRunId( x = par_file2 )
rid 
    # [1] 555 

#   Internal control
if( rid != 555 ){ 
    stop( "Test of rmacroliteRunId() failed" ) } 

#   Clean-up
rm( par_file_path, par_file, par_file2, rid  )

