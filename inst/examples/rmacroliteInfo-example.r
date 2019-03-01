
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



#   Fetch the current information
#   Note: will only work on MACRO In FOCUS par-files
rmacroliteInfo( x = par_file ) 

                         # output_file 
    # "C:\\swash\\macro\\macro001.bin" 
                                # type 
                            # "parent" 
                            # compound 
                              # "GW-X"

#   Modify the parameter
par_file2 <- par_file

new_info <- list( 
    "output_file" = "C:/path/to/macro002.bin", 
    "type"        = "metabolite", 
    "compound"    = "GW-Z" )

rmacroliteInfo( x = par_file2 ) <- new_info 

#   NOTE: Only the meta-information are changed, not the 
#         actual parameters, so the example above is 
#         pointless.

#   Check the result
info <- rmacroliteInfo( x = par_file2 )
info 

if( !identical( new_info, info ) ){
    stop( "Test of rmacroliteInfo() failed" ) }

#   Clean-up
rm( par_file_path, par_file, par_file2, info, new_info  )

