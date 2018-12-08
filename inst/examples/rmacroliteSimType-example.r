
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
rmacroliteSimType( x = par_file ) 
    # $`type`
    # [1] 1

    # $drivingfile
    # [1] "C:\\swash\\macro\\macro999.bin"



#   Modify the parameter
par_file2 <- par_file

new_type <- list( "type" = 4L, "f_conv" = 0.55, 
    "drivingfile" = "C:\\swash\\macro\\macro998.bin" ) 

#   Note: type 4 is metabolite, intermediate output

rmacroliteSimType( x = par_file2 ) <- new_type 

check_type <- rmacroliteSimType( x = par_file2 ) 

check_type 
    # $`type`
    # [1] 4

    # $drivingfile
    # [1] "C:\\swash\\macro\\macro998.bin"

#   Fetch the conversion factor
fc <- as.numeric( rmacroliteGet1Param( 
    x    = par_file2, 
    pTag = "FCONVERT\t%s", 
    type = "SOLUTE PARAMETERS" ) )

fc
    # [1] 0.55




#   Internal control
if( !identical( new_type[ names( new_type ) != 
    "f_conv"], check_type ) ){ 
    stop( "Test of rmacroliteSimType() failed" ) } 

if( fc != 0.55 ){ 
    stop( "Test of rmacroliteSimType() failed (f_conv)" ) } 

#   Clean-up
rm( par_file_path, par_file, par_file2, new_type, check_type, 
    fc )

