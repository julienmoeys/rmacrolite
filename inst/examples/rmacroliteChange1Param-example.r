
library( "rmacrolite" )

#   Path to an example par-file
par_file_path <- system.file( "par-files", 
    "chat_winCer_GW-X_900gHa_d182.par", 
    package = "rmacrolite" )  

#   Import the example par-file
par_file <- rmacroliteImportParFile( 
    file = par_file_path ) 

#   Current value of RUNID
as.numeric( rmacroliteGet1Param( 
     x    = par_file, 
     pTag = "RUNID\t%s", 
     type = "HEAD" ) )

#   Change the RUNID
par_file <- rmacroliteChange1Param( 
    x     = par_file, 
    pTag  = "RUNID\t%s", 
    type  = "HEAD", 
    value = 2L ) 

#   Check new value
new_run_id <- as.numeric( rmacroliteGet1Param( 
     x    = par_file, 
     pTag = "RUNID\t%s", 
     type = "HEAD" ) ) 

new_run_id

#   Internal checks
if( !(new_run_id == 2L) ){
    stop( "Test of rmacroliteChange1Param() failed" )
}   

#   Clean-up
rm( par_file_path, par_file, new_run_id )
