
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
