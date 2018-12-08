
library( "rmacrolite" )

#   Path to an example par-file
par_file_path <- system.file( "par-files", 
    "chat_winCer_GW-X_900gHa_d182.par", 
    package = "rmacrolite" )  

#   Import the example par-file
par_file <- rmacroliteImportParFile( 
    file = par_file_path ) 

#   Current value of ZKD (Freundlich sorption)
as.numeric( rmacroliteGet1Param( 
     x    = par_file, 
     pTag = "ZKD\t%s\t%s", 
     type = "SOLUTE PARAMETERS" ) )

#   Organic matter content profile
oc <- as.numeric( rmacroliteGet1Param( 
     x    = par_file, 
     pTag = "ORGC\t%s\t%s", 
     type = "PROPERTIES" ) )

oc_factors <- oc / oc[ 1L ]

#   Change ZKD
par_file <- rmacroliteChangeParam( 
    x = par_file, 
    p = data.frame( 
        "tag"    = sprintf( "ZKD\t%s\t%s", 1L:6L, "%s" ), 
        "values" = 1 * oc_factors, # 1 is Kf for topsoil
        "type"   = rep( "SOLUTE PARAMETERS", 6L ), 
        "set_id" = rep( 1L, 6L ), 
        stringsAsFactors = FALSE ) )[[ 1L ]]

#   Check new value
new_zkd <- as.numeric( rmacroliteGet1Param( 
     x    = par_file, 
     pTag = "ZKD\t%s\t%s", 
     type = "SOLUTE PARAMETERS" ) )

new_zkd

#   Parameter variations for several parameter sets
p <- data.frame( 
    "tag"    = c( "ALPHA\t1\t%s", "ALPHA\t2\t%s", 
        "ZKD\t1\t%s", "ZKD\t2\t%s" ), 
    "type"   = c( "PHYSICAL PARAMETERS", "PHYSICAL PARAMETERS", 
        "SOLUTE PARAMETERS", "SOLUTE PARAMETERS" ), 
    "values" = c( 0.02, 0.02, 2, 2 ), 
    "set_id" = c( 1, 1, 2, 2 ), 
    stringsAsFactors = FALSE )   

par_file_list <- rmacroliteChangeParam( x = par_file, p = p )

class( par_file_list ) # Should be a list
length( par_file_list ) # Should be 2

# # Not run
# rmacroliteRun( x = par_file_list )

#   Internal checks
if( !all( abs( (1 * oc_factors) - new_zkd ) < 1e-15 ) ){
    stop( "Test of rmacroliteChangeParam() failed" )
}   

#   Clean-up
rm( par_file_path, par_file, oc, oc_factors, new_zkd, p, 
    par_file_list )

