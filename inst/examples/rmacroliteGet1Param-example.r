
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



#   Fetch one parameter (here percent of organic matter in 
#   each soil layer
oc <- rmacroliteGet1Param( 
    x    = par_file, 
    pTag = "ORGC\t%s\t%s", 
    type = "PROPERTIES" )

oc

    # [1] "1.39" "0.93" "0.7"  "0.3"  "0.3"  "0.27"
    # attr(,"layer")
    # [1] "1" "2" "3" "4" "5" "6"
    # attr(,"index")
    # [1] 133 134 135 136 137 138

#   The function does not know which parameters are numeric 
#   and which are character, the output may need to be 
#   manually converted:
as.numeric( oc )
    # [1] 1.39 0.93 0.70 0.30 0.30 0.27

#   Clean-up
rm( par_file_path, par_file, oc )
