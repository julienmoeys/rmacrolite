
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
rmacroliteSorption( x = par_file ) 
    # $`layer`
      # layer_no oc_pc   kf kfoc   nf
    # 1        1  1.39 1.39  100 0.95
    # 2        2  0.93 0.93  100 0.95
    # 3        3  0.70 0.70  100 0.95
    # 4        4  0.30 0.30  100 0.95
    # 5        5  0.30 0.30  100 0.95
    # 6        6  0.27 0.27  100 0.95

    # $site
    # koc 
    # 100

#   Modify the parameter
par_file2 <- par_file
rmacroliteSorption( x = par_file2 ) <- c( "kfoc" = 200, 
    "nf" = 0.90 )

#   Check the result
ads <- rmacroliteSorption( x = par_file2 )
ads 
    # $`layer`
      # layer_no oc_pc   kf kfoc  nf
    # 1        1  1.39 2.78  200 0.9
    # 2        2  0.93 1.86  200 0.9
    # 3        3  0.70 1.40  200 0.9
    # 4        4  0.30 0.60  200 0.9
    # 5        5  0.30 0.60  200 0.9
    # 6        6  0.27 0.54  200 0.9

    # $site
    # koc 
    # 200

#   Internal control
if( !all( ads[[ "layer" ]][, "kfoc" ] == 200 ) ){ 
    stop( "Test of rmacroliteSorption() failed (kfoc, 1)" ) } 

if( ads[[ "site" ]][ "koc" ] != 200  ){ 
    stop( "Test of rmacroliteSorption() failed (kfoc, 2)" ) } 

if( !all( ads[[ "layer" ]][, "nf" ] == 0.9 ) ){ 
    stop( "Test of rmacroliteSorption() failed (nf)" ) } 

#   Clean-up
rm( par_file_path, par_file, par_file2, ads  )

