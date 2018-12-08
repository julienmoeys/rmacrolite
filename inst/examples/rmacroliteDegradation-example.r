
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
rmacroliteDegradation( x = par_file ) 
    # $`layer`
      # layer_no      dt50           k dt50_depth_f
    # 1        1  54.99998 0.012602680          1.0
    # 2        2 110.00000 0.006301338          0.5
    # 3        3 110.00000 0.006301338          0.5
    # 4        4 183.33332 0.003780803          0.3
    # 5        5       Inf 0.000000000          0.0
    # 6        6       Inf 0.000000000          0.0

    # $site
     # dt50_ref_temp        dt50_pf  exp_temp_resp exp_moist_resp 
             # 21.00           1.00           0.01           0.50

#   Modify the parameter
par_file2 <- par_file
rmacroliteDegradation( x = par_file2 ) <- c( "dt50" = 22, 
    "dt50_ref_temp" = 20, "dt50_pf" = 2, 
    "exp_temp_resp" = 0.079, "exp_moist_resp" = 0.49 )

#   Check the result
deg <- rmacroliteDegradation( x = par_file2 )
deg 
    # $`layer`
      # layer_no     dt50          k dt50_depth_f
    # 1        1 22.00000 0.03150669          1.0
    # 2        2 43.99999 0.01575335          0.5
    # 3        3 43.99999 0.01575335          0.5
    # 4        4 73.33331 0.00945201          0.3
    # 5        5      Inf 0.00000000          0.0
    # 6        6      Inf 0.00000000          0.0

    # $site
     # dt50_ref_temp        dt50_pf  exp_temp_resp exp_moist_resp 
            # 20.000          2.000          0.079          0.490

#   Internal controls
if( deg[["layer"]][ 1L, "dt50" ] == 22 ){ 
    stop( "Test of rmacroliteDegradation() failed (DT50)" ) } 

test2 <- identical( deg[["layer"]][, "dt50_depth_f" ], 
    c( 1, .5, .5, .3, 0, 0 ) )

if( !test2 ){  
    stop( "Test of rmacroliteDegradation() failed (DT50 depth factor)" ) } 

test3 <- identical( as.numeric( deg[["site"]] ), 
    c( 20, 2, 0.079, 0.49 ) )

if( !test3 ){  
    stop( "Test of rmacroliteDegradation() failed (site solute parameters)" ) } 

#   Clean-up
rm( par_file_path, par_file, par_file2, deg, test2, test3 )

