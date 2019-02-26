
library( "rmacrolite" ) 

# # Setup MACRO directory (if needed)
# rmacroliteSetModelVar( "C:/swash/macro" )



# ==========================================================
# Example 1: MACRO In FOCUS simulation with a single annual 
#            application
# ==========================================================

#   Path to an example par-file
par_file_path_1appln <- system.file( "par-files", 
    "chat_winCer_GW-X_900gHa_d182.par", 
    package = "rmacrolite" ) 

#   Import the example par-file
par_file_1appln <- rmacroliteImportParFile( 
    file = par_file_path_1appln ) 



#   Fetch and inspect the current parametrization
out1 <- rmacroliteApplications( x = par_file_1appln ) 

unique( out1 ) 
      # g_as_per_ha app_j_day L_sprayer_per_ha f_int
    # 1         900       182             1000     0

nrow( out1 ) 
    # [1] 26



#   Copy the par-file and modify its application parameters
par_file_1appln_b <- par_file_1appln

rmacroliteApplications( x = par_file_1appln_b ) <- list(
    "g_as_per_ha"  = 999, 
    "app_j_day"    = 183, 
    "f_int"        = 0.01 ) 

#   Check
out1b <- rmacroliteApplications( x = par_file_1appln_b ) 

unique( out1b ) 
      # g_as_per_ha app_j_day L_sprayer_per_ha f_int
    # 1         999       183             1000  0.01

nrow( out1b ) 
    # [1] 26



#   Use a method that re-parametrise completely the applications, 
#   like MACRO In FOCUS, for the same result as above
par_file_1appln_c <- par_file_1appln

rmacroliteApplications( x = par_file_1appln_c, 
    focus_mode = "gw" ) <- list(
    "g_as_per_ha"  = 999, 
    "app_j_day"    = 183, 
    "f_int"        = 0.01 ) 

#   Check
out1c <- rmacroliteApplications( x = par_file_1appln_c ) 



#   Internal control
test_1 <- identical( 
    as.numeric( unlist( unique( out1b ) ) ), 
    c( 999, 183, 1000, 0.01 ) )   
if( !test_1 ){ 
    stop( "Example 1 of rmacroliteApplications()<- failed" ) } 

if( !all( all.equal( out1b, out1c, check.attributes = FALSE ) == TRUE ) ){
    stop( "rmacroliteApplications() with focus_mode = 'gw' seems to have failed (1 application every year)" )
}   

#   Clean-up
rm( par_file_path_1appln, par_file_1appln, par_file_1appln_b, 
    out1, out1b, test_1, par_file_1appln_c, out1c )



# ==========================================================
# Example 2: MACRO In FOCUS simulation with two annual 
#            applications
# ==========================================================

#   Path to the example par-file
par_file_path_2appln <- system.file( "par-files", 
    "chat_winCer_GW-X_2appln.par", 
    package = "rmacrolite" ) 

#   Import the example par-file
par_file_2appln <- rmacroliteImportParFile( 
    file = par_file_path_2appln ) 



#   Fetch and inspect the current parametrization
out2 <- rmacroliteApplications( x = par_file_2appln ) 

unique( out2 ) 
  # g_as_per_ha app_j_day L_sprayer_per_ha f_int
# 1         900       182             1000     0
# 2         850       189             1000     0

nrow( out2 ) 
    # [1] 52



#   Copy the par-file and modify its application parameters
par_file_2appln_b <- par_file_2appln

rmacroliteApplications( x = par_file_2appln_b ) <- list(
    "g_as_per_ha"  = c( 999, 899 ), 
    "app_j_day"    = c( 183, 190 ), 
    "f_int"        = 0.01 ) 

#   Check
out2b <- rmacroliteApplications( x = par_file_2appln_b ) 

unique( out2b ) 
  # g_as_per_ha app_j_day L_sprayer_per_ha f_int
# 1         999       183             1000  0.01
# 2         899       190             1000  0.01

nrow( out2b ) 
    # [1] 52



#   Use a method that re-parametrise completely the applications, 
#   like MACRO In FOCUS, for the same result as above
par_file_2appln_c <- par_file_2appln

rmacroliteApplications( x = par_file_2appln_c, 
    focus_mode = "gw" ) <- list(
    "g_as_per_ha"  = c( 999, 899 ), 
    "app_j_day"    = c( 183, 190 ), 
    "f_int"        = 0.01 ) 

#   Check
out2c <- rmacroliteApplications( x = par_file_2appln_c ) 



#   Internal control
test_2 <- identical( 
    as.numeric( unlist( unique( out2b ) ) ), 
    c( 999, 899, 183, 190, 1000, 1000, 0.01, 0.01 ) )   
if( !test_2 ){ 
    stop( "Example 2 of rmacroliteApplications()<- failed" ) } 

if( !all( all.equal( out2b, out2c, check.attributes = FALSE ) == TRUE ) ){
    stop( "rmacroliteApplications() with focus_mode = 'gw' seems to have failed (2 applications every year)" )
}   

#   Clean-up
rm( par_file_path_2appln, par_file_2appln, par_file_2appln_b, 
    out2, out2b, test_2, par_file_2appln_c, out2c )



# ==========================================================
# Example 3: MACRO In FOCUS simulation with a single biennial 
#            application
# ==========================================================

#   Path to the example par-file
par_file_path_biennial <- system.file( "par-files", 
    "chat_pot_GW-D_1kgHa_d119_biennial.par", 
    package = "rmacrolite" ) 

#   Import the example par-file
par_file_biennial <- rmacroliteImportParFile( 
    file = par_file_path_biennial ) 



#   Fetch and inspect the current parametrization
out3 <- rmacroliteApplications( x = par_file_biennial ) 

unique( out3 ) 
      # g_as_per_ha app_j_day L_sprayer_per_ha f_int
    # 1        1000       119             1000     0
    # 2           0         1             1000     0

nrow( out3 ) 
    # [1] 46



#   Copy the par-file and modify its application parameters
par_file_biennial_c <- par_file_biennial_b <- par_file_biennial

#   Variant 1: solute-free irriations are preserved
rmacroliteApplications( x = par_file_biennial_b ) <- list(
    "g_as_per_ha"  = 999, 
    "app_j_day"    = 183, 
    "f_int"        = 0.01 ) 

#   Check
out3b <- rmacroliteApplications( x = par_file_biennial_b ) 

unique( out3b ) 
  # g_as_per_ha app_j_day L_sprayer_per_ha f_int
# 1         999       183             1000  0.01
# 2           0         1             1000  0.00

nrow( out3b ) 
    # [1] 46

#   Internal control
test_3 <- identical( 
    as.numeric( unlist( unique( out3b ) ) ), 
    c( 999, 0, 183, 1, 1000, 1000, 0.01, 0.00 ) )   
if( !test_3 ){ 
    stop( "Example 3 of rmacroliteApplications()<- failed" ) } 



#   Variant 2: solute-free irrigations are not preserved
#   like it is the case for MACRO In FOCUS with multiple years 
#   interval between applications
rmacroliteApplications( x = par_file_biennial_c, 
    keep0conc = FALSE ) <- list(
        "g_as_per_ha"  = c( 999,   1 ), 
        "app_j_day"    = c( 183,   2 ), 
        "f_int"        = c( 0.01,  0 ) ) 

#   Check
out3c <- rmacroliteApplications( x = par_file_biennial_c ) 

unique( out3c ) 
  # g_as_per_ha app_j_day L_sprayer_per_ha f_int
# 1         999       183             1000  0.01
# 2           1         2             1000  0.00

nrow( out3c ) 
    # [1] 46
    
    
    
#   Use a method that re-parametrise completely the applications, 
#   like MACRO In FOCUS, for the same result as above
par_file_biennial_d <- par_file_biennial

rmacroliteApplications( x = par_file_biennial_d, 
    focus_mode = "gw" ) <- list(
        "g_as_per_ha"    = 999, 
        "app_j_day"      = 183, 
        "f_int"          = 0, 
        "years_interval" = 2 ) 

#   Check
out3d <- rmacroliteApplications( x = par_file_biennial_d ) 

#   Re-run case 3b to compare it with 3d, the expected 
#   result from MACRO In FOCUS
rmacroliteApplications( x = par_file_biennial_b ) <- list(
    "g_as_per_ha"  = 999, 
    "app_j_day"    = 183, 
    "f_int"        = 0 ) 

#   Check
out3b <- rmacroliteApplications( x = par_file_biennial_b ) 



#   Internal control
test_3c <- identical( 
    as.numeric( unlist( unique( out3c ) ) ), 
    c( 999, 1, 183, 2, 1000, 1000, 0.01, 0.00 ) )   
if( !test_3c ){ 
    stop( "Example 3c of rmacroliteApplications()<- failed" ) } 

if( !all( all.equal( out3b, out3d, check.attributes = FALSE ) == TRUE ) ){
    stop( "rmacroliteApplications() with focus_mode = 'gw' seems to have failed (1 application every 2 years)" )
}   



#   Clean-up
rm( par_file_path_biennial, par_file_biennial, par_file_biennial_b, 
    out3, out3b, par_file_biennial_c, out3c, test_3, test_3c, 
    par_file_biennial_d, out3d )

