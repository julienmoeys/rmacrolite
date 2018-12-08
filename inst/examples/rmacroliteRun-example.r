
library( "rmacrolite" ) 

# # Setup MACRO directory (if needed)
# rmacroliteSetModelVar( "C:/swash/macro" )



# Import a par-file

#   Path to an example par-file
par_file_path <- system.file( "par-files", 
    "chat_winCer_GW-X_900gHa_d182_1910-1911.par", 
    package = "rmacrolite" )  

#   Import the example par-file
par_file <- rmacroliteImportParFile( 
    file = par_file_path ) 



# Run the simulation
out <- rmacroliteRun( x = par_file ) 



# Inspect the results

#   View the water and solute balance as output by MACRO
#   attributes( out ) # full list of attributes
attr( x = out, which = "waterSoluteBalance" )

library( "macroutils2" )

macroPlot( x = out[, c( "Date", "WOUT_99-100_100", 
    "SFLOW_99-100_100" ) ], gui = FALSE, subPlots = TRUE ) 


