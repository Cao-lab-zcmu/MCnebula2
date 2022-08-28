# ==========================================================================
# Generic for main method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setGeneric("initialize_mcnebula", 
           signature = c(ANY = "x", "character" = "sirius_version"),
           function(x, sirius_version) standardGeneric("initialize_mcnebula"))
