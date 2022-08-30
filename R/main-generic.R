# ==========================================================================
# Generic for main method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setGeneric("initialize_mcnebula", 
           signature = c(ANY = "x", "character" = "sirius_version"),
           function(x, sirius_version) standardGeneric("initialize_mcnebula"))
setGeneric("collate_structure", 
           signature = c(mcnebula = "x"),
           function(x) standardGeneric("collate_structure"))
setGeneric("collate_ppcp", 
           signature = c(mcnebula = "x"),
           function(x) standardGeneric("collate_ppcp"))

