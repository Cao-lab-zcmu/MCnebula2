# ==========================================================================
# Generic for main method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setGeneric("initialize_mcnebula", 
           signature = c(ANY = "x", "character" = "sirius_version"),
           function(x, sirius_version) standardGeneric("initialize_mcnebula"))
setGeneric("collate_data", 
           signature = c(mcnebula = "x", "character" = "subscript",
                         "function" = "fun_collate"),
           function(x, subscript, fun_collate, ...) standardGeneric("collate_data"))
setGeneric("filter_formula", 
           signature = c(mcnebula = "x",
                         "function" = "fun_filter"),
           function(x, fun_filter, ...) standardGeneric("filter_formula"))
setGeneric("filter_structure", 
           signature = c(mcnebula = "x",
                         "function" = "fun_filter"),
           function(x, fun_filter, ...) standardGeneric("filter_structure"))
setGeneric("create_reference", 
           signature = c(mcnebula = "x",
                         "character" = "from_reference",
                         "data.frame" = "data"
                         ),
           function(x, from_reference, data) standardGeneric("create_reference"))
setGeneric("filter_ppcp", 
           signature = c(mcnebula = "x",
                         "function" = "fun_filter",
                         "logical" = "by_reference"
                         ),
           function(x, fun_filter, ..., by_reference)
             standardGeneric("filter_ppcp"))

