# ==========================================================================
# a class for statistic analysis
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.statistic_set <- 
  setClass("statistic_set", 
           contains = character(),
           representation = 
             representation(design_matrix = "matrix",
                            contrast_matrix = "matrix",
                            dataset = "ANY",
                            top_table = "list"
                            ),
           prototype = NULL
           )
# ==========================================================================
# method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("statistic_set", 
          signature = c(x = "ANY"),
          function(x){ x@statistic_set })
setReplaceMethod("statistic_set", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, statistic_set = value)
                 })
## ------------------------------------- 
setMethod("design_matrix", 
          signature = c(x = "ANY"),
          function(x){ x@design_matrix })
setReplaceMethod("design_matrix", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, design_matrix = value)
                 })
## ------------------------------------- 
setMethod("contrast_matrix", 
          signature = c(x = "ANY"),
          function(x){ x@contrast_matrix })
setReplaceMethod("contrast_matrix", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, contrast_matrix = value)
                 })
## ------------------------------------- 
setMethod("top_table", 
          signature = c(x = "ANY"),
          function(x){ x@top_table })
setReplaceMethod("top_table", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, top_table = value)
                 })
