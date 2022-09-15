# ==========================================================================
# a class to store network component
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.nebula <- 
  setClass("nebula", 
           contains = character(),
           representation = 
             representation(parent_nebula = "list",
                            child_nebulae = "list"
                            ),
           prototype = NULL
           )
# ==========================================================================
# method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("parent_nebula", 
          signature = c(x = "ANY"),
          function(x){ x@parent_nebula })
setReplaceMethod("parent_nebula", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, parent_nebula = value)
                 })
setMethod("child_nebulae", 
          signature = c(x = "ANY"),
          function(x){ x@child_nebulae })
setReplaceMethod("child_nebulae", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, child_nebulae = value)
                 })
