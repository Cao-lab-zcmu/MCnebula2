# ==========================================================================
# msframe: class based on data.frame
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.msframe <- 
  setClass("msframe", 
           contains = "subscript",
           representation = 
             representation(entity = "data.frame"),
           prototype = NULL
  )
# ==========================================================================
# methods
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("show", 
          signature = c(object = "msframe"),
          function(object){
            cat( "A class of \"msframe\" of", subscript(object), "\n")
          })
setMethod("msframe", 
          signature = c(x = "ANY"),
          function(x){ x@msframe })
setReplaceMethod("msframe", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, msframe = value)
                 })
## ------------------------------------- 
setMethod("latest", 
          signature = c(x = "msframe"),
          function(x){
            tibble::as_tibble(entity(x))
          })
## ------------------------------------- 
setMethod("entity", 
          signature = c(x = "msframe"),
          function(x){ x@entity })
setReplaceMethod("entity", 
                 signature = c(x = "msframe"),
                 function(x, value){
                   initialize(x, entity = value)
                 })
