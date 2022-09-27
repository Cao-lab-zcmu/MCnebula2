# ==========================================================================
# VIRTUAL classes (sharing slots and methods)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setClass("dataset", 
         contains = character(),
         representation = 
           representation("VIRTUAL",
                          dataset = "list"
                          ),
         prototype = NULL
         )
setClass("reference", 
         contains = character(),
         representation = 
           representation("VIRTUAL",
                          reference = "list"
                          ),
         prototype = NULL
         )
setClass("backtrack", 
         contains = character(),
         representation = 
           representation("VIRTUAL",
                          backtrack = "list"
                          ),
         prototype = NULL
         )
setClass("subscript", 
         contains = character(),
         representation = 
           representation("VIRTUAL",
                          subscript = "character"
                          ),
         prototype = NULL
         )
# ==========================================================================
# validate
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setValidity("reference", 
            function(object){
              check <- vapply(reference(object), is.data.frame,
                              T, USE.NAMES = F)
              if (any(!check))
                "the elements in \"reference\" slot must be data.frame."
              else
                TRUE
            })
# ==========================================================================
# method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("dataset", "ANY",
          function(x){ x@dataset })
setReplaceMethod("dataset", "ANY",
                 function(x, value){
                   initialize(x, dataset = value)
                 })
## ------------------------------------- 
setMethod("add_dataset", 
          signature = c(x = "ANY", list = "list"),
          function(x, list){
            dataset <- c(list, dataset(x))
            dataset(x) <- list_unique_by_names(dataset)
            return(x)
          })
## ------------------------------------- 
setMethod("reference", "ANY",
          function(x){ x@reference })
setReplaceMethod("reference", "ANY",
                 function(x, value){
                   initialize(x, reference = value)
                 })
## ------------------------------------- 
setMethod("backtrack", "ANY",
          function(x){ x@backtrack })
setReplaceMethod("backtrack", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, backtrack = value)
                 })
## ------------------------------------- 
setMethod("subscript", "ANY",
          function(x){ x@subscript })
setReplaceMethod("subscript", "ANY",
                 function(x, value){
                   initialize(x, subscript = value)
                 })
## ------------------------------------- 

