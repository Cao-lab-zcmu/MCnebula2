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
setClass("export", 
         contains = character(),
         representation = 
           representation("VIRTUAL",
                          export_path = "character",
                          export_name = "character"
                          ),
         prototype = NULL
         )
setClass("layerSet", 
         contains = character(),
         representation = 
           representation("VIRTUAL",
                          layers = "list"),
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
setMethod("export_name", 
          signature = c(x = "ANY"),
          function(x){ x@export_name })
setReplaceMethod("export_name", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, export_name = value)
                 })
setMethod("export_path", 
          signature = c(x = "ANY"),
          function(x){ x@export_path })
setReplaceMethod("export_path", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, export_path = value)
                 })
## ---------------------------------------------------------------------- 
setMethod("layers", 
          signature = c(x = "layerSet"),
          function(x){ x@layers })
setReplaceMethod("layers", 
                 signature = c(x = "layerSet"),
                 function(x, value){
                   initialize(x, layers = value)
                 })
## ------------------------------------- 
setMethod("show", 
          signature = c(object = "layerSet"),
          function(object){
            show_layers(object)
          })
setMethod("add_layers", 
          signature = c(x = "layerSet"),
          function(x, ...){
            args <- list(...)
            names(args) <- vapply(args, command_name, "ch")
            layers(x) <- c(layers(x), args)
            return(x)
          })
setMethod("delete_layers", 
          signature = c(x = "layerSet", layers = "numeric"),
          function(x, layers){
            layers(x)[layers] <- NULL
            return(x)
          })
setMethod("move_layers", 
          signature = c(x = "layerSet", from = "numeric", to = "numeric"),
          function(x, from, to){
            layers(x)[c(from, to)] <- layers(x)[c(to, from)]
            return(x)
          })
