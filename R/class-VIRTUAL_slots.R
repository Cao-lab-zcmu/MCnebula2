# ==========================================================================
# VIRTUAL classes (sharing slots and methods)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases VIRTUAL_dataset dataset
#'
#' @title Share slots and methods for classes inherite from VIRTUAL_dataset
#'
#' @description ...
#'
#' @family datasets
#'
#' @slot dataset ...
#'
#' @rdname VIRTUAL_dataset-class
setClass("dataset", 
         contains = character(),
         representation = 
           representation("VIRTUAL",
                          dataset = "list"
                          ),
         prototype = NULL
         )
#' @aliases VIRTUAL_reference reference
#'
#' @title Share slots and methods for classes inherite from VIRTUAL_reference
#'
#' @description ...
#'
#' @family references
#'
#' @slot reference ...
#'
#' @rdname VIRTUAL_reference-class
setClass("reference", 
         contains = character(),
         representation = 
           representation("VIRTUAL",
                          reference = "list"
                          ),
         prototype = NULL
         )
#' @aliases VIRTUAL_backtrack backtrack
#'
#' @title Share slots and methods for classes inherite from VIRTUAL_backtrack
#'
#' @description ...
#'
#' @family backtracks
#'
#' @slot backtrack ...
#'
#' @rdname VIRTUAL_backtrack-class
setClass("backtrack", 
         contains = character(),
         representation = 
           representation("VIRTUAL",
                          backtrack = "list"
                          ),
         prototype = NULL
         )
#' @aliases VIRTUAL_subscript subscript
#'
#' @title Share slots and methods for classes inherite from VIRTUAL_subscript
#'
#' @description ...
#'
#' @family subscripts
#'
#' @slot subscript ...
#'
#' @rdname VIRTUAL_subscript-class
setClass("subscript", 
         contains = character(),
         representation = 
           representation("VIRTUAL",
                          subscript = "character"
                          ),
         prototype = NULL
         )
#' @aliases VIRTUAL_export export
#'
#' @title Share slots and methods for classes inherite from VIRTUAL_export
#'
#' @description ...
#'
#' @family exports
#'
#' @slot export_path ...
#' @slot export_name ...
#'
#' @rdname VIRTUAL_export-class
setClass("export", 
         contains = character(),
         representation = 
           representation("VIRTUAL",
                          export_path = "character",
                          export_name = "character"
                          ),
         prototype = NULL
         )
#' @aliases VIRTUAL_layerSet layerSet
#'
#' @title Share slots and methods for classes inherite from VIRTUAL_layerSet
#'
#' @description ...
#'
#' @family layerSets
#'
#' @slot layers ...
#'
#' @rdname VIRTUAL_layerSet-class
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
#' @exportMethod dataset
#' @aliases dataset
#' @description \code{dataset}, \code{dataset<-}: getter and setter
#' for the \code{dataset} slot of the object.
#' @rdname VIRTUAL_dataset-class
setMethod("dataset", "ANY",
          function(x){ x@dataset })
#' @exportMethod dataset<-
#' @aliases dataset<-
#' @param value The value for the slot.
#' @rdname VIRTUAL_dataset-class
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
#' @exportMethod reference
#' @aliases reference
#' @description \code{reference}, \code{reference<-}: getter and setter
#' for the \code{reference} slot of the object.
#' @rdname VIRTUAL_reference-class
setMethod("reference", "ANY",
          function(x){ x@reference })
#' @exportMethod reference<-
#' @aliases reference<-
#' @param value The value for the slot.
#' @rdname VIRTUAL_reference-class
setReplaceMethod("reference", "ANY",
                 function(x, value){
                   initialize(x, reference = value)
                 })
## ------------------------------------- 
#' @exportMethod backtrack
#' @aliases backtrack
#' @description \code{backtrack}, \code{backtrack<-}: getter and setter
#' for the \code{backtrack} slot of the object.
#' @rdname VIRTUAL_backtrack-class
setMethod("backtrack", "ANY",
          function(x){ x@backtrack })
#' @exportMethod backtrack<-
#' @aliases backtrack<-
#' @param value The value for the slot.
#' @rdname VIRTUAL_backtrack-class
setReplaceMethod("backtrack", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, backtrack = value)
                 })
## ------------------------------------- 
#' @exportMethod subscript
#' @aliases subscript
#' @description \code{subscript}, \code{subscript<-}: getter and setter
#' for the \code{subscript} slot of the object.
#' @rdname VIRTUAL_subscript-class
setMethod("subscript", "ANY",
          function(x){ x@subscript })
#' @exportMethod subscript<-
#' @aliases subscript<-
#' @param value The value for the slot.
#' @rdname VIRTUAL_subscript-class
setReplaceMethod("subscript", "ANY",
                 function(x, value){
                   initialize(x, subscript = value)
                 })
## ------------------------------------- 
#' @exportMethod export_name
#' @aliases export_name
#' @description \code{export_name}, \code{export_name<-}: getter and setter
#' for the \code{export_name} slot of the object.
#' @rdname VIRTUAL_export-class
setMethod("export_name", 
          signature = c(x = "ANY"),
          function(x){ x@export_name })
#' @exportMethod export_name<-
#' @aliases export_name<-
#' @param value The value for the slot.
#' @rdname VIRTUAL_export-class
setReplaceMethod("export_name", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, export_name = value)
                 })
#' @exportMethod export_path
#' @aliases export_path
#' @description \code{export_path}, \code{export_path<-}: getter and setter
#' for the \code{export_path} slot of the object.
#' @rdname VIRTUAL_export-class
setMethod("export_path", 
          signature = c(x = "ANY"),
          function(x){ x@export_path })
#' @exportMethod export_path<-
#' @aliases export_path<-
#' @param value The value for the slot.
#' @rdname VIRTUAL_export-class
setReplaceMethod("export_path", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, export_path = value)
                 })
## ---------------------------------------------------------------------- 
#' @exportMethod layers
#' @aliases layers
#' @description \code{layers}, \code{layers<-}: getter and setter
#' for the \code{layers} slot of the object.
#' @rdname VIRTUAL_layerSet-class
setMethod("layers", 
          signature = c(x = "layerSet"),
          function(x){ x@layers })
#' @exportMethod layers<-
#' @aliases layers<-
#' @param value The value for the slot.
#' @rdname VIRTUAL_layerSet-class
setReplaceMethod("layers", 
                 signature = c(x = "layerSet"),
                 function(x, value){
                   initialize(x, layers = value)
                 })
## ------------------------------------- 
#' @exportMethod show
#' @aliases show
#' @rdname VIRTUAL_layerSet-class
setMethod("show", 
          signature = c(object = "layerSet"),
          function(object){
            show_layers(object)
          })
#' @exportMethod add_layers
#' @aliases add_layers
#' @description \code{add_layers}: ...
#' @param x ...
#' @param ... ...
# @family ...s
# @seealso [fun()]
#' @rdname VIRTUAL_layerSet-class
#' @examples
#' \dontrun{
#' add_layers(...)
#' }
setMethod("add_layers", 
          signature = c(x = "layerSet"),
          function(x, ...){
            args <- list(...)
            names(args) <- vapply(args, command_name, "ch")
            layers(x) <- c(layers(x), args)
            return(x)
          })
#' @exportMethod delete_layers
#' @aliases delete_layers
#' @description \code{delete_layers}: ...
#' @param x ...
#' @param layers ...
# @family ...s
# @seealso [fun()]
#' @rdname VIRTUAL_layerSet-class
#' @examples
#' \dontrun{
#' delete_layers(...)
#' }
setMethod("delete_layers", 
          signature = c(x = "layerSet", layers = "numeric"),
          function(x, layers){
            layers(x)[layers] <- NULL
            return(x)
          })
#' @exportMethod move_layers
#' @aliases move_layers
#' @description \code{move_layers}: ...
#' @param x ...
#' @param from ...
#' @param to ...
# @family ...s
# @seealso [fun()]
#' @rdname VIRTUAL_layerSet-class
#' @examples
#' \dontrun{
#' move_layers(...)
#' }
setMethod("move_layers", 
          signature = c(x = "layerSet", from = "numeric", to = "numeric"),
          function(x, from, to){
            layers(x)[c(from, to)] <- layers(x)[c(to, from)]
            return(x)
          })
