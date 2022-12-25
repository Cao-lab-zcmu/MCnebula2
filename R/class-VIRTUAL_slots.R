# ==========================================================================
# VIRTUAL classes (sharing slots and methods)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases VIRTUAL_dataset dataset
#'
#' @title Share slots and methods for classes inherite from VIRTUAL_dataset
#'
#' @description This VIRTUAL class provides a slot for storing data and methods
#' for accessing data in slot.
#'
#' @family datasets
#'
#' @slot dataset list with names (subscript, imply file names).
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
#' @description This VIRTUAL class provides a slot for storing processed data.
#'
#' @family references
#'
#' @slot reference list with names (formal name).
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
#' @description This VIRTUAL class provides a slot for storing discarded data.
#'
#' @family backtracks
#'
#' @slot backtrack list with names.
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
#' @description This VIRTUAL class provides a slot for signing the data.
#' The "subscript" like the signature for data, used to distinguish different data
#' or file and retrieve it accurately.
#' The "subscript" is mostly used for [project-class] (as well as its related classes):
#' - imply file names. e.g., for "sirius.v4", ".f3_fingerid" indicate all files in
#' directory of "fingerid" for each features.
#' - imply attribute names. e.g., for "sirius.v4", "tani.score" indicate attribute name
#' of "tanimotoSimilarity".
#'
#' In essence, "subscript" is the alias of a file or data or attribute.
#' In this package, using the "subscript" system means that
#' all external data names are given an alias.
#' In fact, this makes things more complicated. Why did we do this?
#' Because the naming system of external data is not constant,
#' these names may change with the version of the data source.
#' In order to enable this R package to accurately extract and call these data,
#' it is necessary to establish a set of aliases within the package.
#' "Subscript" names are used internally by this package.
#' They correspond to external data and are equivalent to providing an interface
#' to interface with external data.
#'
#' @family subscripts
#'
#' @slot subscript character(1).
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
#' @description This VIRTUAL class provides slots for recording export path
#' and export name of attributes.
#'
#' @family exports
#'
#' @slot export_path character(1). The export directory path.
#' @slot export_name character with names.
#' While export, the attribute name will be converted to the value.
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
#' @description This VIRTUAL class provides: slot \code{layers} for storing
#' hierarchical data; and methods for modify slot \code{layers}.
#'
#' @family layerSets
#'
#' @slot layers list with names.
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
            dataset(x) <- vecter_unique_by_names(dataset)
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
#' @param x object inherit class \code{reference}.
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
#' @param x object inherit class \code{backtrack}.
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
#' @param x object inherit class \code{subscript}.
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
#' @param x object inherit class \code{export}.
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
          function(x){
            path <- x@export_path
            if (!file.exists(path))
              dir.create(path)
            path
          })

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
#' @param x object inherit class \code{layerSet}.
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
#' @description \code{add_layers}: add extra "layer" into slot \code{layers}.
#' @param x object contains slot \code{layers}.
#' @param ... extra "layer".
#' @rdname VIRTUAL_layerSet-class
setMethod("add_layers", 
          signature = c(x = "layerSet"),
          function(x, ...){
            args <- list(...)
            layers(x) <- c(layers(x), args)
            return(x)
          })

#' @exportMethod delete_layers
#' @aliases delete_layers
#' @description \code{delete_layers}: delete "layer" in slot \code{layers}.
#' @param layers numeric. The specified "layer" in slot \code{layers}.
#' @rdname VIRTUAL_layerSet-class
setMethod("delete_layers", 
          signature = c(x = "layerSet", layers = "numeric"),
          function(x, layers){
            layers(x)[layers] <- NULL
            return(x)
          })

#' @exportMethod move_layers
#' @aliases move_layers
#' @description \code{move_layers}: change the order of "layer" in slot \code{layers}.
#' @param from sequence (sequence in list) of "layer" move from.
#' @param to sequence (sequence in list) of "layer" move to.
#' @rdname VIRTUAL_layerSet-class
setMethod("move_layers", 
          signature = c(x = "layerSet", from = "numeric", to = "numeric"),
          function(x, from, to){
            layers(x)[c(from, to)] <- layers(x)[c(to, from)]
            return(x)
          })

#' @exportMethod insert_layers
#' @aliases insert_layers
#' @description \code{insert_layers}: Insert "layers" into the specified
#' position (sequence) of slot \code{layers}.
#' @rdname VIRTUAL_layerSet-class
setMethod("insert_layers", 
          signature = c(x = "layerSet", to = "numeric"),
          function(x, to, ...){
            before <- length(layers(x))
            x <- add_layers(x, ...)
            now <- length(layers(x))
            x <- move_layers(x, to:before, (before + 1):now)
            return(x)
          })
