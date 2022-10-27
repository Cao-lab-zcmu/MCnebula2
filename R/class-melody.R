# ==========================================================================
# a class to store hex color set.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportClass melody
#'
#' @aliases melody
#'
#' @title ...
#'
#' @description ...
#'
# @family melodys
#' @seealso [ggsci::pal_simpsons()], [ggsci::pal_igv()], [ggsci::pal_ucscgb()],
#' [ggsci::pal_d3()]...
#'
#' @slot palette_set ...
#' @slot palette_gradient ...
#' @slot palette_stat ...
#' @slot palette_col ...
#' @slot palette_label ...
#'
#' @rdname melody-class
#'
#' @examples
#' \dontrun{
#' new('melody', ...)
#' }
.melody <- 
  setClass("melody", 
           contains = character(),
           representation = 
             representation(palette_set = "character",
                            palette_gradient = "character",
                            palette_stat = "character",
                            palette_col = "character",
                            palette_label = "character"
                            ),
           prototype = NULL
  )
# ==========================================================================
# method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportMethod show
#' @aliases show
#' @rdname melody-class
setMethod("show", 
          signature = c(object= "melody"),
          function(object){
            .show(object)
          })
## ------------------------------------- 
#' @exportMethod melody
#' @aliases melody
#' @description \code{melody}, \code{melody<-}: getter and setter
#' for the \code{melody} slot of the object.
#' @rdname melody-class
setMethod("melody", 
          signature = c(x = "ANY"),
          function(x){ x@melody })
#' @exportMethod melody<-
#' @aliases melody<-
#' @param value The value for the slot.
#' @rdname melody-class
setReplaceMethod("melody", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, melody = value)
                 })
## ------------------------------------- 
#' @exportMethod palette_set
#' @aliases palette_set
#' @description \code{palette_set}, \code{palette_set<-}: getter and setter
#' for the \code{palette_set} slot of the object.
#' @rdname melody-class
setMethod("palette_set", 
          signature = c(x = "melody"),
          function(x){ x@palette_set })
#' @exportMethod palette_set<-
#' @aliases palette_set<-
#' @param value The value for the slot.
#' @rdname melody-class
setReplaceMethod("palette_set", 
                 signature = c(x = "melody"),
                 function(x, value){
                   initialize(x, palette_set = value)
                 })
## ------------------------------------- 
#' @exportMethod palette_gradient
#' @aliases palette_gradient
#' @description \code{palette_gradient}, \code{palette_gradient<-}: getter and setter
#' for the \code{palette_gradient} slot of the object.
#' @rdname melody-class
setMethod("palette_gradient", 
          signature = c(x = "melody"),
          function(x){ x@palette_gradient })
#' @exportMethod palette_gradient<-
#' @aliases palette_gradient<-
#' @param value The value for the slot.
#' @rdname melody-class
setReplaceMethod("palette_gradient", 
                 signature = c(x = "melody"),
                 function(x, value){
                   initialize(x, palette_gradient = value)
                 })
## ------------------------------------- 
#' @exportMethod palette_stat
#' @aliases palette_stat
#' @description \code{palette_stat}, \code{palette_stat<-}: getter and setter
#' for the \code{palette_stat} slot of the object.
#' @rdname melody-class
setMethod("palette_stat", 
          signature = c(x = "melody"),
          function(x){ x@palette_stat })
#' @exportMethod palette_stat<-
#' @aliases palette_stat<-
#' @param value The value for the slot.
#' @rdname melody-class
setReplaceMethod("palette_stat", 
                 signature = c(x = "melody"),
                 function(x, value){
                   initialize(x, palette_stat = value)
                 })
## ------------------------------------- 
#' @exportMethod palette_col
#' @aliases palette_col
#' @description \code{palette_col}, \code{palette_col<-}: getter and setter
#' for the \code{palette_col} slot of the object.
#' @rdname melody-class
setMethod("palette_col", 
          signature = c(x = "melody"),
          function(x){ x@palette_col })
#' @exportMethod palette_col<-
#' @aliases palette_col<-
#' @param value The value for the slot.
#' @rdname melody-class
setReplaceMethod("palette_col", 
                 signature = c(x = "melody"),
                 function(x, value){
                   initialize(x, palette_col = value)
                 })
## ------------------------------------- 
#' @exportMethod palette_label
#' @aliases palette_label
#' @description \code{palette_label}, \code{palette_label<-}: getter and setter
#' for the \code{palette_label} slot of the object.
#' @rdname melody-class
setMethod("palette_label", 
          signature = c(x = "melody"),
          function(x){ x@palette_label })
#' @exportMethod palette_label<-
#' @aliases palette_label<-
#' @param value The value for the slot.
#' @rdname melody-class
setReplaceMethod("palette_label", 
                 signature = c(x = "melody"),
                 function(x, value){
                   initialize(x, palette_label = value)
                 })
