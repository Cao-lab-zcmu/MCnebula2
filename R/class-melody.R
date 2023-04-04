# ==========================================================================
# a class to store hex color set.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportClass melody
#'
#' @aliases melody
#'
#' @title Mutiple color palette in hexadecimal code
#'
#' @description
#' This is a class object store Hex color used for visualization.
#' In default (use [initialize_mcnebula()] to initialize the object),
#' these these Hex color in each palette were get from package \code{ggsci}.
#' Most of these palette in this package would passed to [ggplot2::scale_fill_manual] for
#' filling color. So, let these Hex color with names may work well to specify target.
#'
#' @seealso [ggsci::pal_simpsons()], [ggsci::pal_igv()], [ggsci::pal_ucscgb()],
#' [ggsci::pal_d3()]...
#'
#' @slot palette_set character with names or not. Hex color.
#' @slot palette_gradient character with names or not. Hex color.
#' @slot palette_stat character with names or not. Hex color.
#' @slot palette_col character with names or not. Hex color.
#' @slot palette_label character with names or not. Hex color.
#'
#' @rdname melody-class
#'
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
