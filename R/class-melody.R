# ==========================================================================
# a class to store hex color set.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
setMethod("show", 
          signature = c(object= "melody"),
          function(object){
            .show(object)
          })
## ------------------------------------- 
setMethod("melody", 
          signature = c(x = "ANY"),
          function(x){ x@melody })
setReplaceMethod("melody", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, melody = value)
                 })
## ------------------------------------- 
setMethod("palette_set", 
          signature = c(x = "melody"),
          function(x){ x@palette_set })
setReplaceMethod("palette_set", 
                 signature = c(x = "melody"),
                 function(x, value){
                   initialize(x, palette_set = value)
                 })
## ------------------------------------- 
setMethod("palette_gradient", 
          signature = c(x = "melody"),
          function(x){ x@palette_gradient })
setReplaceMethod("palette_gradient", 
                 signature = c(x = "melody"),
                 function(x, value){
                   initialize(x, palette_gradient = value)
                 })
## ------------------------------------- 
setMethod("palette_stat", 
          signature = c(x = "melody"),
          function(x){ x@palette_stat })
setReplaceMethod("palette_stat", 
                 signature = c(x = "melody"),
                 function(x, value){
                   initialize(x, palette_stat = value)
                 })
## ------------------------------------- 
setMethod("palette_col", 
          signature = c(x = "melody"),
          function(x){ x@palette_col })
setReplaceMethod("palette_col", 
                 signature = c(x = "melody"),
                 function(x, value){
                   initialize(x, palette_col = value)
                 })
## ------------------------------------- 
setMethod("palette_label", 
          signature = c(x = "melody"),
          function(x){ x@palette_label })
setReplaceMethod("palette_label", 
                 signature = c(x = "melody"),
                 function(x, value){
                   initialize(x, palette_label = value)
                 })
