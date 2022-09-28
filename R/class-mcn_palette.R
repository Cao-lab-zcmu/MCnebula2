# ==========================================================================
# a class to store hex color set.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.mcn_palette <- 
  setClass("mcn_palette", 
           contains = character(),
           representation = 
             representation(palette_set = "character",
                            palette_gradient = "character",
                            palette_stat = "character",
                            palette_ppcp = "character",
                            palette_label = "character"
                            ),
           prototype = NULL
  )
# ==========================================================================
# method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("show", 
          signature = c(object= "mcn_palette"),
          function(object){
            .show(object)
          })
## ------------------------------------- 
setMethod("mcn_palette", 
          signature = c(x = "ANY"),
          function(x){ x@mcn_palette })
setReplaceMethod("mcn_palette", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, mcn_palette = value)
                 })
## ------------------------------------- 
setMethod("palette_set", 
          signature = c(x = "mcn_palette"),
          function(x){ x@palette_set })
setReplaceMethod("palette_set", 
                 signature = c(x = "mcn_palette"),
                 function(x, value){
                   initialize(x, palette_set = value)
                 })
## ------------------------------------- 
setMethod("palette_gradient", 
          signature = c(x = "mcn_palette"),
          function(x){ x@palette_gradient })
setReplaceMethod("palette_gradient", 
                 signature = c(x = "mcn_palette"),
                 function(x, value){
                   initialize(x, palette_gradient = value)
                 })
## ------------------------------------- 
setMethod("palette_stat", 
          signature = c(x = "mcn_palette"),
          function(x){ x@palette_stat })
setReplaceMethod("palette_stat", 
                 signature = c(x = "mcn_palette"),
                 function(x, value){
                   initialize(x, palette_stat = value)
                 })
## ------------------------------------- 
setMethod("palette_ppcp", 
          signature = c(x = "mcn_palette"),
          function(x){ x@palette_ppcp })
setReplaceMethod("palette_ppcp", 
                 signature = c(x = "mcn_palette"),
                 function(x, value){
                   initialize(x, palette_ppcp = value)
                 })
## ------------------------------------- 
setMethod("palette_label", 
          signature = c(x = "mcn_palette"),
          function(x){ x@palette_label })
setReplaceMethod("palette_label", 
                 signature = c(x = "mcn_palette"),
                 function(x, value){
                   initialize(x, palette_label = value)
                 })
