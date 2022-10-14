# ==========================================================================
# a class to store network component
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setClass("parent_nebula", 
         contains = character(),
         representation = 
           representation(igraph = "ANY",
                          tbl_graph = "ANY",
                          layout_ggraph = "ANY",
                          ggset = "ggset"
                          ),
         prototype = NULL
         )
setClass("child_nebulae", 
         contains = character(),
         representation = 
           representation(igraph = "list",
                          tbl_graph = "list",
                          layout_ggraph = "list",
                          grid_layout = "ANY",
                          viewports = "list",
                          panel_viewport = "ANY",
                          legend_viewport = "ANY",
                          ggset = "list",
                          structures_grob = "list",
                          nodes_ggset = "list",
                          nodes_grob = "list",
                          ppcp_data = "list",
                          ration_data = "list",
                          ggset_annotate = "list"
                          ),
         prototype = NULL
         )
.nebula <- 
  setClass("nebula", 
           contains = character(),
           representation = 
             representation(parent_nebula = "parent_nebula",
                            child_nebulae = "child_nebulae"
                            ),
           prototype = NULL
           )
# ==========================================================================
# method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("show", 
          signature = c(object = "parent_nebula"),
          function(object){
            .show_nebulae_data(object)
                            
          })
setMethod("show", 
          signature = c(object = "child_nebulae"),
          function(object){
            .show_nebulae_data(object)
          })
.show_nebulae_data <- 
  function(object){
    slots_mapply(object, function(slot, name){
                   if (is(slot, "viewport")) {
                     num <- 1
                   } else if (is.list(slot)) {
                     num <- length(slot)
                   } else {
                     if (is.null(slot))
                       num <- 0
                     else
                       num <- 1
                   }
                   if (num == 0 | is(slot, "name")) 
                     return()
                   cat(name, ": ", class(slot)[1], " of ", num,
                       "\n", sep = "")
                      })
  }
## ------------------------------------- 
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
## ---------------------------------------------------------------------- 
setMethod("igraph", 
          signature = c(x = "ANY"),
          function(x){ x@igraph })
setReplaceMethod("igraph", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, igraph = value)
                 })
## ------------------------------------- 
setMethod("tbl_graph", 
          signature = c(x = "ANY"),
          function(x){ x@tbl_graph })
setReplaceMethod("tbl_graph", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, tbl_graph = value)
                 })
## ------------------------------------- 
setMethod("layout_ggraph", 
          signature = c(x = "ANY"),
          function(x){ x@layout_ggraph })
setReplaceMethod("layout_ggraph", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, layout_ggraph = value)
                 })
## ------------------------------------- 
setMethod("grid_layout", 
          signature = c(x = "ANY"),
          function(x){ x@grid_layout })
setReplaceMethod("grid_layout", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, grid_layout = value)
                 })
## ------------------------------------- 
setMethod("viewports", 
          signature = c(x = "ANY"),
          function(x){ x@viewports })
setReplaceMethod("viewports", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, viewports = value)
                 })
## ------------------------------------- 
setMethod("ggset", 
          signature = c(x = "ANY"),
          function(x){ x@ggset })
setReplaceMethod("ggset", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, ggset = value)
                 })
## ------------------------------------- 
setMethod("panel_viewport", 
          signature = c(x = "ANY"),
          function(x){ x@panel_viewport })
setReplaceMethod("panel_viewport", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, panel_viewport = value)
                 })
## ------------------------------------- 
setMethod("legend_viewport", 
          signature = c(x = "ANY"),
          function(x){ x@legend_viewport })
setReplaceMethod("legend_viewport", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, legend_viewport = value)
                 })
## ------------------------------------- 
setMethod("structures_grob", 
          signature = c(x = "ANY"),
          function(x){ x@structures_grob })
setReplaceMethod("structures_grob", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, structures_grob = value)
                 })
## ------------------------------------- 
setMethod("nodes_ggset", 
          signature = c(x = "ANY"),
          function(x){ x@nodes_ggset })
setReplaceMethod("nodes_ggset", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, nodes_ggset = value)
                 })
## ------------------------------------- 
setMethod("nodes_grob", 
          signature = c(x = "ANY"),
          function(x){ x@nodes_grob })
setReplaceMethod("nodes_grob", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, nodes_grob = value)
                 })
## ------------------------------------- 
setMethod("ppcp_data", 
          signature = c(x = "ANY"),
          function(x){ x@ppcp_data })
setReplaceMethod("ppcp_data", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, ppcp_data = value)
                 })
## ------------------------------------- 
setMethod("ration_data", 
          signature = c(x = "ANY"),
          function(x){ x@ration_data })
setReplaceMethod("ration_data", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, ration_data = value)
                 })
## ------------------------------------- 
setMethod("ggset_annotate", 
          signature = c(x = "ANY"),
          function(x){ x@ggset_annotate })
setReplaceMethod("ggset_annotate", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, ggset_annotate = value)
                 })
