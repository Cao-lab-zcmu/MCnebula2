# ==========================================================================
# a class to store network component
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportClass parent_nebula
#'
#' @aliases parent_nebula
#'
#' @description ...
#'
#' @rdname nebula-class
#'
#' @examples
#' \dontrun{
#' new('parent_nebula', ...)
#' }
.parent_nebula <- 
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
#' @exportClass child_nebulae
#'
#' @aliases child_nebulae
#'
#' @description ...
#'
#' @slot igraph ...
#' @slot tbl_graph ...
#' @slot layout_ggraph ...
#' @slot grid_layout ...
#' @slot viewports ...
#' @slot panel_viewport ...
#' @slot legend_viewport ...
#' @slot ggset ...
#' @slot structures_grob ...
#' @slot nodes_ggset ...
#' @slot nodes_grob ...
#' @slot ppcp_data ...
#' @slot ration_data ...
#' @slot ggset_annotate ...
#'
#' @rdname nebula-class
#'
#' @examples
#' \dontrun{
#' new('child_nebulae', ...)
#' }
.child_nebulae <- 
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
#' @exportClass nebula
#'
#' @aliases nebula
#'
#' @title Visualization component of chemical nebulae/nebula
#'
#' @description ...
#'
#' @family nebulae
# @seealso ...
#'
#' @slot parent_nebula ...
#' @slot child_nebulae ...
#'
#' @rdname nebula-class
#' @order 1
#'
#' @examples
#' \dontrun{
#' new('nebula', ...)
#' }
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
#' @exportMethod parent_nebula
#' @aliases parent_nebula
#' @rdname nebula-class
setMethod("show", 
          signature = c(object = "parent_nebula"),
          function(object){
            .show_nebulae_data(object)
          })
#' @exportMethod child_nebulae
#' @aliases child_nebulae
#' @rdname nebula-class
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
#' @exportMethod parent_nebula
#' @aliases parent_nebula
#' @description \code{parent_nebula}, \code{parent_nebula<-}: getter and setter
#' for the \code{parent_nebula} slot of the object.
#' @rdname nebula-class
setMethod("parent_nebula", 
          signature = c(x = "ANY"),
          function(x){ x@parent_nebula })
#' @exportMethod parent_nebula<-
#' @aliases parent_nebula<-
#' @param value The value for the slot.
#' @rdname nebula-class
setReplaceMethod("parent_nebula", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, parent_nebula = value)
                 })
#' @exportMethod child_nebulae
#' @aliases child_nebulae
#' @description \code{child_nebulae}, \code{child_nebulae<-}: getter and setter
#' for the \code{child_nebulae} slot of the object.
#' @rdname nebula-class
setMethod("child_nebulae", 
          signature = c(x = "ANY"),
          function(x){ x@child_nebulae })
#' @exportMethod child_nebulae<-
#' @aliases child_nebulae<-
#' @param value The value for the slot.
#' @rdname nebula-class
setReplaceMethod("child_nebulae", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, child_nebulae = value)
                 })
## ---------------------------------------------------------------------- 
#' @exportMethod igraph
#' @aliases igraph
#' @description \code{igraph}, \code{igraph<-}: getter and setter
#' for the \code{igraph} slot of the object.
#' @rdname nebula-class
setMethod("igraph", 
          signature = c(x = "ANY"),
          function(x){ x@igraph })
#' @exportMethod igraph<-
#' @aliases igraph<-
#' @param value The value for the slot.
#' @rdname nebula-class
setReplaceMethod("igraph", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, igraph = value)
                 })
## ------------------------------------- 
#' @exportMethod tbl_graph
#' @aliases tbl_graph
#' @description \code{tbl_graph}, \code{tbl_graph<-}: getter and setter
#' for the \code{tbl_graph} slot of the object.
#' @rdname nebula-class
setMethod("tbl_graph", 
          signature = c(x = "ANY"),
          function(x){ x@tbl_graph })
#' @exportMethod tbl_graph<-
#' @aliases tbl_graph<-
#' @param value The value for the slot.
#' @rdname nebula-class
setReplaceMethod("tbl_graph", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, tbl_graph = value)
                 })
## ------------------------------------- 
#' @exportMethod layout_ggraph
#' @aliases layout_ggraph
#' @description \code{layout_ggraph}, \code{layout_ggraph<-}: getter and setter
#' for the \code{layout_ggraph} slot of the object.
#' @rdname nebula-class
setMethod("layout_ggraph", 
          signature = c(x = "ANY"),
          function(x){ x@layout_ggraph })
#' @exportMethod layout_ggraph<-
#' @aliases layout_ggraph<-
#' @param value The value for the slot.
#' @rdname nebula-class
setReplaceMethod("layout_ggraph", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, layout_ggraph = value)
                 })
## ------------------------------------- 
#' @exportMethod grid_layout
#' @aliases grid_layout
#' @description \code{grid_layout}, \code{grid_layout<-}: getter and setter
#' for the \code{grid_layout} slot of the object.
#' @rdname nebula-class
setMethod("grid_layout", 
          signature = c(x = "ANY"),
          function(x){ x@grid_layout })
#' @exportMethod grid_layout<-
#' @aliases grid_layout<-
#' @param value The value for the slot.
#' @rdname nebula-class
setReplaceMethod("grid_layout", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, grid_layout = value)
                 })
## ------------------------------------- 
#' @exportMethod viewports
#' @aliases viewports
#' @description \code{viewports}, \code{viewports<-}: getter and setter
#' for the \code{viewports} slot of the object.
#' @rdname nebula-class
setMethod("viewports", 
          signature = c(x = "ANY"),
          function(x){ x@viewports })
#' @exportMethod viewports<-
#' @aliases viewports<-
#' @param value The value for the slot.
#' @rdname nebula-class
setReplaceMethod("viewports", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, viewports = value)
                 })
## ------------------------------------- 
#' @exportMethod ggset
#' @aliases ggset
#' @description \code{ggset}, \code{ggset<-}: getter and setter
#' for the \code{ggset} slot of the object.
#' @rdname nebula-class
setMethod("ggset", 
          signature = c(x = "ANY"),
          function(x){ x@ggset })
#' @exportMethod ggset<-
#' @aliases ggset<-
#' @param value The value for the slot.
#' @rdname nebula-class
setReplaceMethod("ggset", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, ggset = value)
                 })
## ------------------------------------- 
#' @exportMethod panel_viewport
#' @aliases panel_viewport
#' @description \code{panel_viewport}, \code{panel_viewport<-}: getter and setter
#' for the \code{panel_viewport} slot of the object.
#' @rdname nebula-class
setMethod("panel_viewport", 
          signature = c(x = "ANY"),
          function(x){ x@panel_viewport })
#' @exportMethod panel_viewport<-
#' @aliases panel_viewport<-
#' @param value The value for the slot.
#' @rdname nebula-class
setReplaceMethod("panel_viewport", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, panel_viewport = value)
                 })
## ------------------------------------- 
#' @exportMethod legend_viewport
#' @aliases legend_viewport
#' @description \code{legend_viewport}, \code{legend_viewport<-}: getter and setter
#' for the \code{legend_viewport} slot of the object.
#' @rdname nebula-class
setMethod("legend_viewport", 
          signature = c(x = "ANY"),
          function(x){ x@legend_viewport })
#' @exportMethod legend_viewport<-
#' @aliases legend_viewport<-
#' @param value The value for the slot.
#' @rdname nebula-class
setReplaceMethod("legend_viewport", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, legend_viewport = value)
                 })
## ------------------------------------- 
#' @exportMethod structures_grob
#' @aliases structures_grob
#' @description \code{structures_grob}, \code{structures_grob<-}: getter and setter
#' for the \code{structures_grob} slot of the object.
#' @rdname nebula-class
setMethod("structures_grob", 
          signature = c(x = "ANY"),
          function(x){ x@structures_grob })
#' @exportMethod structures_grob<-
#' @aliases structures_grob<-
#' @param value The value for the slot.
#' @rdname nebula-class
setReplaceMethod("structures_grob", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, structures_grob = value)
                 })
## ------------------------------------- 
#' @exportMethod nodes_ggset
#' @aliases nodes_ggset
#' @description \code{nodes_ggset}, \code{nodes_ggset<-}: getter and setter
#' for the \code{nodes_ggset} slot of the object.
#' @rdname nebula-class
setMethod("nodes_ggset", 
          signature = c(x = "ANY"),
          function(x){ x@nodes_ggset })
#' @exportMethod nodes_ggset<-
#' @aliases nodes_ggset<-
#' @param value The value for the slot.
#' @rdname nebula-class
setReplaceMethod("nodes_ggset", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, nodes_ggset = value)
                 })
## ------------------------------------- 
#' @exportMethod nodes_grob
#' @aliases nodes_grob
#' @description \code{nodes_grob}, \code{nodes_grob<-}: getter and setter
#' for the \code{nodes_grob} slot of the object.
#' @rdname nebula-class
setMethod("nodes_grob", 
          signature = c(x = "ANY"),
          function(x){ x@nodes_grob })
#' @exportMethod nodes_grob<-
#' @aliases nodes_grob<-
#' @param value The value for the slot.
#' @rdname nebula-class
setReplaceMethod("nodes_grob", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, nodes_grob = value)
                 })
## ------------------------------------- 
#' @exportMethod ppcp_data
#' @aliases ppcp_data
#' @description \code{ppcp_data}, \code{ppcp_data<-}: getter and setter
#' for the \code{ppcp_data} slot of the object.
#' @rdname nebula-class
setMethod("ppcp_data", 
          signature = c(x = "ANY"),
          function(x){ x@ppcp_data })
#' @exportMethod ppcp_data<-
#' @aliases ppcp_data<-
#' @param value The value for the slot.
#' @rdname nebula-class
setReplaceMethod("ppcp_data", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, ppcp_data = value)
                 })
## ------------------------------------- 
#' @exportMethod ration_data
#' @aliases ration_data
#' @description \code{ration_data}, \code{ration_data<-}: getter and setter
#' for the \code{ration_data} slot of the object.
#' @rdname nebula-class
setMethod("ration_data", 
          signature = c(x = "ANY"),
          function(x){ x@ration_data })
#' @exportMethod ration_data<-
#' @aliases ration_data<-
#' @param value The value for the slot.
#' @rdname nebula-class
setReplaceMethod("ration_data", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, ration_data = value)
                 })
## ------------------------------------- 
#' @exportMethod ggset_annotate
#' @aliases ggset_annotate
#' @description \code{ggset_annotate}, \code{ggset_annotate<-}: getter and setter
#' for the \code{ggset_annotate} slot of the object.
#' @rdname nebula-class
setMethod("ggset_annotate", 
          signature = c(x = "ANY"),
          function(x){ x@ggset_annotate })
#' @exportMethod ggset_annotate<-
#' @aliases ggset_annotate<-
#' @param value The value for the slot.
#' @rdname nebula-class
setReplaceMethod("ggset_annotate", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, ggset_annotate = value)
                 })
