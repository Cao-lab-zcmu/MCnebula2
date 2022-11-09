# ==========================================================================
# for creating parent layout, these layouts includes:
# layouts for nodes position of ggraph.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases create_parent_layout
#'
#' @title Create layout for visualization of Parent-Nebula
#'
#' @description
#' These methods use functions of [tidygraph::as_tbl_graph()] and
#' [ggraph::create_layout()] to create 'layout_ggraph' (data.frame)
#' object to standby visualization of Parent-Nebula.
#' Run after [create_parent_nebula()].
#'
#' @name create_parent_layout-methods
#' 
#' @seealso [tidygraph::as_tbl_graph()], [ggraph::create_layout()].
#'
#' @order 1
NULL
#> NULL

#' @exportMethod create_parent_layout
#' @description \code{create_parent_layout()}:
#' get the default parameters for the method
#' \code{create_parent_layout}.
#' @rdname create_parent_layout-methods
setMethod("create_parent_layout", 
          signature = setMissing("create_parent_layout"),
          function(){
            list(ggraph_layout = "kk", seed = 1)
          })

#' @exportMethod create_parent_layout
#' @description \code{create_parent_layout(x, ...)}:
#' use the default parameters whatever 'missing'
#' while performing the method \code{create_parent_layout}.
#' @rdname create_parent_layout-methods
setMethod("create_parent_layout", 
          signature = c(x = "mcnebula"),
          function(x, ggraph_layout, seed){
            reCallMethod("create_parent_layout",
                         .fresh_param(create_parent_layout()))
          })

#' @exportMethod create_parent_layout
#'
#' @aliases create_parent_layout
#'
#' @param x [mcnebula-class] object.
#' @param ggraph_layout character(1). Layout name. See [ggraph::create_layout()].
#' @param seed numeric(1). Passed to [set.seed()].
#'
#' @rdname create_parent_layout-methods
#'
#' @examples
#' \dontrun{
#' create_parent_layout(...)
#' }
setMethod("create_parent_layout", 
          signature = c(x = "mcnebula", ggraph_layout = "character",
                        seed = "numeric"),
          function(x, ggraph_layout, seed){
            .message_info_formal("MCnebula2", "create_parent_layout")
            .check_data(parent_nebula(x), list(igraph = "create_parent_nebula"))
            tbl_graph(parent_nebula(x)) <-
              tidygraph::as_tbl_graph(igraph(parent_nebula(x)))
            set.seed(seed)
            layout_ggraph(parent_nebula(x)) <-
              ggraph::create_layout(tbl_graph(parent_nebula(x)),
                                    layout = ggraph_layout)
            return(x)
          })
