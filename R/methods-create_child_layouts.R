# # ==========================================================================
# for creating child layout, these layouts includes:
# layouts for nodes position of ggraph (layout_ggraph slot);
# layouts of size and position of grid panel (grid_layout slot);
# layouts of viewports of each child_nebula (in which grid panel) (viewports slot).
# layout of viewport of panel of overall child_nebulae (panel_viewport slot);
# layout of viewport of legend of child_nebulae (legend_viewport slot).
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @importFrom grid grid.layout
#' @importFrom grid viewport
#' @importFrom grid pushViewport
#' @importFrom dplyr desc
#' @importFrom dplyr arrange
#' @importFrom dplyr select
#' @exportMethod create_child_layouts
#' @description \code{create_child_layouts()}: get the function for generating
#' default parameters for the method
#' \code{create_child_layouts}.
#' @rdname create_child_layouts-methods
setMethod("create_child_layouts", 
          signature = setMissing("create_child_layouts",
                                 x = "missing"),
          function(){
            function(x){
              .check_data(child_nebulae(x), list(igraph = "create_child_nebulae"))
              len <- length(igraph(child_nebulae(x)))
              ggraph_layouts <-
                vapply(igraph(child_nebulae(x)), .propose_graph_layout, "ch")
              seeds <- rep(1, len)
              ncol <- round(sqrt(len))
              if (ncol ^ 2 < len) {
                nrow <- ncol + 1
              } else {
                nrow <- ncol
              }
              grid_layout <- grid::grid.layout(nrow, ncol)
              num_grid <- grid_layout$nrow * grid_layout$ncol
              if (num_grid < len) {
                stop( "`grid_layout` must contains enough sub-panels for child_nebulae." )
              }
              names <- as.character(sort(factor(names(igraph(child_nebulae(x))),
                                                levels = hierarchy(x)$class.name)))
              mtrx <- matrix(c(names, rep("...FILL", num_grid - len)),
                             ncol = grid_layout$ncol, byrow = T)
              viewports <-
                unlist(recursive = F,
                       mapply(apply(mtrx, 1, c, simplify = F), 1:nrow(mtrx),
                              SIMPLIFY = F,
                              FUN = function(names, row) {
                                lapply(1:length(names),
                                       function(n) {
                                         grid::viewport(layout = grid_layout,
                                                        layout.pos.row = row,
                                                        layout.pos.col = n)
                                       })
                              }))[1:len]
              names(viewports) <- names
              panel_viewport <-
                grid::viewport(0, 0.5, 0.8, 1, just = c("left", "centre"))
              legend_viewport <- 
                grid::viewport(0.8, 0.5, 0.2, 1, just = c("left", "centre"))
              list(ggraph_layouts = ggraph_layouts,
                   seeds = seeds,
                   grid_layout = grid_layout,
                   viewports = viewports,
                   panel_viewport = panel_viewport,
                   legend_viewport = legend_viewport
              )
            }
          })
#' @exportMethod create_child_layouts
#'
#' @aliases create_child_layouts
#'
#' @title ...
#'
#' @description ...
#' @description \code{create_child_layouts(x, ...)}: use the default parameters whatever 'missing'
#' while performing the method \code{create_child_layouts}.
#'
#' @details ...
#'
#' @param x ...
#' @param ggraph_layouts ...
#' @param seeds ...
#' @param grid_layout ...
#' @param viewports ...
#' @param panel_viewport ...
#' @param legend_viewport ...
#'
# @inheritParams rdname
#'
#' @return ...
#'
# @seealso ...
#'
#' @rdname create_child_layouts-methods
#'
#' @order 1
#'
#' @examples
#' \dontrun{
#' create_child_layouts(...)
#' }
setMethod("create_child_layouts", 
          signature = c(x = "mcnebula"),
          function(x, ggraph_layouts, seeds,
                   grid_layout, viewports,
                   panel_viewport, legend_viewport){
            .message_info_formal("MCnebula2", "create_child_layouts")
            do.call(.create_child_layouts,
                    .fresh_param(create_child_layouts()(x)))
          })
.create_child_layouts <- 
  function(x, ggraph_layouts, seeds,
           grid_layout, viewports,
           panel_viewport, legend_viewport
           ){
    set <- igraph(child_nebulae(x))
    ggraph_layouts <- .as_dic(ggraph_layouts, names(set), fill = F)
    seeds <- .as_dic(seeds, names(set))
    if (!is.null(grid_layout))
      .check_class(grid_layout)
    .check_names(viewports, set, "viewports", "igraph(child_nebulae(x))")
    if (length(viewports) != length(set)) {
      stop(paste0("`viewports` must be a list ",
                  "the same length as 'igraph(child_nebulae(x))'."))
    }
    viewports <- .as_dic(viewports, names(set), fill = F)
    .check_class(panel_viewport, "viewport", "grid::viewport")
    .check_class(legend_viewport, "viewport", "grid::viewport")
    tbl_graph(child_nebulae(x)) <- lapply(set, tidygraph::as_tbl_graph)
    layout_ggraph(child_nebulae(x)) <-
      lapply(names(tbl_graph(child_nebulae(x))),
             function(name){
               layout <- ggraph_layouts[[name]]
               if (is.null(layout))
                 layout <- .propose_graph_layout(graph)
               set.seed(seeds[[name]])
               ggraph::create_layout(tbl_graph(child_nebulae(x))[[ name ]],
                                     layout = layout)
             })
    names(layout_ggraph(child_nebulae(x))) <-
      names(tbl_graph(child_nebulae(x)))
    grid_layout(child_nebulae(x)) <- grid_layout
    viewports(child_nebulae(x)) <- viewports
    panel_viewport(child_nebulae(x)) <- panel_viewport
    legend_viewport(child_nebulae(x)) <- legend_viewport
    return(x)
  }
.propose_graph_layout <- 
  function(graph){
    if (length(graph) >= 300 | length(graph) <= 10)
      "kk"
    else
      "fr"
  }
