# # ==========================================================================
# for creating child layout, these layouts includes:
# layouts for nodes position of ggraph;
# layouts of size and position of grid panel;
# layouts of viewports of each child_nebula (in which grid panel).
# layout of viewport of panel of overall child_nebulae;
# layout of viewport of legend of child_nebulae.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @importFrom grid grid.layout
#' @importFrom grid viewport
#' @importFrom grid pushViewport
#' @importFrom dplyr desc
#' @importFrom dplyr arrange
#' @importFrom dplyr select
setMethod("create_child_layouts", 
          signature = c(x = "mcnebula"),
          function(x, ggraph_layouts, seeds,
                   grid_layout, viewports,
                   panel_viewport, legend_viewport){
            .get_info_formal("MCnebula2", "create_child_layouts")
            .check_data(child_nebulae(x), list(igraph = "create_child_nebulae"))
            len <- length(igraph(child_nebulae(x)))
            if (missing(ggraph_layouts)) {
              ggraph_layouts <-
                vapply(igraph(child_nebulae(x)), .default_graph_layout, "ch")
            }
            if (missing(seeds)) {
              seeds <- rep(1, len)
            }
            if (missing(grid_layout)) {
              ncol <- round(sqrt(len))
              if (ncol ^ 2 < len) {
                nrow <- ncol + 1
              } else {
                nrow <- ncol
              }
              grid_layout <- grid::grid.layout(nrow, ncol)
            }
            if (missing(viewports)) {
              num_grid <- grid_layout$nrow * grid_layout$ncol
              if (num_grid < len) {
                stop( "`grid_layout` must contains enough sub-panels for child_nebulae." )
              }
              names <- as.character(sort(factor(names(igraph(child_nebulae(x))),
                                                levels = hierarchy(x)$class.name)))
              mtrx <- matrix(c(names, rep("...FILL", num_grid - len)),
                             ncol = grid_layout$ncol, byrow = T)
              viewports <- unlist(recursive = F,
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
            }
            if (missing(panel_viewport)) {
              panel_viewport <-
                grid::viewport(0, 0.5, 0.8, 1, just = c("left", "centre"))
            }
            if (missing(legend_viewport)) {
              legend_viewport <- 
                grid::viewport(0.8, 0.5, 0.2, 1, just = c("left", "centre"))
            }
            .create_child_layouts(x, ggraph_layouts, seeds,
                                  grid_layout, viewports,
                                  panel_viewport, legend_viewport)
          })
.create_child_layouts <- 
  function(x, ggraph_layouts, seeds,
           grid_layout, viewports,
           panel_viewport, legend_viewport
           ){
    set <- igraph(child_nebulae(x))
    if (is.null(names(ggraph_layouts)))
      names(ggraph_layouts) <- names(set)[1:length(set)]
    if (is.null(names(seeds)))
      names(seeds) <- names(set)[1:length(set)]
    if (!is.null(grid_layout))
      .check_class(grid_layout)
    .check_names(viewports, set, "viewports", "igraph(child_nebulae(x))")
    if (length(viewports) != length(set)) {
      stop(paste0("`viewports` must be a list ",
                  "the same length as 'igraph(child_nebulae(x))'."))
    }
    if (is.null(names(viewports)))
      names(viewports) <- names(set)
    .check_class(panel_viewport, "viewport", "grid::viewport")
    .check_class(legend_viewport, "viewport", "grid::viewport")
    tbl_graph(child_nebulae(x)) <- lapply(set, tidygraph::as_tbl_graph)
    layout_ggraph(child_nebulae(x)) <-
      lapply(names(tbl_graph(child_nebulae(x))),
             function(name){
               graph <- tbl_graph(child_nebulae(x))[[ name ]]
               seed <- seeds[[name]]
               if (is.null(seed))
                 seed <- 1
               layout <- ggraph_layouts[[name]]
               if (is.null(layout))
                 layout <- .default_graph_layout(graph)
               set.seed(seed)
               ggraph::create_layout(graph, layout = layout)
             })
    names(layout_ggraph(child_nebulae(x))) <-
      names(tbl_graph(child_nebulae(x)))
    grid_layout(child_nebulae(x)) <- grid_layout
    viewports(child_nebulae(x)) <- viewports
    panel_viewport(child_nebulae(x)) <- panel_viewport
    legend_viewport(child_nebulae(x)) <- legend_viewport
    return(x)
  }
.default_graph_layout <- 
  function(graph){
    if (length(graph) >= 300 | length(graph) <= 10)
      "kk"
    else
      "fr"
  }
