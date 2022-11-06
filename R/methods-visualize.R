# ==========================================================================
# extract and visualize 'ggset' in 'mcnebula' object
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases visualize
#'
#' @title ...
#'
#' @description ...
#'
#' @details ...
#'
#' @name visualize-methods
#'
#' @order 1
NULL
#> NULL

#' @importFrom tibble tibble
setClassUnion("numeric_or_character", c("numeric", "character"))
#' @exportMethod visualize
#' @description \code{visualize(x)}: get a 'tbl' about child-nebulae candidates
#' for \code{visualize} methods to visualize.
#' @rdname visualize-methods
setMethod("visualize", 
          signature = setMissing("visualize",
                                 x = "mcnebula",
                                 fun_modify = "ANY"),
          function(x, fun_modify){
            .message_info_formal("MCnebula2", "visualize")
            cat("\tSpecify item as following to visualize:\n\n")
            class.name <- names(ggset(child_nebulae(x)))
            hierarchy <- vapply(class.name, function(c, h) h[[c]], 1,
                                h = .get_hierarchy(x))
            tibble::tibble(seq = 1:length(class.name),
                           hierarchy = hierarchy,
                           class.name = class.name
            )
          })
#' @exportMethod visualize
#' @description \code{visualize()}: get the default parameters for the method
#' \code{visualize}.
#' @rdname visualize-methods
setMethod("visualize", 
          signature = setMissing("visualize"),
          function(){
            list(fun_modify = modify_set_labs)
          })
#' @exportMethod visualize
#' @description \code{visualize(x, ...)}: use the default parameters whatever 'missing'
#' while performing the method \code{visualize}.
#' @rdname visualize-methods
setMethod("visualize", 
          signature = c(x = "mcnebula"),
          function(x, item, fun_modify, annotate){
            reCallMethod("visualize", .fresh_param(visualize()))
          })
#' @exportMethod visualize
#'
#' @aliases visualize
#'
#' @title ...
#'
#' @description ...
#'
#' @details ...
#'
#' @param x ...
#' @param item ...
#' @param fun_modify ...
#' @param annotate ...
#'
#' @rdname visualize-methods
#'
#' @examples
#' \dontrun{
#' visualize(...)
#' }
setMethod("visualize", 
          signature = setMissing("visualize",
                                 x = "mcnebula",
                                 item = "character",
                                 fun_modify = "function"),
          function(x, item, fun_modify){
            .message_info_formal("MCnebula2", "visualize")
            if (item == "parent") {
              call_command(fun_modify(ggset(parent_nebula(x))))
            } else {
              obj <- ggset(child_nebulae(x))[[ item ]]
              if (!is.null(obj)) {
                call_command(fun_modify(obj))
              } else {
                stop( "the `item` not found in `ggset(child_nebula(x))`" )
              }
            }
          })
#' @exportMethod visualize
#' @rdname visualize-methods
setMethod("visualize", 
          signature = setMissing("visualize",
                                 x = "mcnebula",
                                 item = "numeric",
                                 fun_modify = "function"),
          function(x, item, fun_modify){
            .message_info_formal("MCnebula2", "visualize")
            call_command(fun_modify(ggset(child_nebulae(x))[[ item ]]))
          })
#' @exportMethod visualize
#' @rdname visualize-methods
setMethod("visualize", 
          signature = setMissing("visualize",
                                 x = "mcnebula",
                                 item = "numeric_or_character",
                                 fun_modify = "function",
                                 annotate = "logical"),
          function(x, item, fun_modify, annotate){
            if (annotate) {
              obj <- ggset_annotate(child_nebulae(x))[[ item ]]
              if (is.null(obj)) {
                stop( "the `item` not found in `ggset_annotate(child_nebula(x))`" )
              } else {
                call_command(fun_modify(obj))
              }
            } else {
              visualize(x, item)
            }
          })
#' @exportMethod visualize_all
#' @description \code{visualize_all()}: get the default parameters for the method
#' \code{visualize_all}.
#' @rdname visualize-methods
setMethod("visualize_all", 
          signature = setMissing("visualize_all",
                                 x = "missing"),
          function(){
            list(newpage = T,
                 fun_modify = modify_default_child,
                 legend_hierarchy = T
            )
          })
#' @exportMethod visualize_all
#' @description \code{visualize_all(x, ...)}: use the default parameters whatever 'missing'
#' while performing the method \code{visualize_all}.
#' @rdname visualize-methods
setMethod("visualize_all", 
          signature = c(x = "mcnebula"),
          function(x, newpage, fun_modify, legend_hierarchy){
            reCallMethod("visualize_all",
                         .fresh_param(visualize_all()))
          })
#' @importFrom grid grid.newpage
#' @importFrom grid viewport
#' @importFrom grid pushViewport
#' @importFrom grid upViewport
#' @importFrom grid grid.draw
#' @exportMethod visualize_all
#'
#' @description ...
#'
#' @param x ...
#' @param newpage ...
#' @param fun_modify ...
#' @param legend_hierarchy ...
#'
#' @rdname visualize-methods
#'
#' @examples
#' \dontrun{
#' visualize_all(...)
#' }
setMethod("visualize_all", 
          signature = setMissing("visualize_all",
                                 x = "mcnebula",
                                 newpage = "logical",
                                 fun_modify = "function",
                                 legend_hierarchy = "logical"),
          function(x, newpage, fun_modify, legend_hierarchy){
            .message_info_formal("MCnebula2", "visualize_all")
            set <- child_nebulae(x)
            if (newpage)
              grid::grid.newpage()
            .message_info_viewport("BEGIN")
            grid::pushViewport(panel_viewport(set))
            layer <- 1
            .message_info_viewport()
            if (legend_hierarchy) {
              .visualize_legend_hierarchy(set)
              layer <- layer + 1
            }
            layer <- layer +
              .visualize_child_nebulae(set, fun_modify)
            grid::upViewport(layer)
            .message_info_viewport()
            .visualize_legend_nebulae(set, fun_modify)
            .message_info_viewport("END")
          })
.visualize_child_nebulae <- 
  function(set, fun_modify = modify_default_child, x){
    x <- .get_missing_x(x, "mcnebula")
    if (!is.null(grid_layout(set))) {
      grid::pushViewport(grid::viewport(layout = grid_layout(set)))
      layer <- 1
    } else {
      layer <- 0
    }
    lapply(names(ggset(set)),
           function(name){
             print(call_command(fun_modify(ggset(set)[[ name ]])),
                   vp = viewports(set)[[ name ]],
                   newpage = F)
           })
    return(layer)
  }
.visualize_legend_nebulae <- 
  function(set, fun_modify = modify_default_child, x){
    x <- .get_missing_x(x, "mcnebula")
    grid::pushViewport(legend_viewport(set))
    .message_info("visualize", "legend:",
                paste0("\n\textract legend from ",
                       "`ggset(child_nebulae(x))[[1]]` ",
                       "(nebula names:", names(ggset(set)[[1]]), ").",
                       "\n\tIn default, legend scales have been unified ",
                       "for all child-nebulae."
                       ))
    grob <- .get_legend(call_command(fun_modify(ggset(set)[[1]])))
    grid::grid.draw(grob)
  }
.visualize_legend_hierarchy <- 
  function(set, x){
    x <- .get_missing_x(x, "mcnebula")
    grob <- .legend_hierarchy(set)
    pushViewport(viewport(0.5, 0, 1, 0.1,
                          just = c("centre", "bottom"),
                          name = "legend_hierarchy"))
    .message_info_viewport()
    grid::grid.draw(grob)
    upViewport(1)
    pushViewport(viewport(0.5, 0.1, 1, 0.9,
                          just = c("centre", "bottom"),
                          name = "sub_panel"))
    .message_info_viewport()
  }
.legend_hierarchy <- 
  function(set, x){
    x <- .get_missing_x(x, "mcnebula")
    theme <- layers(ggset(set)[[1]])$theme
    if (is.null(theme)) {
      theme <- new_command(match.fun("theme"), name = "theme")
    }
    class.names <- names(ggset(set))
    .check_data(x, list("hierarchy" = "create_hierarchy"))
    hierarchy <- .get_hierarchy(x)
    hierarchy <- vapply(class.names, function(name) hierarchy[[name]], 1)
    color <- vapply(hierarchy, function(n) palette_label(x)[[n]], "ch")
    names(color) <- paste0("Level ", hierarchy)
    .grob_legend_hierarchy_plot(color, call_command(theme))
  }

