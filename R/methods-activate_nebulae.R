# ==========================================================================
# make up 'ggset' (a meta class for visualizing ggplot object),
# based on layout of parent nebula and child nebulae.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportMethod activate_nebulae
#' @description \code{activate_nebulae()}: get the default parameters for the method
#' \code{activate_nebulae}.
#' @rdname activate_nebulae-methods
setMethod("activate_nebulae", 
          signature = setMissing("activate_nebulae"),
          function(){
            list(fun_default_parent = ggset_activate_parent_nebula,
                 fun_default_child = ggset_activate_child_nebulae)
          })
#' @exportMethod activate_nebulae
#' @description \code{activate_nebulae(x, ...)}: use the default parameters whatever 'missing'
#' while performing the method \code{activate_nebulae}.
#' @rdname activate_nebulae-methods
setMethod("activate_nebulae", 
          signature = c(x = "mcnebula"),
          function(x, fun_default_parent, fun_default_child){
            reCallMethod("activate_nebulae",
                         .fresh_param(activate_nebulae()))
          })
#' @exportMethod activate_nebulae
#'
#' @aliases activate_nebulae
#'
#' @title ...
#'
#' @description ...
#'
#' @details ...
#'
#' @param x ...
#' @param fun_default_parent ...
#' @param fun_default_child ...
#'
# @inheritParams rdname
#'
#' @return ...
#'
#' @seealso [fun()]
#'
#' @rdname activate_nebulae-methods
#'
#' @order 1
#'
#' @examples
#' \dontrun{
#' activate_nebulae(...)
#' }
setMethod("activate_nebulae", 
          signature = c(x = "mcnebula",
                        fun_default_parent = "function",
                        fun_default_child = "function"),
          function(x, fun_default_parent, fun_default_child){
            .message_info_formal("MCnebula2", "activate_nebulae")
            ggset(parent_nebula(x)) <- fun_default_parent(x)
            ggset(child_nebulae(x)) <- fun_default_child(x)
            return(x)
          })
#' @description FUNCTION_DESCRIPTION
#' @param x PARAM_DESCRIPTION
#' @return OUTPUT_DESCRIPTION
#' @details DETAILS
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname activate_nebulae-methods
#' @export 
#' @importFrom ggraph ggraph
ggset_activate_parent_nebula <- 
  function(x){
    new_ggset(new_command(ggraph::ggraph, layout_ggraph(parent_nebula(x))),
              .command_parent_edge(),
              .command_parent_node(),
              .command_parent_edge_width(),
              .command_parent_fill(palette_gradient(x)),
              new_command(match.fun(theme_grey), name = "theme_grey"),
              .command_parent_theme()
    )
  }
#' @description ...
#' @param x ...
#' @return ...
#' @details ...
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname activate_nebulae-methods
#' @export 
#' @importFrom ggraph ggraph
ggset_activate_child_nebulae <-
  function(x){
    set <- layout_ggraph(child_nebulae(x))
    hierarchy <- .get_hierarchy(x)
    ggsets <-
      lapply(names(set),
             function(name){
               fill <- palette_label(x)[[ hierarchy[[name]] ]]
               new_ggset(new_command(ggraph::ggraph, set[[ name ]]),
                         .command_parent_edge("black"),
                         .command_parent_node(),
                         .command_parent_edge_width(),
                         .command_parent_fill(palette_gradient(x)),
                         .command_child_title(name),
                         new_command(match.fun(theme_grey), name = "theme_grey"),
                         .command_child_theme(fill)
               )
             })
    names(ggsets) <- names(set)
    return(ggsets)
  }
.get_node_attribute_range <- function(x, attr){
  .check_data(x, list("features_annotation" = "create_features_annotation"))
  range(features_annotation(x)[[ attr ]])
}
.get_hierarchy <- 
  function(x){
    hierarchy <- as.list(hierarchy(x)[["hierarchy"]])
    names(hierarchy) <- hierarchy(x)[["class.name"]]
    return(hierarchy)
  }
.get_textbox_fill <-
  function(x, class.name){
    if (missing(class.name)) {
      class.name <- names(igraph(child_nebulae(x)))
    }
    hierarchy <- .get_hierarchy(x)
    vapply(class.name, FUN.VALUE = "ch",
           function(name){
             palette_label(x)[[ hierarchy[[ name ]] ]]
           })
  }
