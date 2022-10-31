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
# @seealso ...
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
            if (!is.null(layout_ggraph(parent_nebula(x)))) {
              ggset(parent_nebula(x)) <- fun_default_parent(x)
              parent <- T
            } else {
              parent <- F
            }
            if (!is.null(layout_ggraph(child_nebulae(x)))) {
              ggset(child_nebulae(x)) <- fun_default_child(x)
              child <- T
            } else {
              child <- F
            }
            if (any(!(parent | child)))
              stop("nothing need to be activate")
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
    .check_data(parent_nebula(x), list(layout_ggraph = "create_parent_layout"))
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
    .check_data(child_nebulae(x), list(layout_ggraph = "create_child_layouts"))
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
#' @importFrom dplyr select
#' @exportMethod set_nodes_color
#'
#' @aliases set_nodes_color
#'
#' @title ...
#'
#' @description ...
#'
#' @details ...
#'
#' @param x ...
#' @param attribute ...
#' @param extra_data ...
#' @param use_tracer ...
#'
# @inheritParams rdname
#'
#' @return ...
#'
#' @seealso [activate_nebulae()], [visualize()]...
#'
#' @rdname set_nodes_color-methods
#'
#' @order 1
#'
#' @examples
#' \dontrun{
#' set_nodes_color(...)
#' }
setMethod("set_nodes_color", 
          signature = setMissing("set_nodes_color",
                                 x = "mcnebula",
                                 attribute = "character",
                                 extra_data = "data.frame"),
          function(x, attribute, extra_data){
            .check_data(child_nebulae(x), list(ggset = "activate_nebulae"))
            if (length(attribute) != 1) {
              stop( "`attribute` must be a character `length(attribute) == 1`." )
            }
            if (any(!(c(".features_id", attribute) %in% colnames(extra_data)))) {
              stop("extra_data must contains columns of '.features_id' and the ",
                   "specified `attribute`.")
            }
            if (is.numeric(extra_data[[ attribute ]])) {
              command_scale <- .command_parent_fill(palette_gradient(x))
            } else {
              command_scale <- .command_parent_fill2(palette_set(x))
            }
            attribute <- as.name(attribute)
            aes_ex <- aes(fill = !!attribute)
            ggset(child_nebulae(x)) <-
              lapply(ggset(child_nebulae(x)),
                     function(ggset) {
                       data <- command_args(layers(ggset)[[ 
                                            "ggraph::ggraph" 
                                            ]])[[ "graph" ]]
                       mapped_attr <- .get_mapping2(ggset)
                       mapped_attr <- mapped_attr[names(mapped_attr) != "fill"]
                       mapped_attr <- mapped_attr[mapped_attr %in% colnames(data)]
                       data <- dplyr::select(data, x, y, name,
                                             dplyr::all_of(unname(mapped_attr)))
                       data <- merge(data, extra_data, by.x = "name", by.y = ".features_id",
                                     all.x = T)
                       mapping <-
                         command_args(layers(ggset)[[ "ggraph::geom_node_point" ]])$mapping
                       mapping$fill <- NULL
                       mapping <- ggraph:::aes_intersect(mapping, aes_ex)
                       ggset <- mutate_layer(ggset, "ggraph::geom_node_point",
                                             data = data,
                                             mapping = mapping)
                       seq <- grep(paste0("^scale_fill|^ggplot2::scale_fill"),
                                   names(layers(ggset)))
                       layers(ggset)[[ seq ]] <- NULL
                       add_layers(ggset, command_scale)
                     })
            return(x)
          })
#' @exportMethod set_nodes_color
#' @rdname set_nodes_color-methods
setMethod("set_nodes_color", 
          signature = setMissing("set_nodes_color",
                                 x = "mcnebula",
                                 attribute = "character"),
          function(x, attribute){
            attr <- as.name(attribute)
            aes_ex <- aes(fill = !!attr)
            ggset(child_nebulae(x)) <-
              lapply(ggset(child_nebulae(x)),
                     function(ggset) {
                       mapping <-
                         command_args(layers(ggset)[[
                                      "ggraph::geom_node_point"
                                      ]])$mapping
                       mapping$fill <- NULL
                       mapping <- ggraph:::aes_intersect(mapping, aes_ex)
                       ggset <- mutate_layer(ggset, "ggraph::geom_node_point",
                                             data = NULL,
                                             mapping = mapping)
                       data <- command_args(layers(ggset)[[ 
                                            "ggraph::ggraph"
                                            ]])[[ "graph" ]]
                       if (is.numeric(data[[ attribute ]])) {
                         command_scale <- .command_parent_fill(palette_gradient(x))
                       } else {
                         command_scale <- .command_parent_fill2(palette_set(x))
                       }
                       seq <- grep(paste0("^scale_fill|^ggplot2::scale_fill"),
                                   names(layers(ggset)))
                       layers(ggset)[[ seq ]] <- NULL
                       add_layers(ggset, command_scale)
                     })
            return(x)
          })
#' @exportMethod set_nodes_color
#' @rdname set_nodes_color-methods
setMethod("set_nodes_color", 
          signature = setMissing("set_nodes_color",
                                 x = "mcnebula",
                                 use_tracer = "logical"),
          function(x, use_tracer){
            .check_data(x, list(nebula_index = "create_nebula_index"))
            if (use_tracer & is.logical(nebula_index(x)[[ "tracer" ]])) {
              data <- dplyr::distinct(nebula_index(x),
                                      .features_id, tracer_color, tracer)
              data <- dplyr::mutate(data, tracer = ifelse(tracer, .features_id,
                                                          "Others"))
              pal <- dplyr::distinct(data, tracer, tracer_color)
              palette_set(melody(x)) <-
                .as_dic(pal$tracer_color, pal$tracer, fill = F, as.list = F)
              x <- set_nodes_color(x, attribute = "tracer", extra_data = data)
            }
            return(x)
          })
