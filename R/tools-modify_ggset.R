# ==========================================================================
# functions to modify 'ggset' object
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases fun_modify
#'
#' @title Modify 'ggset' object
#'
#' @description
#' These are multiple functions used for post modification of [ggset-class]
#' object. These functions provide a convenient, fast, and repeatable way
#' to make improvements to [ggset-class] object.
#'
#' @param ggset [ggset-class] object.
#' @param x [mcnebula-class] object.
#' 
#' @seealso [ggset-class]
#'
#' @name fun_modify
NULL
#> NULL

#' @export modify_default_child
#' @aliases modify_default_child
#' 
#' @description \code{modify_default_child}:
#' Used for \code{visualize_all()}.
#' \code{modify_rm_legend} + \code{modify_set_labs} + \code{modify_unify_scale_limits}.
#' In addition, if the 'use_tracer' is TRUE (see [set_nodes_color()]),
#' \code{modify_tracer_node} and \code{modify_color_edge} would be performed.
#' 
#' @rdname fun_modify
modify_default_child <- 
  function(ggset, x){
    x <- .get_missing_x(x, "mcnebula")
    maps <- .get_mapping2(ggset)
    if (maps[[ "fill" ]] == "tracer")
      ggset <- modify_color_edge(modify_tracer_node(ggset), "lightblue")
    modify_rm_legend(modify_set_labs(modify_unify_scale_limits(ggset)))
  }

#' @export modify_stat_child
#' @aliases modify_stat_child
#' 
#' @description \code{modify_stat_child}:
#' Repalce [scale_fill_gradientn()] with [scale_fill_gradient2()] in 'layers';
#' unify the "aes" scale except for "fill";
#' perfrom [modify_set_labs()];
#' only keep the legend for 'fill', and adjust its width;
#' move the position of the legend to the bottom;
#' remove the title of the legend.
#' 
#' @rdname fun_modify
modify_stat_child <- 
  function(ggset, x) {
    x <- .get_missing_x(x, "mcnebula")
    ## replace
    seq <- grep("scale_fill_gradientn", names(layers(ggset)))
    if (is.integer(seq) & length(seq) > 0)
      ggset <- delete_layers(ggset, seq)
    args <- list(low = "blue", mid = "grey90", high = "red", na.value = "white")
    pal <- palette_gradient(x)
    pal <- pal[names(pal) %in% names(args)]
    args <- .fresh_param(args, as.list(pal))
    breaks <- function(x) round(seq(floor(min(x)), ceiling(max(x)), length.out = 7), 1)
    command <- do.call(new_command, c(fun = scale_fill_gradient2,
                                      breaks = breaks, args,
                                      name = "scale_fill_gradient2"))
    ggset <- add_layers(ggset, command)
    ## unify and set labs
    aes_name <- names(.get_mapping2(ggset))
    ggset <- modify_unify_scale_limits(ggset, aes_name = aes_name[aes_name != "fill"])
    ggset <- modify_set_labs(ggset)
    ## ...
    args <- sapply(aes_name, simplify = F,
                   function(name) {
                     if (name == "fill")
                       guide_colorbar(title = NULL, barheight = grid::unit(.5, "line"))
                     else F
                   })
    if (any(grepl("^guides|ggplot2::guides", names(layers(ggset)))))
      ggset <- do.call(mutate_layer, c(list(x = ggset, layer = "guides"), args))
    else {
      command <- do.call(new_command,
                         c(fun = match.fun("guides"), args, name = "guides"))
      ggset <- add_layers(ggset, command)
    }
    ggset <- mutate_layer(ggset, "theme", legend.position = "bottom")
    attr(ggset, "modify") <- "rev.modify_stat_child"
    ggset
  }

rev.modify_stat_child <- 
  function(ggset){
    args <- sapply(names(.get_mapping2(ggset)), simplify = F,
                   function(name) {
                     if (name == "fill") F else NULL
                   })
    ggset <- do.call(mutate_layer, c(list(x = ggset, layer = "guides"), args))
    ggset <- mutate_layer(ggset, "theme", legend.position = "right")
    ggset
  }

#' @export modify_set_labs_and_unify_scale_limits
#' @aliases modify_set_labs_and_unify_scale_limits
#' 
#' @description \code{modify_set_labs_and_unify_scale_limits}:
#' \code{modify_set_labs} + \code{modify_unify_scale_limits}
#' 
#' @rdname fun_modify
modify_set_labs_and_unify_scale_limits <- 
  function(ggset, x){
    x <- .get_missing_x(x, "mcnebula")
    modify_set_labs(modify_unify_scale_limits(ggset))
  }

#' @export modify_annotate_child
#' @aliases modify_annotate_child
#' 
#' @description \code{modify_annotate_child}:
#' \code{modify_set_labs} + ...
#' (for parameters of \code{panel.grid} and \code{panel.background}
#' in [ggplot2::theme()]).
#' 
#' @rdname fun_modify
modify_annotate_child <- 
  function(ggset, x){
    x <- .get_missing_x(x, "mcnebula")
    mutate_layer(modify_set_labs(ggset), "theme",
                 panel.grid = element_line("white", inherit.blank = T),
                 panel.background = element_rect("grey92", color = NA,
                                                 inherit.blank = T))
  }

#' @export modify_rm_legend
#' @aliases modify_rm_legend
#' 
#' @description \code{modify_rm_legend}: remove the legend.
#' For parameter of \code{legend.position} in [ggplot2::theme()].
#' 
#' @rdname fun_modify
modify_rm_legend <- 
  function(ggset){
    mutate_layer(ggset, "theme", legend.position = "none")
  }

#' @export modify_tracer_node
#' @aliases modify_tracer_node
#' @description \code{modify_tracer_node}: Set the stroke for nodes in
#' Nebulae (network) as 0, and the color as 'transparent';
#' Override the node color (border color) in legend.
#' @rdname fun_modify
modify_tracer_node <- 
  function(ggset){
    seq <- grep("geom_node_point", names(layers(ggset)))
    ggset <- mutate_layer(ggset, seq, stroke = 0, color = "transparent")
    ## override the nodes boder color in legend 
    seq <- grep("^guides|ggplot2::guides", names(layers(ggset)))
    size_legend <- guide_legend(override.aes = list(stroke = .3, color = "black"))
    fill_legend <- guide_legend(override.aes = list(size = 4))
    if (length(seq) > 0)
      ggset <- mutate_layer(ggset, seq, size = size_legend, fill = fill_legend)
    else {
      command <- new_command(match.fun("guides"), size = size_legend,
                             fill = fill_legend, name = "guides")
      ggset <- add_layers(ggset, command)
    }
  }

#' @export modify_color_edge
#' @aliases modify_color_edge
#' @description \code{modify_color_edge}: Set color for edge.
#' @param color character(1).
#' @rdname fun_modify
modify_color_edge <- 
  function(ggset, color){
    seq <- grep("geom_edge_", names(layers(ggset)))
    mutate_layer(ggset, seq, color = color)
  }

#' @importFrom grid unit
#' @export modify_set_margin
#' @aliases modify_set_margin
#' 
#' @description \code{modify_set_margin}: reduce margin.
#' For parameter of \code{plot.margin} in [ggplot2::theme()].
#' 
#' @rdname fun_modify
modify_set_margin <- 
  function(ggset, margin = grid::unit(rep(-8, 4), "lines")){
    mutate_layer(ggset, "theme", plot.margin = margin)
  }

#' @export modify_unify_scale_limits
#' @aliases modify_unify_scale_limits
#' 
#' @description \code{modify_unify_scale_limits}:
#' Uniform mapping 'scale' for all Child-Nebulae.
#' Related to \code{ggplot2::scale_*} function.
#' Use \code{MCnebula2:::.LEGEND_mapping()} to get the possibly mapping.
#'
#' @param aes_name character. Specify which 'aes' to unify scale,
#' e.g., c("fill", "size", "edge_width").
#' 
#' @rdname fun_modify
modify_unify_scale_limits <- 
  function(ggset, x, aes_name = NA){
    x <- .get_missing_x(x, "mcnebula")
    .check_data(x, list(features_annotation = "create_features_annotation",
                        spectral_similarity = "compute_spectral_similarity"))
    layers_name <- names(layers(ggset))
    args <- as.list(.get_mapping2(ggset))
    if (is.logical(aes_name))
      aes_name <- .LEGEND_mapping()
    for (i in aes_name) {
      if (is.null(args[[ i ]])) {
        next
      }
      if (i == "edge_width") {
        attr <- spectral_similarity(x)[[ args[[i]] ]]
        fun <- paste0("scale_", i)
      } else {
        attr <- features_annotation(x)[[ args[[i]] ]]
        if (is.null(attr)) {
          attr <- attr(features_annotation(x), "extra_data")[[ args[[i]] ]]
          if (is.null(attr))
            stop(paste0("Not found attribute '", args[[i]],
                        "' in `features_annotation(x)`."))
        }
        fun <- paste0("scale_", i, "_continuous")
      }
      if (!is.numeric(attr)) {
        next
      }
      range <- range(attr, na.rm = T)
      seq <- grep(paste0("^scale_", i, "|^ggplot2::scale_", i), layers_name)
      if (length(seq) == 1) {
        ggset <- mutate_layer(ggset, seq, limits = range)
      } else if (length(seq) > 1) {
        stop(paste0("multiple layers of 'scale_", i,
             ".*", "' were found"))
      } else {
        ggset <-
          add_layers(ggset,
                     new_command(match.fun(fun),
                                 limits = range,
                                 name = fun
                                 ))
      }
    }
    ggset
  }

#' @export modify_set_labs_xy
#' @aliases modify_set_labs_xy
#'
#' @description \code{modify_set_labs_xy}:
#' According to names in slot \code{export_name} of [mcnebula-class] object
#' to rename the labs of x and y axis.
#' 
#' @rdname fun_modify
modify_set_labs_xy <- 
  function(ggset, x){
    x <- .get_missing_x(x, "mcnebula")
    .modify_set_labs(ggset, x, c("x", "y"))
  }

#' @export modify_set_labs
#' @aliases modify_set_labs
#' 
#' @description \code{modify_set_labs}:
#' According to names in slot \code{export_name} of [mcnebula-class] object
#' to rename the labs of legends.
#' 
#' @rdname fun_modify
modify_set_labs <- 
  function(ggset, x){
    x <- .get_missing_x(x, "mcnebula")
    .modify_set_labs(ggset, x)
  }

.modify_set_labs <- 
  function(ggset, x, ...) {
    export_name <- as.list(export_name(x))
    mapping <- vecter_unique_by_names(.get_mapping2(ggset, ...))
    args <- vapply(mapping, FUN.VALUE = "ch",
                   function(attr) {
                     if (is.null(export_name[[ attr ]]))
                       attr
                     else
                       export_name[[ attr ]]
                   })
    seq <- grep("^labs$|^ggplot2::labs$", names(layers(ggset)))
    if ( length(seq) == 1) {
      ggset <- do.call(mutate_layer, c(ggset, seq, args))
    } else if ( length(seq) > 1 ) {
      stop( "multiple layers of 'labs' were found" )
    } else {
      ggset <- do.call(add_layers,
                       c(ggset, do.call(new_command,
                                        c(match.fun(labs),
                                          args, name = "labs"))))
    }
    ggset
  }

#' @importFrom stringr str_extract
.get_mapping2 <-
  function(ggset, maps = .LEGEND_mapping()){
    args <- .get_mapping(ggset)
    pattern <- "[a-z|A-Z|.|_|0-9]{1,}"
    args[] <-
      stringr::str_extract(args,
                           paste0("(?<=\\()", pattern, "(?=\\),)",
                                  "|^", pattern, "$"))
    args[names(args) %in% maps]
  }

.LEGEND_mapping <- 
  function(){
    c("fill", "color", "colour", "alpha", "size", "edge_width")
  }

.get_mapping <- 
  function(ggset){
    unlist(lapply(unname(layers(ggset)),
                  function(com){
                    mapping <- command_args(com)$mapping
                    if (!is.null(mapping)) {
                      vapply(mapping, FUN.VALUE = "ch",
                             function(m) tail(paste0(m), 1))
                    }
                  }))
  }

