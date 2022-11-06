# ==========================================================================
# functions to modify 'ggset' object
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases fun_modify
#'
#' @title ...
#'
#' @description ...
#'
#' @param ggset ...
#' @param x ...
#'
#' @details ...
#'
#' @name fun_modify
NULL
#> NULL

#' @export modify_default_child
#' @aliases modify_default_child
#' @description \code{modify_default_child}: ...
#' @rdname fun_modify
modify_default_child <- 
  function(ggset, x){
    x <- .get_missing_x(x, "mcnebula")
    modify_rm_legend(modify_set_labs(modify_unify_scale_limits(ggset)))
  }
#' @export modify_set_labs_and_unify_scale_limits
#' @aliases modify_set_labs_and_unify_scale_limits
#' @description \code{modify_set_labs_and_unify_scale_limits}: ...
#' @rdname fun_modify
modify_set_labs_and_unify_scale_limits <- 
  function(ggset, x){
    x <- .get_missing_x(x, "mcnebula")
    modify_set_labs(modify_unify_scale_limits(ggset))
  }
#' @export modify_annotate_child
#' @aliases modify_annotate_child
#' @description \code{modify_annotate_child}: ...
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
#' @description \code{modify_rm_legend}: ...
#' @rdname fun_modify
modify_rm_legend <- 
  function(ggset){
    mutate_layer(ggset, "theme", legend.position = "none")
  }
#' @importFrom grid unit
#' @export modify_set_margin
#' @aliases modify_set_margin
#' @description \code{modify_set_margin}: ...
#' @rdname fun_modify
modify_set_margin <- 
  function(ggset, margin = grid::unit(rep(-8, 4), "lines")){
    mutate_layer(ggset, "theme", plot.margin = margin)
  }
#' @export modify_unify_scale_limits
#' @aliases modify_unify_scale_limits
#' @description \code{modify_unify_scale_limits}: ...
#' @rdname fun_modify
modify_unify_scale_limits <- 
  function(ggset, x){
    x <- .get_missing_x(x, "mcnebula")
    .check_data(x, list(features_annotation = "create_features_annotation",
                        spectral_similarity = "compute_spectral_similarity"))
    layers_name <- names(layers(ggset))
    args <- as.list(.get_mapping2(ggset))
    for (i in .LEGEND_mapping()) {
      if (is.null(args[[ i ]])) {
        next
      }
      if (i == "edge_width") {
        attr <- spectral_similarity(x)[[ args[[i]] ]]
        fun <- paste0("scale_", i)
      } else {
        attr <- features_annotation(x)[[ args[[i]] ]]
        fun <- paste0("scale_", i, "_continuous")
      }
      if (!is.numeric(attr)) {
        next
      }
      range <- range(attr)
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
#' @export modify_set_labs
#' @aliases modify_set_labs
#' @description \code{modify_set_labs}: ...
#' @rdname fun_modify
modify_set_labs <- 
  function(ggset, x){
    x <- .get_missing_x(x, "mcnebula")
    export_name <- as.list(export_name(x))
    args <- vapply(.get_mapping2(ggset), FUN.VALUE = "ch",
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
  function(ggset, only_legend = T){
    args <- .get_mapping(ggset)
    pattern <- "[a-z|A-Z|.|_]{1,}"
    args[] <-
      stringr::str_extract(args,
                           paste0("(?<=\\()", pattern, "(?=\\),)",
                                  "|^", pattern, "$"))
    if (only_legend) {
      args <- args[names(args) %in% .LEGEND_mapping()]
    }
    args
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

