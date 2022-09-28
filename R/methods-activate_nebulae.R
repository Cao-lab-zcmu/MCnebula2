# ==========================================================================
# make up 'ggset' (a meta class for visualizing ggplot object),
# based on layout of parent nebula and child nebulae.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("activate_nebulae", 
          signature = setMissing("activate_nebulae",
                                 x = "mcnebula"),
          function(x){
            activate_nebulae(x, default_vis_parent_nebula,
                             default_vis_child_nebulae)
          })
setMethod("activate_nebulae", 
          signature = c(x = "mcnebula",
                        fun_default_parent = "function",
                        fun_default_child = "function"),
          function(x, fun_default_parent, fun_default_child){
            ggset(parent_nebula(x)) <- fun_default_parent(x)
            ggset(child_nebulae(x)) <- fun_default_child(x)
            return(x)
          })
#' @importFrom ggraph ggraph
default_vis_parent_nebula <- 
  function(x){
    set_ggset(set_command(ggraph::ggraph, layout_ggraph(parent_nebula(x))),
              .default_parent_edge(),
              .default_parent_node(),
              .default_parent_labs(),
              .default_parent_fill(palette_gradient(x), .get_mz_range(x)),
              set_command(theme_grey),
              .default_parent_theme()
    )
  }
default_vis_child_nebulae <-
  function(x){
    set <- layout_ggraph(child_nebulae(x))
    range <- .get_mz_range(x)
    hierarchy <- .get_hierarchy(x)
    ggset <- lapply(names(set),
                    function(name){
                      fill <- palette_label(x)[[ hierarchy[[name]] ]]
                      set_ggset(set_command(ggraph::ggraph, set[[ name ]]),
                                .default_parent_edge("black"),
                                .default_parent_node(),
                                .default_parent_labs(),
                                .default_parent_fill(palette_gradient(x), range),
                                .default_child_title(name),
                                set_command(theme_grey),
                                .default_child_theme(fill)
                      )
                    })
    names(ggset) <- names(set)
    return(ggset)
}
.get_mz_range <- function(x){
  range <- try(range(features_annotation(x)[[ "mz" ]]), silent = T)
  if (inherits(range, "try-error"))
    range <- c(100, 1000)
  return(range)
}
.get_hierarchy <- 
  function(x){
    hierarchy <- as.list(hierarchy(x)[["hierarchy"]])
    names(hierarchy) <- hierarchy(x)[["class.name"]]
    return(hierarchy)
  }
