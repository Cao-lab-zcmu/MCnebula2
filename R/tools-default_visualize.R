# ==========================================================================
# functions to get ggplot setting for visualizing nebulae
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @importFrom ggraph geom_edge_fan
#' @importFrom ggraph geom_node_point
#' @importFrom ggraph scale_edge_width
#' @importFrom stringr str_wrap
#' @importFrom stringr str_wrap
.default_parent_edge <- function(edge_color = "lightblue"){
  new_command(ggraph::geom_edge_fan,
              aes(edge_width = similarity),
              color = edge_color
  )
}
.default_parent_node <- function(){
  new_command(ggraph::geom_node_point,
              aes(size = ifelse(is.na(tani.score), 0.2, tani.score),
                  fill = mz),
              shape = 21)
}
.default_parent_fill <- function(pal){
  new_command(scale_fill_gradientn, colours = pal)
}
.default_parent_labs <- function(){
  new_command(labs, fill = "m/z", size = "Tanimoto similarity",
              edge_width = "Spectral similarity")
}
.default_parent_edge_width <- function(){
  new_command(scale_edge_width, range = c(0, 0.7))
}
.default_parent_theme <- function(){
  new_command(match.fun(theme),
              text = element_text(family = "Times", face = "bold"),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank(),
              panel.grid = element_blank(),
              panel.background = element_rect(fill = "white"),
              legend.background = element_rect(fill = "transparent"),
              name = "theme"
  )
}
.default_child_title <-
  function(title){
    new_command(ggtitle, stringr::str_wrap(title, width = 30))
  }
.default_child_theme <- 
  function(fill){
    command <- .default_parent_theme()
    command_args(command)[[ "plot.title" ]] <-
      call_command(.default_title_textbox(fill))
    command
  }
.default_title_textbox <- 
  function(fill){
    new_command(.element_textbox, fill = fill)
  }
.default_node_nuclear <- 
  function(color){
    new_command(geom_ribbon, fill = color,
                aes(ymin = -5L, ymax = 0L,
                    x = seq(0, max(seq) + 1, length.out = length(seq)))
    )
  }
.default_node_border <- 
  function(){
    new_command(geom_ribbon, fill = "black",
                aes(ymin = 0, ymax = 1.1,
                    x = seq(0, max(seq) + 1, length.out = length(seq)))
    )
  }
.default_nodes_radial_bar <- 
  function(){
    new_command(geom_col, aes(x = seq, y = pp.value,
                              fill = reorder(paste0(rel.index), rel.index)),
                color = "white", size = 0.25)
  }
.default_node_fill <- 
  function(pal, labels){
    new_command(scale_fill_manual, values = pal, labels = labels)
  }
.default_node_ylim <- 
  function(){
    new_command(ylim, ... = c(-5, 1.3))
  }
.default_node_polar <- 
  function(){
    new_command(coord_polar)
  }
.default_node_theme <- 
  function(){
    new_command(match.fun(theme),
                text = element_text(family = "Times", face = "bold"),
                name = "theme")
  }
.default_node_statistic <- 
  function(df){
    new_command(geom_tile, data = df, size = 0.2, color = "white",
                aes(y = -2.5, x = x, width = width,
                    height = 2.5, fill = group))
  }
