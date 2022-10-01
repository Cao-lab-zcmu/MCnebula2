# ==========================================================================
# functions to get ggplot setting for visualizing nebulae
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @importFrom ggraph geom_edge_fan
#' @importFrom ggraph geom_node_point
#' @importFrom ggraph scale_edge_width
#' @importFrom stringr str_wrap
#' @importFrom stringr str_wrap
.default_parent_edge <- function(edge_color = "lightblue"){
  set_command(ggraph::geom_edge_fan,
              aes(edge_width = similarity),
              color = edge_color
  )
}
.default_parent_node <- function(){
  set_command(ggraph::geom_node_point,
              aes(size = ifelse(is.na(tani.score), 0.2, tani.score),
                  fill = mz),
              shape = 21)
}
.default_parent_fill <- function(pal){
  set_command(scale_fill_gradientn, colours = pal)
}
.default_parent_labs <- function(){
  set_command(labs, fill = "m/z", size = "Tanimoto similarity",
              edge_width = "Spectral similarity")
}
.default_parent_edge_width <- function(){
  set_command(scale_edge_width, range = c(0, 0.7))
}
.default_parent_theme <- function(){
  set_command(theme,
              text = element_text(family = "Times", face = "bold"),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank(),
              panel.grid = element_blank(),
              panel.background = element_rect(fill = "white"),
              legend.background = element_rect(fill = "transparent")
  )
}
.default_child_title <-
  function(title){
    set_command(ggtitle, stringr::str_wrap(title, width = 30))
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
    set_command(.element_textbox, fill = fill)
  }
