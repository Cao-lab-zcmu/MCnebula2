# ==========================================================================
# functions to get ggplot setting for visualizing nebulae
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @importFrom ggraph geom_edge_fan
#' @importFrom ggraph geom_node_point
#' @importFrom ggraph scale_edge_width
#' @importFrom stringr str_wrap
#' @importFrom ggtext element_textbox
#' @importFrom stringr str_wrap
.default_parent_edge <- function(edge_color = "lightblue"){
  set_command(ggraph::geom_edge_fan,
              aes(edge_width = similarity),
              color = edge_color, show.legend = F
  )
}
.default_parent_node <- function(){
  set_command(ggraph::geom_node_point,
              aes(size = ifelse(is.na(tani.score), 0.2, tani.score),
                  fill = mz),
              shape = 21)
}
.default_parent_fill <- function(pal, range = c(100, 1000)){
  set_command(scale_fill_gradientn, colours = pal, limits = range)
}
.default_parent_labs <- function(){
  set_command(labs, fill = "m/z", size = "Tanimoto similarity")
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
    ggset <- set_ggset(.default_parent_theme())
    ggset <- mutate_layer(ggset, 1, plot.title = .get_title_textbox(fill))
    layers(ggset)[[1]]
  }
.get_title_textbox <- function(fill){
  ggtext::element_textbox(color = "white", fill = fill,
                          box.color = "white",
                          halign = 0.5, linetype = 1,
                          r = unit(5, "pt"), width = unit(1, "npc"),
                          padding = margin(2, 0, 1, 0),
                          margin = margin(3, 3, 3, 3))
}
