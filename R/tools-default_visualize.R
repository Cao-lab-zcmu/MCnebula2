# ==========================================================================
# functions to get 'command' of ggplot, grob for visualizing nebulae
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @importFrom ggraph geom_edge_fan
#' @importFrom ggraph geom_node_point
#' @importFrom ggraph scale_edge_width
#' @importFrom stringr str_wrap
#' @importFrom stringr str_wrap
.command_parent_edge <- function(edge_color = "lightblue"){
  new_command(ggraph::geom_edge_fan,
              aes(edge_width = similarity),
              color = edge_color
  )
}

.command_parent_node <- function(){
  new_command(ggraph::geom_node_point,
              aes(size = ifelse(is.na(tani.score), 0.2, tani.score),
                  fill = mz),
              shape = 21)
}

.command_parent_fill <- function(pal){
  new_command(scale_fill_gradientn, colours = pal, na.value = "white")
}

.command_parent_fill2 <- function(pal){
  new_command(scale_fill_manual, values = pal)
}

.command_parent_labs <- function(){
  new_command(labs, fill = "m/z", size = "Tanimoto similarity",
              edge_width = "Spectral similarity")
}

.command_parent_edge_width <- function(){
  new_command(scale_edge_width, range = c(0, 0.7))
}

.command_parent_theme <- function(){
  new_command(match.fun(theme),
              text = element_text(family = .font, face = "bold"),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank(),
              panel.grid = element_blank(),
              panel.background = element_rect(fill = "white"),
              legend.background = element_rect(fill = "transparent"),
              name = "theme"
  )
}

.command_child_title <-
  function(title){
    new_command(ggtitle, stringr::str_wrap(title, width = 30))
  }

.command_child_theme <- 
  function(fill){
    command <- .command_parent_theme()
    command_args(command)[[ "plot.title" ]] <-
      call_command(.command_title_textbox(fill))
    command
  }

.command_title_textbox <- 
  function(fill){
    new_command(.element_textbox, fill = fill)
  }

.command_node_nuclear <- 
  function(color){
    new_command(geom_ribbon, fill = color,
                aes(ymin = -5L, ymax = 0L,
                    x = seq(0, max(seq) + 1, length.out = length(seq)))
    )
  }

.command_node_border <- 
  function(){
    new_command(geom_ribbon, fill = "black",
                aes(ymin = 0, ymax = 1.1,
                    x = seq(0, max(seq) + 1, length.out = length(seq)))
    )
  }

.command_node_radial_bar <- 
  function(){
    new_command(geom_col, aes(x = seq, y = pp.value,
                              fill = reorder(paste0(rel.index), rel.index)),
                color = "white", size = 0.25)
  }

.command_node_fill <- 
  function(pal, labels){
    new_command(scale_fill_manual, values = pal, labels = labels)
  }

.command_node_ylim <- 
  function(){
    new_command(ylim, ... = c(-5, 1.3))
  }

.command_node_polar <- 
  function(){
    new_command(coord_polar)
  }

.command_node_theme <- 
  function(){
    new_command(match.fun(theme),
                text = element_text(family = .font, face = "bold"),
                name = "theme")
  }

.command_node_ration <- 
  function(df){
    new_command(geom_tile, data = df, size = 0.2, color = "white",
                aes(y = -2.5, x = x, width = width,
                    height = 2.5, fill = group))
  }

#' @importFrom ggimage geom_subview
.command_node_annotate <- 
  function(data, subview){
    new_command(ggimage::geom_subview, data = data, 
                aes(x = x, y = y, width = size, height = size),
                subview = subview)
  }

## ---------------------------------------------------------------------- 
.grob_legend_hierarchy_plot <- 
  function(color, theme){
    df <- data.frame(h = names(color), color = color, y = 1:length(color))
    p <- ggplot(df) +
      geom_tile(aes(x = 1, y = h, fill = h)) +
      labs(fill = "Class hierarchy") +
      scale_fill_manual(values = color) +
      guides(fill = guide_legend(nrow = 1, direction = "horizontal")) +
      theme
    .get_legend(p)
  }

.grob_node_text <- 
  function(label, color = "black"){
    grid::textGrob(label, y = 0.12,
                   gp = grid::gpar(fontfamily = .font,
                                   fontsize = 20, col = color))
  }
