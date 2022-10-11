# ==========================================================================
# Get hexadecimal color with ggsci package
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @importFrom ggsci pal_simpsons
#' @importFrom ggsci pal_igv
#' @importFrom ggsci pal_ucscgb
#' @importFrom ggsci pal_d3
.get_color_set <- 
  function(){
    unique(c(ggsci::pal_simpsons()(16),
             ggsci::pal_igv("default")(51),
             ggsci::pal_ucscgb()(26),
             ggsci::pal_d3("category20")(20)
             ))
  }
.get_color_gradient <- 
  function(){
    c("#D5E4A2FF", "#FFCD00FF", "#EEA236FF", "#FB6467FF", "#9467BDFF")
  }
.get_label_color <- 
  function(){
    colorRampPalette(c("#C6DBEFFF", "#3182BDFF", "red"))(10)
  }
#' @importFrom ggsci pal_locuszoom
.get_color_stat <- 
  function(){
    col <- ggsci::pal_locuszoom()(7)
    vapply(col, .depigment_col, "ch", USE.NAMES = F)
  }
