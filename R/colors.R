# ==========================================================================
# Get hexadecimal color with ggsci package
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.get_color_set <- 
  function(){
    unique(c(ggsci::pal_simpsons()(16),
             ggsci::pal_igv("default")(51),
             ggsci::pal_ucscgb()(26),
             ggsci::pal_d3("category20")(20)
             ))
  }
.get_label_color <- 
  function(){
    colorRampPalette(c("#C6DBEFFF", "#3182BDFF", "red"))(10)
  }
