# ==========================================================================
# for creating parent layout, these layouts includes:
# layouts for nodes position of ggraph.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("create_parent_layout", 
          signature = setMissing("create_parent_layout",
                                 x = "mcnebula",
                                 ggraph_layout = "character"),
          function(x, ggraph_layout){
            create_parent_layout(x, ggraph_layout, seed = 1)
          })
setMethod("create_parent_layout", 
          signature = setMissing("create_parent_layout",
                                 x = "mcnebula"),
          function(x){
            create_parent_layout(x, ggraph_layout = "kk", seed = 1)
          })
setMethod("create_parent_layout", 
          signature = c(x = "mcnebula", ggraph_layout = "character",
                        seed = "numeric"),
          function(x, ggraph_layout, seed){
            .get_info_formal("MCnebula2", "create_parent_layout")
            .check_data(parent_nebula(x), list(igraph = "create_parent_nebula"))
            tbl_graph(parent_nebula(x)) <-
              tidygraph::as_tbl_graph(igraph(parent_nebula(x)))
            set.seed(seed)
            layout_ggraph(parent_nebula(x)) <-
              ggraph::create_layout(tbl_graph(parent_nebula(x)),
                                    layout = ggraph_layout)
            return(x)
          })
