# ==========================================================================
# algorithmic functions used in methods-*.R files
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

.rank_by_csi.score <- 
  function(df){
    head( dplyr::arrange(df, desc(csi.score)), n = 1)
  }

.rank_by_default <- 
  function(df){
    head(df, n = 1)
  }

.filter_ppcp_by_threshold <- 
  function(df, pp.threshold = 0.5){
    dplyr::filter(df, pp.value > pp.threshold)
  }

.decrease_edges <-
  function(edges, max_edge_number = 5){
    ## order
    edges <- edges[order(edges$similarity, decreasing = T), ]
    edges[[ "...SEQ" ]] <- 1:nrow(edges)
    freq <- table(c(edges[[ ".features_id1" ]], edges[[ ".features_id2" ]]))
    ## at least loop number
    while (max(freq) > max_edge_number) {
      target_id <- names(freq[freq == max(freq)])[1]
      ## get ...SEQ of the edges which need to be excluded
      include <- edges[[ ".features_id1" ]] == target_id |
        edges[[ ".features_id2" ]] == target_id
      edges_include_target <- edges[include, ]
      seq_exclude_edges <- edges_include_target[-(1:max_edge_number), ]$...SEQ
      ## exclude edges
      edges <- edges[!edges$...SEQ %in% seq_exclude_edges, ]
      freq <- table(c(edges[[ ".features_id1" ]], edges[[ ".features_id2" ]]))
    }
    edges[[ "...SEQ" ]] <- NULL
    edges
  }
