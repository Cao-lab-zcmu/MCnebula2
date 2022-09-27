# ==========================================================================
# use features annotation and spectral similarity data to create network
# for child-nebula
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("create_child_nebulae", 
          signature = setMissing("create_child_nebulae",
                                 x = "mcnebula"),
          function(x){
            create_child_nebulae(x, 0.5, 5)
          })
setMethod("create_child_nebulae", 
          signature = setMissing("create_child_nebulae",
                                 x = "mcnebula",
                                 edge_cutoff = "numeric"),
          function(x, edge_cutoff){
            create_child_nebulae(x, edge_cutoff, 5)
          })
setMethod("create_child_nebulae", 
          signature = setMissing("create_child_nebulae",
                                 x = "mcnebula",
                                 max_edge_number = "numeric"),
          function(x, max_edge_number){
            create_child_nebulae(x, 0.5, max_edge_number)
          })
setMethod("create_child_nebulae", 
          signature = setMissing("create_child_nebulae",
                                 x = "mcnebula",
                                 edge_cutoff = "numeric",
                                 max_edge_number = "numeric"),
          function(x, edge_cutoff, max_edge_number){
            .get_info_formal("MCnebula2", "create_child_nebulae")
            .check_data(x, list(features_annotation = "create_features_annotation",
                                spectral_similarity = "compute_spectral_similarity",
                                nebula_index = "create_nebula_index"
                                ))
            if (max_edge_number < 1) {
              stop( "`max_edge_number` must be a numeric greater or equal to 1" )
            }
            features <- features_annotation(x)
            edges <- dplyr::filter(spectral_similarity(x),
                                   similarity >= edge_cutoff)
            igraph(child_nebulae(x)) <-
              lapply(split(nebula_index(x), ~ class.name),
                     function(meta) {
                       features <- dplyr::filter(features, .features_id %in%
                                                 meta$.features_id)
                       edges <- dplyr::filter(edges, .features_id1 %in% meta$.features_id &
                                              .features_id2 %in% meta$.features_id)
                       if (nrow(edges) > max_edge_number) {
                         edges <- .decrease_edges(edges, max_edge_number)
                       }
                       igraph::graph_from_data_frame(edges, directed = T,
                                                     vertices = features)
                     })
            return(x)
          })


