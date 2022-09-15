# ==========================================================================
# use features annotation and spectral similarity data to create network
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("create_parent_nebula", 
          signature = setMissing("create_parent_nebula",
                                 x = "mcnebula"),
          function(x){
            create_parent_nebula(x, edge.threashould = 0.5,
                                 remove_isolate = T)
          })
setMethod("create_parent_nebula", 
          signature = setMissing("create_parent_nebula",
                                 x = "mcnebula",
                                 edge.threashould = "numeric"),
          function(x, edge.threashould){
            create_parent_nebula(x, edge.threashould = edge.threashould,
                                 remove_isolate = T)
          })
setMethod("create_parent_nebula", 
          signature = setMissing("create_parent_nebula",
                                 x = "mcnebula",
                                 edge.threashould = "numeric",
                                 remove_isolate = "logical"),
          function(x, edge.threashould, remove_isolate){
            .get_info_formal("MCnebula2", "create_parent_nebula")
            .check_data(x, list(features_annotation = "create_features_annotation",
                                spectral_similarity = "compute_spectral_similarity"
                                ))
            edge <- dplyr::filter(spectral_similarity(x),
                                  similarity >= edge.threashould)
            if (remove_isolate) {
              features <-
                dplyr::filter(features_annotation(x), .features_id %in%
                              unique(c(edge[[ ".features_id1" ]],
                                       edge[[ ".features_id2" ]]))
                )
            } else {
              features <- features_annotation(x)
            }
            parent_nebula <-
              igraph::graph_from_data_frame(edge, directed = T,
                                            vertices = features)
            parent_nebula(x) <- list(parent_nebula)
            return(x)
          })
