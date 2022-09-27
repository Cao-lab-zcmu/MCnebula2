# ==========================================================================
# use features annotation and spectral similarity data to create network
# for parent-nebula
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @importFrom igraph graph_from_data_frame
#' @exportMethod create_parent_nebula
setMethod("create_parent_nebula", 
          signature = setMissing("create_parent_nebula",
                                 x = "mcnebula"),
          function(x){
            create_parent_nebula(x, edge_cutoff = 0.5,
                                 remove_isolate = T)
          })
setMethod("create_parent_nebula", 
          signature = setMissing("create_parent_nebula",
                                 x = "mcnebula",
                                 edge_cutoff = "numeric"),
          function(x, edge_cutoff){
            create_parent_nebula(x, edge_cutoff = edge_cutoff,
                                 remove_isolate = T)
          })
setMethod("create_parent_nebula", 
          signature = setMissing("create_parent_nebula",
                                 x = "mcnebula",
                                 edge_cutoff = "numeric",
                                 remove_isolate = "logical"),
          function(x, edge_cutoff, remove_isolate){
            .get_info_formal("MCnebula2", "create_parent_nebula")
            .check_data(x, list(features_annotation = "create_features_annotation",
                                spectral_similarity = "compute_spectral_similarity"
                                ))
            edges <- dplyr::filter(spectral_similarity(x),
                                   similarity >= edge_cutoff)
            if (remove_isolate) {
              features <-
                dplyr::filter(features_annotation(x), .features_id %in%
                              unique(c(edges[[ ".features_id1" ]],
                                       edges[[ ".features_id2" ]]))
                )
            } else {
              features <- features_annotation(x)
            }
            igraph(parent_nebula(x)) <-
              igraph::graph_from_data_frame(edges, directed = T,
                                            vertices = features)
            return(x)
          })
