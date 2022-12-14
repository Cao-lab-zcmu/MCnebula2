# ==========================================================================
# use features annotation and spectral similarity data to create network
# for parent-nebula
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases create_parent_nebula
#'
#' @title Gather data to create Parent-Nebula
#'
#' @description
#' Gather 'spectral_similarity' data and 'features_annotation' data
#' to create 'igraph' object use function of [igraph::graph_from_data_frame()].
#'
#' @name create_parent_nebula-methods
#'
#' @seealso [compute_spectral_similarity()], [create_features_annotation()],
#' [igraph::graph_from_data_frame()].
#' 
#' @order 1
NULL
#> NULL

#' @importFrom igraph graph_from_data_frame
#' @exportMethod create_parent_nebula
#' @description \code{create_parent_nebula()}: get the default parameters for the method
#' \code{create_parent_nebula}.
#' @rdname create_parent_nebula-methods
setMethod("create_parent_nebula", 
          signature = setMissing("create_parent_nebula"),
          function(){
            list(edge_cutoff = 0.5,
                 max_edge_number = 5,
                 remove_isolate = T)
          })

#' @exportMethod create_parent_nebula
#' @description \code{create_parent_nebula(x, ...)}:
#' use the default parameters whatever 'missing'
#' while performing the method \code{create_parent_nebula}.
#' @rdname create_parent_nebula-methods
setMethod("create_parent_nebula", 
          signature = c(x = "mcnebula"),
          function(x, edge_cutoff, max_edge_number, remove_isolate){
            reCallMethod("create_parent_nebula",
                         .fresh_param(create_parent_nebula()))
          })

#' @exportMethod create_parent_nebula
#'
#' @aliases create_parent_nebula
#'
#' @param x [mcnebula-class] object.
#' @param edge_cutoff numeric(1). Value in (0,1). Set a threshold to
#' create edges upon similarity value of 'spectral_similarity' data.
#' 
#' @param max_edge_number numeric(1).
#' For nodes (features) in each Parent-Nebulae (i.e. network), the maximum number of
#' edges link with. If the number exceeds the limitation, only edges representing higher
#' spectral similarity would be retained.
#' 
#' @param remove_isolate logical. If \code{TRUE}, remove the isolate 'features'
#' (in network, i.e. the nodes without edge)
#'
#' @rdname create_parent_nebula-methods
#'
#' @examples
#' \dontrun{
#'   test <- mcn_5features
#'   
#'   ## the previous steps
#'   test1 <- filter_structure(test)
#'   test1 <- create_reference(test1)
#'   test1 <- filter_formula(test1, by_reference = T)
#'   test1 <- create_stardust_classes(test1)
#'   test1 <- create_features_annotation(test1)
#'   test1 <- cross_filter_stardust(test1, 2, 1)
#'   test1 <- create_nebula_index(test1)
#'   test1 <- compute_spectral_similarity(test1)
#'   
#'   ## default parameters
#'   create_parent_nebula()
#'   
#'   test1 <- create_parent_nebula(test1, 0.01)
#'   ## see results
#'   igraph(parent_nebula(test1))
#'   ## write output for 'Cytoscape' or other network software
#'   tmp <- tempdir()
#'   igraph::write_graph(
#'     igraph(parent_nebula(test1)),
#'     file = paste0(tmp, "/parent_nebula.graphml",
#'       format = "graphml"
#'     )
#'   )
#'   
#'   unlink(tmp, T, T)
#' }
setMethod("create_parent_nebula", 
          signature = setMissing("create_parent_nebula",
                                 x = "mcnebula",
                                 edge_cutoff = "numeric",
                                 max_edge_number = "numeric",
                                 remove_isolate = "logical"),
          function(x, edge_cutoff, max_edge_number, remove_isolate){
            .message_info_formal("MCnebula2", "create_parent_nebula")
            .check_data(x, list(features_annotation = "create_features_annotation",
                                spectral_similarity = "compute_spectral_similarity"
                                ))
            edges <- dplyr::filter(spectral_similarity(x),
                                   similarity >= edge_cutoff)
            if (nrow(edges) > max_edge_number) {
              edges <- .decrease_edges(edges, max_edge_number)
            }
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
