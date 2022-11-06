# ==========================================================================
# use features annotation and spectral similarity data to create network
# for parent-nebula
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases create_parent_nebula
#'
#' @title ...
#'
#' @description ...
#'
#' @details ...
#'
#' @name create_parent_nebula-methods
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
          signature = setMissing("create_parent_nebula",
                                 x = "mcnebula"),
          function(x){
            create_parent_nebula(x, edge_cutoff = 0.5,
                                 remove_isolate = T)
          })
#' @exportMethod create_parent_nebula
#' @description \code{create_parent_nebula(x, ...)}: use the default parameters whatever 'missing'
#' while performing the method \code{create_parent_nebula}.
#' @rdname create_parent_nebula-methods
setMethod("create_parent_nebula", 
          signature = setMissing("create_parent_nebula",
                                 x = "mcnebula",
                                 edge_cutoff = "numeric"),
          function(x, edge_cutoff){
            create_parent_nebula(x, edge_cutoff = edge_cutoff,
                                 remove_isolate = T)
          })
#' @exportMethod create_parent_nebula
#'
#' @aliases create_parent_nebula
#'
#' @title ...
#'
#' @description ...
#'
#' @details ...
#'
#' @param x ...
#' @param edge_cutoff ...
#' @param remove_isolate ...
#'
# @inheritParams rdname
#'
#' @return ...
#'
# @seealso ...
#'
#' @rdname create_parent_nebula-methods
#'
#' @examples
#' \dontrun{
#' create_parent_nebula(...)
#' }
setMethod("create_parent_nebula", 
          signature = setMissing("create_parent_nebula",
                                 x = "mcnebula",
                                 edge_cutoff = "numeric",
                                 remove_isolate = "logical"),
          function(x, edge_cutoff, remove_isolate){
            .message_info_formal("MCnebula2", "create_parent_nebula")
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
