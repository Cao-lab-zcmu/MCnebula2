# ==========================================================================
# use features annotation and spectral similarity data to create network
# for child-nebula
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportMethod create_child_nebulae
#' @description \code{create_child_nebulae()}: get the default parameters for the method
#' \code{create_child_nebulae}.
#' @rdname create_child_nebulae-methods
setMethod("create_child_nebulae", 
          signature = setMissing("create_child_nebulae"),
          function(){
            list(edge_cutoff = 0.5,
                 max_edge_number = 5,
                 use_tracer = T)
          })
#' @exportMethod create_child_nebulae
#' @description \code{create_child_nebulae(x, ...)}: use the default parameters whatever 'missing'
#' while performing the method \code{create_child_nebulae}.
#' @rdname create_child_nebulae-methods
setMethod("create_child_nebulae", 
          signature = c(x = "mcnebula"),
          function(x, edge_cutoff, max_edge_number, use_tracer){
            reCallMethod("create_child_nebulae",
                         .fresh_param(create_child_nebulae()))
          })
#' @exportMethod create_child_nebulae
#'
#' @aliases create_child_nebulae
#'
#' @title ...
#'
#' @description ...
#'
#' @details ...
#'
#' @param x ...
#' @param edge_cutoff ...
#' @param max_edge_number ...
#'
# @inheritParams rdname
#'
#' @return ...
#'
# @seealso ...
#'
#' @rdname create_child_nebulae-methods
#'
#' @order 1
#'
#' @examples
#' \dontrun{
#' create_child_nebulae(...)
#' }
setMethod("create_child_nebulae", 
          signature = setMissing("create_child_nebulae",
                                 x = "mcnebula",
                                 edge_cutoff = "numeric",
                                 max_edge_number = "numeric",
                                 use_tracer = "logical"),
          function(x, edge_cutoff, max_edge_number, use_tracer){
            .message_info_formal("MCnebula2", "create_child_nebulae")
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
            if (use_tracer & is.logical(nebula_index(x)[[ "tracer" ]])) {
              classes <- unique(dplyr::filter(nebula_index(x), tracer)$class.name)
              nebula_index <- dplyr::filter(nebula_index(x), class.name %in% classes)
            } else {
              nebula_index <- nebula_index(x)
            }
            igraph(child_nebulae(x)) <-
              lapply(split(nebula_index, ~ class.name),
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


