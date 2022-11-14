# ==========================================================================
# use features annotation and spectral similarity data to create network
# for child-nebula
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases create_child_nebulae
#'
#' @title Gather data to create Child-Nebulae
#'
#' @description
#' Similar to [create_parent_nebula()], gather 'spectral_similarity' data and
#' and 'features_annotation' data; but additionally, use 'nebula_index' data
#' to group 'features' by chemical classes. Each chemical classes in 'nebula_index'
#' data would lead to a 'igraph' object.
#' 
#' @seealso [compute_spectral_similarity()], [create_features_annotation()],
#' [create_nebula_index()],
#' [igraph::graph_from_data_frame()].
#'
#' @name create_child_nebulae-methods
#'
#' @order 1
NULL
#> NULL

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
#' @description \code{create_child_nebulae(x, ...)}:
#' use the default parameters whatever 'missing'
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
#' @param x [mcnebula-class] object.
#' @param edge_cutoff numeric(1). Value in (0,1). Set a threshold to
#' create edges upon similarity value of 'spectral_similarity' data.
#'
#' @param max_edge_number numeric(1).
#' For nodes (features) in each Child-Nebulae (i.e. network), the maximum number of
#' edges link with. If the number exceeds the limitation, only edges representing higher
#' spectral similarity would be retained.
#'
#' @rdname create_child_nebulae-methods
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
#'   create_child_nebulae()
#'   
#'   test1 <- create_child_nebulae(test1, 0.01, 5)
#'   ## see results
#'   igraph(child_nebulae(test1))
#'   ## write output for 'Cytoscape' or other network software
#'   tmp <- paste0(tempdir(), "/child_nebulae/")
#'   dir.create(tmp)
#'   res <- igraph(child_nebulae(test1))
#'   lapply(
#'     names(res),
#'     function(name) {
#'       igraph::write_graph(
#'         res[[name]],
#'         file = paste0(tmp, name, ".graphml"),
#'         format = "graphml"
#'       )
#'     }
#'   )
#'   list.files(tmp)
#'   
#'   unlink(tmp, T, T)
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


