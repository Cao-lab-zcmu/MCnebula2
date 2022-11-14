# ==========================================================================
# visualize the nebula and annotate it with multiple attributes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases annotate_nebula
#'
#' @title Add multiple annotation data for visualization of Child-Nebula.
#'
#' @description
#' Use methods [draw_nodes()] and [draw_structures()] to standby visualization
#' of Child-Nebula with mutiple annotation: chemical classification,
#' 'features' quantification, chemical structure...
#' Run after [activate_nebulae()].
#'
#' @details
#' Primarily, remove the [ggraph::geom_node_point()] layer in [ggset-class] object
#' of Child-Nebula. The 'nodes' would be replaced with 'grob' object create by
#' [draw_nodes()]. The function of [ggimage::geom_subview()] is used to add
#' 'grob' object into 'ggplot' object.
#'
#' @name annotate_nebula-methods
#'
#' @order 1
NULL
#> NULL

#' @importFrom dplyr select
#' @importFrom gridExtra arrangeGrob
#' @exportMethod annotate_nebula
#'
#' @aliases annotate_nebula
#'
#' @param x [mcnebula-class] object.
#' @param nebula_name character(1). Chemical classes in 'nebula_index' data.
#'
#' @seealso [activate_nebulae()], [draw_nodes()], [draw_structures()],
#' [set_ppcp_data()], [set_ration_data()]...
#'
#' @rdname annotate_nebula-methods
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
#'   test1 <- create_child_nebulae(test1, 0.01, 5)
#'   test1 <- create_child_layouts(test1)
#'   test1 <- activate_nebulae(test1)
#'   
#'   ## set features quantification data
#'   ids <- features_annotation(test1)$.features_id
#'   quant. <- data.frame(
#'     .features_id = ids,
#'     sample_1 = rnorm(length(ids), 1000, 200),
#'     sample_2 = rnorm(length(ids), 2000, 500)
#'   )
#'   metadata <- data.frame(
#'     sample = paste0("sample_", 1:2),
#'     group = c("control", "model")
#'   )
#'   features_quantification(test1) <- quant.
#'   sample_metadata(test1) <- metadata
#'   
#'   ## optional 'nebula_name'
#'   visualize(test1)
#'   ## a class for example
#'   class <- visualize(test1)$class.name[1]
#'   tmp <- export_path(test1)
#'   test1 <- annotate_nebula(test1, class)
#'   
#'   ## The following can be run before "annotate_nebula()"
#'   ## to customize the visualization of nodes.
#'   # test1 <- draw_structures(test1, "Fatty Acyls")
#'   ## set parameters for visualization of nodes
#'   # test1 <- draw_nodes(
#'   #   test1, "Fatty Acyls",
#'   #   add_id_text = T,
#'   #   add_structure = T,
#'   #   add_ration = T,
#'   #   add_ppcp = T
#'   # )
#'   # test1 <- annotate_nebula(test1, class)
#'   
#'   ## see results
#'   ggset <- ggset_annotate(child_nebulae(test1))
#'   ggset[[class]]
#'   ## visualize 'ggset'
#'   call_command(ggset[[class]])
#' }
setMethod("annotate_nebula", 
          signature = c(x = "ANY", nebula_name = "character"),
          function(x, nebula_name){
            .message_info_formal("MCnebula2", "annotate_nebula")
            data <- layout_ggraph(child_nebulae(x))[[nebula_name]]
            data <- dplyr::select(data, x, y,
                                  .features_id = name, size = tani.score)
            .features_id <- data$.features_id
            x <- draw_structures(x, nebula_name)
            x <- draw_nodes(x, nebula_name)
            nodes_grob <- nodes_grob(child_nebulae(x))
            nodes_grob <- lapply(.features_id,
                                 function(id) {
                                   gridExtra::arrangeGrob(nodes_grob[[id]])
                                 })
            ggset <- ggset(child_nebulae(x))[[ nebula_name ]]
            layers(ggset)[[ "ggraph::geom_node_point" ]] <- NULL
            ggset_annotate(child_nebulae(x))[[ nebula_name ]] <- 
              add_layers(modify_annotate_child(ggset),
                         .command_node_annotate(data, nodes_grob))
            return(x)
          })
