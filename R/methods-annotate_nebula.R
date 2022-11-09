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
#' annotate_nebula(...)
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
