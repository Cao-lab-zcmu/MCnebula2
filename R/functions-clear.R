# ==========================================================================
# Cleans up data in the mcnebula object that is no longer in use.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases clear
#'
#' @title Clean up data in the mcnebula object that is no longer in use
#' 
#' @param x [mcnebula-class] object.
#'
#' @name clear
NULL
#> NULL

#' @export clear_dataset
#' @aliases clear_dataset
#' @description \code{clear_dataset}: The data (chemical formula,
#' chemical structure, chemical classes)
#' in \code{project_dataset(x)}, and data in \code{backtrack(mcn_dataset(mcn)}
#' would be clean up to reduced memory usage.
#' This may be best used after running the [create_nebula_index()],
#' if your machine doesn't have much Random Access Memory (RAM).
#' @note If this function has conducted, the PPCP dataset would not be
#' available for downstream methods, such as [set_ppcp_data()],
#' [annotate_nebula()]...
#' @rdname clear
clear_dataset <- function(x) {
  clear <- c(".f2_formula", ".f3_fingerid", ".f3_canopus")
  for (i in clear) {
    dataset(project_dataset(x))[[ i ]] <- NULL
  }
  backtrack(mcn_dataset(x)) <- list()
  return(x)
}

#' @export clear_nodes
#' @aliases clear_nodes
#' @description \code{clear_nodes}: Clear data ('grobs' and 'ggset') of 'nodes'
#' in slot \code{child_nebulae}.
#' @rdname clear
clear_nodes <- function(x) {
  nodes_ggset(child_nebulae(x)) <- list()
  nodes_grob(child_nebulae(x)) <- list()
  return(x)
}

