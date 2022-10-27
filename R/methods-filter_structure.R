# ==========================================================================
# collate structure dataset in sirius project and do filtering
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportMethod filter_structure
#' @description \code{filter_structure()}: get the default parameters for the method
#' \code{filter_structure}.
#' @rdname filter_structure-methods
setMethod("filter_structure", 
          signature = setMissing("filter_structure",
                                 x = "missing"),
          function(){
            list(fun_filter = .rank_by_csi.score,
                 by_reference = F
            )
          })
#' @exportMethod filter_structure
#' @description \code{filter_structure(x, ...)}: use the default parameters whatever 'missing'
#' while performing the method \code{filter_structure}.
#' @rdname filter_structure-methods
setMethod("filter_structure", 
          signature = c(x = "mcnebula"),
          function(x, fun_filter, ..., by_reference){
            reCallMethod("filter_structure",
                         .fresh_param(filter_structure()), ...)
          })
#' @exportMethod filter_structure
#'
#' @aliases filter_structure
#'
#' @title ...
#'
#' @description ...
#'
#' @details ...
#'
#' @param x [mcnebula-class] object.
#' @param fun_filter function. e.g., [dplyr::filter()], [dplyr::slice()] [head()]...
#' @param ... parameters pass to 'fun_filter'.
#' @param by_reference ...
#'
# @inheritParams rdname
#'
#' @return ...
#'
#' @seealso [fun()]
#'
#' @rdname filter_structure-methods
#'
#' @examples
#' \dontrun{
#' filter_structure(...)
#' }
setMethod("filter_structure", 
          signature = setMissing("filter_structure",
                                 x = "mcnebula",
                                 fun_filter = "function",
                                 by_reference = "logical"),
          function(x, fun_filter, ..., by_reference){
            .message_info_formal("MCnebula2", "filter_structure")
            subscript <- ".f3_fingerid"
            x <- collate_data(x, subscript)
            ## filter
            msframe.lst <- extract_rawset(x, subscript)
            if (by_reference) {
              .message_info("filter_structure", "by_reference == T")
              .check_data(x, list(specific_candidate = "create_reference"))
              entity(msframe.lst[[1]]) <- 
                merge(specific_candidate(x), entity(msframe.lst[[1]]),
                      by = c(".features_id", ".candidates_id"))
            }
            msframe.lst[[1]] <-
              filter_msframe(msframe.lst[[1]], fun_filter = fun_filter,
                             f = ~.features_id, ...)
            mcn_dataset(x) <- add_dataset(mcn_dataset(x), msframe.lst)
            return(x)
          })
