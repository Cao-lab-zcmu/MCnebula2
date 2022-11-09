# ==========================================================================
# collate formula dataset in sirius project and do filtering
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases filter_formula
#'
#' @title Collate and filter candidates of chemical formula for each 'feature'
#'
#' @description This methods provide an approach to
#' collate and filter chemical formula candidates data in baches for each
#' 'feature'.
#'
#' @details In SIRIUS project directory, if the computation job has done,
#' each 'feature' has multiple prediction candidates whether for chemical formula,
#' structure, or classification. This method provides an approach to collate
#' and filter these data in baches. See \link{MCnebula2} for details of chemical
#' formula, structure and classification.
#'
#' @name filter_formula-methods
#'
#' @order 1
NULL
#> NULL

#' @exportMethod filter_formula
#' @description \code{filter_formula()}: get the default parameters for the method
#' \code{filter_formula}.
#' @rdname filter_formula-methods
setMethod("filter_formula", 
          signature = setMissing("filter_formula",
                                 x = "missing"),
          function(){
            list(fun_filter = .rank_by_default,
                 by_reference = F
            )
          })

#' @exportMethod filter_formula
#' @description \code{filter_formula(x, ...)}: use the default parameters whatever 'missing'
#' while performing the method \code{filter_formula}.
#' @rdname filter_formula-methods
setMethod("filter_formula", 
          signature = c(x = "mcnebula"),
          function(x, fun_filter, ..., by_reference){
            reCallMethod("filter_formula",
                         .fresh_param(filter_formula()), ...)
          })

#' @exportMethod filter_formula
#'
#' @aliases filter_formula
#'
#' @param x [mcnebula-class] object.
#' 
#' @param fun_filter function. Used to filter data.frame. The function would
#' run for candidates data (data.frame) for each 'features'. Such as:
#' - \code{lapply(split(all_data, ~.features_id), fun_filter, ...)}.
#' 
#' This parameter provides an elegant and flexible way to filter data.
#' Users can pass function [dplyr::filter()] to specify
#' any attributes condition to filter the data.
#' 
#' @param ... Other parameters passed to the function \code{fun_filter}.
#' @param by_reference logical. Use \code{specific_candidate(object)} data to filter
#' candidates data. See [create_reference()].
#'
#' @rdname filter_formula-methods
#'
#' @examples
#' \dontrun{
#' filter_formula(...)
#' }
setMethod("filter_formula", 
          signature = setMissing("filter_formula",
                                 x = "mcnebula",
                                 fun_filter = "function",
                                 by_reference = "logical"),
          function(x, fun_filter, ..., by_reference){
            .message_info_formal("MCnebula2", "filter_formula")
            subscript <- ".f2_formula"
            x <- collate_data(x, subscript, .collate_formula.msframe)
            ## filter
            msframe.lst <- extract_rawset(x, subscript)
            if (by_reference) {
              .message_info("filter_formula", "by_reference == T",
                        "\n\tcase formula, ignore `fun_filter`")
              .check_data(x, list(specific_candidate = "create_reference"))
              fun <- methods_match(project_api(x))[[ "generate_candidates_id" ]]
              entity(msframe.lst[[1]]) <- 
                merge(specific_candidate(x),
                      format_msframe(entity(msframe.lst[[1]]), fun_format = fun),
                      by = c(".features_id", ".candidates_id"))
            } else {
              msframe.lst[[1]] <-
                filter_msframe(msframe.lst[[1]], fun_filter = fun_filter,
                               f = ~.features_id, ...)
            }
            mcn_dataset(x) <- add_dataset(mcn_dataset(x), msframe.lst)
            return(x)
          })

.collate_formula.msframe <- 
  function(x, subscript){
    msframe <- .collate_data.msframe(x, subscript)
    if (!"zodiac.score" %in% colnames(entity(msframe))) {
      warning("`zodiac.score` not found in `msframe`, fill it with `zodiac.score` = 0")
      entity(msframe)$zodiac.score <- 0
    }
    msframe
  }
