# ==========================================================================
# collate ppcp dataset in sirius project and do filtering
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases filter_ppcp
#'
#' @title Collate and filter candidates of chemical classification for each 'feature'
#'
#' @description This methods provide an approach to
#' collate and filter chemical classification candidates data in baches for each
#' 'feature'.
#'
#' @details 
#' Filter for PPCP (posterior probability of classification prediction) data.
#' See details about classification prediction for compounds:
#' \url{http://www.nature.com/articles/s41587-020-0740-8}.
#' See other details in [filter_formula()].
#'
#' @name filter_ppcp-methods
#'
#' @order 1
NULL
#> NULL

#' @exportMethod filter_ppcp
#' @description \code{filter_ppcp()}: get the default parameters for the method
#' \code{filter_ppcp}.
#' @rdname filter_ppcp-methods
setMethod("filter_ppcp", 
          signature = setMissing("filter_ppcp",
                                 x = "missing"),
          function(){
            list(fun_filter = .filter_ppcp_by_threshold,
                 by_reference = T
            )
          })
#' @exportMethod filter_ppcp
#' @description \code{filter_ppcp(x, ...)}: use the default parameters whatever 'missing'
#' while performing the method \code{filter_ppcp}.
#' @rdname filter_ppcp-methods
setMethod("filter_ppcp", 
          signature = c(x = "mcnebula"),
          function(x, fun_filter, ..., by_reference){
            reCallMethod("filter_ppcp",
                         .fresh_param(filter_ppcp()), ...)
          })
#' @exportMethod filter_ppcp
#'
#' @aliases filter_ppcp
#'
#' @inheritParams filter_formula-methods
#'
#' @rdname filter_ppcp-methods
#'
#' @examples
#' \dontrun{
#' filter_ppcp(...)
#' }
setMethod("filter_ppcp", 
          signature = setMissing("filter_ppcp",
                                 x = "mcnebula", fun_filter = "function",
                                 by_reference = "logical"),
          function(x, fun_filter, ..., by_reference){
            .message_info_formal("MCnebula2", "filter_ppcp")
            if (by_reference) {
              .message_info("filter_ppcp", "by_reference == T")
              .check_data(x, list(specific_candidate = "create_reference"))
            }
            subscript <- c(".canopus", ".f3_canopus")
            if (ion_mode(x) == "neg")
              subscript[1] <- c(".canopus_neg")
            for (i in subscript) {
              x <- get_metadata(x, i)
              if (by_reference & i == subscript[2])
                x <- collate_data(x, i, reference = specific_candidate(x))
              else
                x <- collate_data(x, i)
            }
            annotation <- entity(dataset(project_dataset(x))[[ subscript[1] ]])
            msframe.lst <- extract_rawset(x, subscript = subscript[2])
            ## validate
            if ( !subscript[2] %in% names(dataset(mcn_dataset(x))) ) {
              .message_info("filter_ppcp", "validate annotation data",
                        paste0(subscript, collapse = " >>> "))
              validate_ppcp_annotation(annotation, msframe.lst)
              ## add annotation into dataset
              msframe.lst <- merge_ppcp_annotation(annotation, msframe.lst)
              project_dataset(x) <- add_dataset(project_dataset(x), msframe.lst)
            }
            ## filter
            msframe.lst[[1]] <-
              filter_msframe(msframe.lst[[1]], fun_filter = fun_filter,
                             f = ~ paste0(.features_id, "_", .candidates_id), ...)
            mcn_dataset(x) <- add_dataset(mcn_dataset(x), msframe.lst)
            return(x)
          })
validate_ppcp_annotation <- 
  function(annotation, lst){
    rows <- nrow(annotation)
    lst <- split(entity(lst[[1]]), f = ~ paste0(.features_id, "_", .candidates_id))
    if (!identical( annotation$rel.index, lst[[1]]$rel.index))
      stop("the annotation not match the classification dataset: 1")
    lapply(lst, function(df){
             if (nrow(df) != rows)
               stop("the annotation not match the classification dataset")
          })
  }
merge_ppcp_annotation <- 
  function(annotation, msframe.lst){
    annotation <- dplyr::select(annotation, -.features_id, -.candidates_id)
    entity(msframe.lst[[1]]) <- 
      merge(entity(msframe.lst[[1]]), annotation,
            by = "rel.index", all.x = T, sort = F)
    return(msframe.lst)
  }
