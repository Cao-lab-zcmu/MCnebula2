# ==========================================================================
# collate structure dataset in sirius project and do filtering
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportMethod filter_structure
setMethod("filter_structure", 
          signature = setMissing("filter_structure",
                                 x = "mcnebula"),
          function(x){
            filter_structure(x, fun_filter = .rank_by_csi.score,
                             by_reference = F)
          })
setMethod("filter_structure", 
          signature = setMissing("filter_structure",
                                 x = "mcnebula",
                                 by_reference = "logical"),
          function(x, by_reference){
            filter_structure(x, fun_filter = .rank_by_csi.score,
                             by_reference = by_reference)
          })
setMethod("filter_structure", 
          signature = setMissing("filter_structure",
                                 x = "mcnebula",
                                 fun_filter = "function"),
          function(x, fun_filter, ...){
            filter_structure(x, fun_filter = fun_filter, ...,
                             by_reference = F)
          })
setMethod("filter_structure", 
          signature = setMissing("filter_structure",
                                 x = "mcnebula",
                                 fun_filter = "function",
                                 by_reference = "logical"),
          function(x, fun_filter, ..., by_reference){
            .get_info_formal("MCnebula2", "filter_structure")
            subscript <- ".f3_fingerid"
            x <- collate_data(x, subscript)
            ## filter
            msframe.lst <- extract_rawset(x, subscript)
            if (by_reference) {
              .get_info("filter_structure", "by_reference == T")
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
