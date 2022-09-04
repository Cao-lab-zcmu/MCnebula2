# ==========================================================================
# collate structure dataset in sirius project and do filtering
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("filter_structure", 
          signature = c(x = "mcnebula"),
          function(x){
            filter_structure(x, fun_filter = .rank_by_csi.score)
          })
setMethod("filter_structure", 
          signature = c(x = "mcnebula", fun_filter = "function"),
          function(x, fun_filter, ...){
            .get_info_formal("MCnebula2", "filter_structure")
            subscript <- ".f3_fingerid"
            x <- collate_data(x, subscript)
            ## filter
            msframe.lst <- extract_rawset(x, subscript)
            msframe.lst[[1]] <-
              filter_msframe(msframe.lst[[1]], fun_filter = fun_filter,
                             f = ~.features_id, ...)
            mcn_dataset(x) <- add_dataset(mcn_dataset(x), msframe.lst)
            return(x)
          })
