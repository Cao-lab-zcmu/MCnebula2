# ==========================================================================
# collate formula dataset in sirius project and do filtering
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("filter_formula", 
          signature = c(x = "mcnebula"),
          function(x){
            filter_formula(x, fun_filter = .rank_by_default)
          })
setMethod("filter_formula", 
          signature = c(x = "mcnebula", fun_filter = "function"),
          function(x, fun_filter, ...){
            .get_info_formal("MCnebula2", "filter_formula")
            subscript <- ".f2_formula"
            x <- collate_data(x, subscript, .collate_formula.msframe)
            ## filter
            msframe.lst <- extract_rawset(x, subscript)
            msframe.lst[[1]] <-
              filter_msframe(msframe.lst[[1]], fun_filter = fun_filter,
                             f = ~.features_id, ...)
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
