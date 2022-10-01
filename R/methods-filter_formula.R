# ==========================================================================
# collate formula dataset in sirius project and do filtering
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportMethod filter_formula
setMethod("filter_formula", 
          signature = setMissing("filter_formula",
                                 x = "mcnebula", by_reference = "logical"),
          function(x, by_reference){
            filter_formula(x, fun_filter = .rank_by_default,
                           by_reference = by_reference)
          })
setMethod("filter_formula", 
          signature = setMissing("filter_formula",
                                 x = "mcnebula"),
          function(x){
            filter_formula(x, fun_filter = .rank_by_default, by_reference = F)
          })
setMethod("filter_formula", 
          signature = setMissing("filter_formula",
                                 x = "mcnebula",
                                 fun_filter = "function"),
          function(x, fun_filter, ...){
            filter_formula(x, fun_filter = fun_filter, ..., by_reference = F)
          })
setMethod("filter_formula", 
          signature = setMissing("filter_formula",
                                 x = "mcnebula",
                                 fun_filter = "function",
                                 by_reference = "logical"),
          function(x, fun_filter, ..., by_reference){
            .print_info_formal("MCnebula2", "filter_formula")
            subscript <- ".f2_formula"
            x <- collate_data(x, subscript, .collate_formula.msframe)
            ## filter
            msframe.lst <- extract_rawset(x, subscript)
            if (by_reference) {
              .print_info("filter_formula", "by_reference == T",
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
