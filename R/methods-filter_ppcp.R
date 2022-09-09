# ==========================================================================
# collate ppcp dataset in sirius project and do filtering
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("filter_ppcp", 
          signature = setMissing("filter_ppcp",
                                 x = "mcnebula"),
          function(x, ...){
            filter_ppcp(x, .filter_ppcp_by_threashold, ..., by_reference = T)
          })
setMethod("filter_ppcp", 
          signature = setMissing("filter_ppcp",
                                 x = "mcnebula", fun_filter = "function"),
          function(x, fun_filter, ...){
            filter_ppcp(x, fun_filter, ..., by_reference = T)
          })
setMethod("filter_ppcp", 
          signature = setMissing("filter_ppcp",
                                 x = "mcnebula",
                                 by_reference = "logical"),
          function(x, ..., by_reference){
            filter_ppcp(x, .filter_ppcp_by_threashold, ..., by_reference)
          })
setMethod("filter_ppcp", 
          signature = setMissing("filter_ppcp",
                                 x = "mcnebula",
                                 by_reference = "logical"),
          function(x, by_reference){
            filter_ppcp(x, .filter_ppcp_by_threashold, by_reference = by_reference)
          })
setMethod("filter_ppcp", 
          signature = setMissing("filter_ppcp",
                                 x = "mcnebula", fun_filter = "function",
                                 by_reference = "logical"),
          function(x, fun_filter, ..., by_reference){
            .get_info_formal("MCnebula2", "filter_ppcp")
            if (by_reference) {
              .get_info("filter_ppcp", "by_reference == T")
              if (is.null(specific_candidate(x)))
                stop("is.null(specific_candidate(x)) == T. use `create_reference(x)` previously.")
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
              .get_info("filter_ppcp", "validate annotation data",
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
