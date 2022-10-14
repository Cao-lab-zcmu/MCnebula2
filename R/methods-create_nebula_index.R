# ==========================================================================
# create nebula index from filtered stardust classes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("create_nebula_index", 
          signature = setMissing("create_nebula_index",
                                 x = "mcnebula"),
          function(x){
            create_nebula_index(x, force = F)
          })
setMethod("create_nebula_index", 
          signature = c(x = "mcnebula", force = "logical"),
          function(x, force){
            .message_info_formal("MCnebula2", "create_nebula_index")
            .check_data(x, list(stardust_classes = "create_stardust_classes"))
            if (!force) {
              class_num <- length(unique(stardust_classes(x)[[ "rel.index" ]]))
              if (class_num > 120)
                stop("too many classes; length(unique(stardust_classes(x)$rel.index)) > 120")
            }
            reference(mcn_dataset(x))[[ "nebula_index" ]] <- 
              dplyr::select(stardust_classes(x), rel.index, class.name,
                            hierarchy, .features_id)
            return(x)
          })
