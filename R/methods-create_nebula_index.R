# ==========================================================================
# create nebula index from filtered stardust classes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("create_nebula_index", 
          signature = setMissing("create_nebula_index",
                                 x = "mcnebula"),
          function(x){
            .get_info_formal("MCnebula2", "create_nebula_index")
            .check_data(x, list(stardust_classes = "create_stardust_classes"))
            reference(mcn_dataset(x))[[ "nebula_index" ]] <- 
              dplyr::select(stardust_classes(x), rel.index, class.name,
                            hierarchy, .features_id)
            return(x)
          })
