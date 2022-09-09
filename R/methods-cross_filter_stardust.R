# ==========================================================================
# across attributes of each other features to filter classes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("cross_filter_stardust", 
          signature = setMissing("cross_filter_stardust",
                                 x = "mcnebula"),
          function(x){

          })
setMethod("cross_filter_stardust", 
          signature = setMissing("cross_filter_stardust",
                                 x = "mcnebula", args = "list"),
          function(x, args){

          })
setMethod("cross_filter_quantity", 
          signature = setMissing("cross_filter_quantity",
                                 x = "mcnebula", min_number = "numeric",
                                 max_ratio = "numeric"),
          function(x, min_number, max_ratio){
            sum <- length( unique(stardust_classes(x)[[ ".features_id" ]]) )
            set <- split(stardust_classes(x), ~ rel.index)
            set <- lapply(set, function(df)
                          if (nrow(df) >= min_number &
                              nrow(df) / sum <= max_ratio) df)
            reference(mcn_dataset(x))[[ "stardust_classes" ]] <- 
              dplyr::as_tibble(data.table::rbindlist(set))
            return(x)
          })
setMethod("cross_filter_score", 
          signature = setMissing("cross_filter_score",
                                 x = "mcnebula", types = "character",
                                 cutoff = "numeric", tolerance = "numeric"),
          function(x, types, cutoff, tolerance){

          })
setMethod("cross_filter_identical", 
          signature = setMissing("cross_filter_identical",
                                 x = "mcnebula", hierarchy_range = "numeric",
                                 identical_factor = "numeric"),
          function(x, hierarchy_range, identical_factor){

          })
