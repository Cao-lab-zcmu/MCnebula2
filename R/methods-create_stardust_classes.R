# ==========================================================================
# filter classification for each features, as stardust classes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("create_stardust_classes", 
          signature = setMissing("create_stardust_classes",
                                 x = "missing"),
          function(){
            list(pp.threashold = 0.5,
                 hierarchy_priority = 5:2,
                 position_isomerism = T,
                 inherit_dataset = F)
          })
setMethod("create_stardust_classes", 
          signature = c(x = "mcnebula"),
          function(x, pp.threashold, hierarchy_priority,
                   position_isomerism, inherit_dataset){
            reCallMethod("create_stardust_classes",
                         .fresh_param(create_stardust_classes()))
          })
setMethod("create_stardust_classes", 
          signature = c(x = "mcnebula",
                        pp.threashold = "numeric",
                        hierarchy_priority = "numeric",
                        position_isomerism = "logical",
                        inherit_dataset = "logical"),
          function(x, pp.threashold, hierarchy_priority,
                   position_isomerism, inherit_dataset){
            .message_info_formal("MCnebula2", "create_stardust_classes")
            if (is.null(hierarchy(x)))
              x <- create_hierarchy(x)
            hierarchy <- hierarchy(x)
            if (inherit_dataset) {
              dataset <- latest(x, subscript = ".f3_canopus")
              check <- dplyr::distinct(dataset, .features_id, .candidates_id)
              if (any( duplicated(check[[ ".features_id" ]]) ))
                stop("`.candidates_id` for features in ppcp dataset were not unique")
            } else {
              x <- filter_ppcp(x, pp.threashold = pp.threashold)
              dataset <- latest(x)
            }
            dataset <-
              merge(dplyr::select(dataset, .features_id, .candidates_id,
                                  pp.value, rel.index),
                    hierarchy, by = "rel.index", all.x = T)
            dataset <- dplyr::filter(dataset, hierarchy %in% hierarchy_priority)
            if (position_isomerism) {
              dataset <- dplyr::filter(dataset, !grepl("[0-9]", class.name))
            }
            reference(mcn_dataset(x))[[ "stardust_classes" ]] <- 
              dplyr::relocate(dplyr::as_tibble(dataset), .features_id, .candidates_id)
            return(x)
          })
