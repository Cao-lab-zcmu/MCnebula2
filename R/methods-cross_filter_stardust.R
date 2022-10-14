# ==========================================================================
# across attributes of each other features to filter classes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("cross_filter_stardust", 
          signature = setMissing("cross_filter_stardust",
                                 x = "missing"),
          function(){
            list(min_number = 30,
                 max_ratio = 0.1,
                 types = "tani.score",
                 cutoff = 0.3,
                 tolerance = 0.6,
                 hierarchy_range = c(3, 11),
                 identical_factor = 0.7
            )
          })
setMethod("cross_filter_stardust", 
          signature = c(x = "mcnebula"),
          function(x, min_number, max_ratio,
                   types, cutoff, tolerance,
                   hierarchy_range, identical_factor){
            .message_info_formal("MCnebula2", "cross_filter_stardust")
            .check_data(x, list(stardust_classes = "create_stardust_classes"))
            new_args <- .fresh_param(cross_filter_stardust())
            methods <- c("cross_filter_quantity", "cross_filter_score",
                         "cross_filter_identical")
            for (i in methods) {
              args <- new_args[names(new_args) %in% formalArgs(i)]
              new_args[[ "x" ]] <- do.call(match.fun(i), args)
            }
            backtrack(mcn_dataset(new_args[[ "x" ]]))[[ "stardust_classes" ]] <- 
              stardust_classes(x)
            return(new_args[[ "x" ]])
          })
setMethod("cross_filter_quantity", 
          signature = setMissing("cross_filter_quantity",
                                 x = "mcnebula", min_number = "numeric",
                                 max_ratio = "numeric"),
          function(x, min_number, max_ratio){
            .message_info("cross_filter_stardust", "quantity")
            if (min_number < 1) {
              stop( "`min_number` must be a numeric greater or equal to 1" )
            }
            if (!(max_ratio <= 1 & max_ratio > 0)) {
              stop( "`max_ratio` must be a numeric within (0, 1]" )
            }
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
            .message_info("cross_filter_stardust", "score")
            .check_data(x, list(features_annotation = "create_features_annotation"))
            set <- split(stardust_classes(x), f = ~ rel.index)
            features <- features_annotation(x)
            res <- mapply(types, cutoff, tolerance,
                          SIMPLIFY = F, USE.NAMES = F,
                          FUN = function(type, cutoff, tolerance){
                            if (!is.numeric(features[[ type ]]))
                              stop("the columns of `types` were not numeric")
                            express <- parse(text = type)
                            ref <- dplyr::filter(features, eval(express) >= cutoff)
                            ref <- ref[[ ".features_id" ]]
                            vapply(set, FUN.VALUE = T, USE.NAMES = F,
                                   function(data){
                                     ref <- ref[ref %in% data[[ ".features_id" ]]]
                                     if (length(ref) / nrow(data) >= tolerance)
                                       return(T)
                                     else
                                       return(F)
                                   })
                          })
            if (length(res) == 1) {
              set <- set[unlist(res)]
            } else {
              logic  <- res[[1]]
              for (i in res[2:length(res)]) {
                logic <- logic & i
              }
              set <- set[logic]
            }
            reference(mcn_dataset(x))[[ "stardust_classes" ]] <- 
              dplyr::as_tibble(data.table::rbindlist(set))
            return(x)
          })
setMethod("cross_filter_identical", 
          signature = setMissing("cross_filter_identical",
                                 x = "mcnebula", hierarchy_range = "numeric",
                                 identical_factor = "numeric"),
          function(x, hierarchy_range, identical_factor){
            .message_info("cross_filter_stardust", "identical")
            set <- dplyr::filter(stardust_classes(x),
                                 hierarchy %in% hierarchy_range)
            set <- split(set, f = ~ rel.index)
            ids <- lapply(set, `[[`, ".features_id")
            groups <- combn(1:length(ids), 2, simplify = F)
            discard <- lapply(groups,
                          function(group){
                            if (any( ids[group[1]] %in% ids[group[2]] )) {
                              p <- mapply(c(1, 2), c(2, 1),
                                          SIMPLIFY = F,
                                          FUN = function(x, y){
                                            table(ids[group[x]] %in% ids[group[y]])[[ "TRUE" ]]
                                          })
                              if (p[[1]] > identical_factor & p[[2]] > identical_factor) {
                                if (length(ids[group[1]]) < length(ids[group[2]]))
                                  return(group[1])
                                else
                                  return(group[2])
                              }
                            }
                          })
            discard_index <-
              unique( data.table::rbindlist( set[unlist(discard)] )[[ "rel.index" ]]
              )
            reference(mcn_dataset(x))[[ "stardust_classes" ]] <- 
              dplyr::filter(stardust_classes(x), !rel.index %in% discard_index)
            return(x)
          })
