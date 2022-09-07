# ==========================================================================
# create reference data based on mcn_dataset
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("create_reference", 
          signature = c(x = "mcnebula", rep("ANY", 4), fill = "logical"),
          function(x, from, subscript, data, columns, fill){
            args <- as.list(environment())
            args$fill <- NULL
            args <- args[ !vapply(args, is.name, T) ]
            x <- do.call(create_reference, args)
            if (fill) {
              .get_info("create_reference", "fill",
                        "\nfilling missing features with top formula")
              x <- filter_formula(x)
              .ref <- specific_candidate(create_reference(x, data = latest(x)))
              reference(mcn_dataset(x))[[ "specific_candidate" ]] <- 
                dplyr::distinct(dplyr::bind_rows(specific_candidate(x), .ref),
                                .features_id, .keep_all = T)
            }
            return(x)
          })
setMethod("create_reference", 
          signature = c(x = "mcnebula"),
          function(x){
            create_reference(x, "structure")
          })
setMethod("create_reference", 
          signature = c(x = "mcnebula", from = "character"),
          function(x, from){
            subscript <- switch(from,
                                structure = ".f3_fingerid",
                                formula = ".f2_formula",
                                ppcp = ".f3_canopus"
            )
            create_reference(x, subscript = subscript)
          })
setMethod("create_reference", 
          signature = c(x = "mcnebula", from = "missing",
                        subscript = "character"),
          function(x, subscript){
            .get_info_formal("MCnebula2", "create_reference")
            data <- entity(dataset(mcn_dataset(x))[[ subscript ]])
            if (subscript == ".f3_canopus") {
              data <- dplyr::distinct(data, .features_id, .candidates_id)
            }
            create_reference(x, data = data)
          })
setMethod("create_reference", 
          signature = c(x = "mcnebula", from = "missing",
                        subscript = "missing", data = "data.frame",
                        columns = "character"),
          function(x, data, columns){
            if (length(columns) != 2)
              stop( "length(`columns`) != 2" )
            colnames(data)[which(colnames(data) == columns)] <- 
              c(".features_id", ".candidates_id")
            create_reference(x, data = data)
          })
setMethod("create_reference", 
          signature = c(x = "mcnebula", from = "missing",
                        subscript = "missing", data = "data.frame",
                        columns = "integer"),
          function(x, data, columns){
            if (length(columns) != 2)
              stop( "length(`columns`) != 2" )
            colnames(data)[columns] <- c(".features_id", ".candidates_id")
            create_reference(x, data = data)
          })
setMethod("create_reference", 
          signature = c(x = "mcnebula", from = "missing",
                        subscript = "missing", data = "data.frame"),
          function(x, data){
            if (any( duplicated(data[[ ".features_id" ]]) ))
              stop("`.features_id` in `data` were not unique")
            fun <- methods_match(project_api(x))[[ "generate_candidates_id" ]]
            results <- try(fun(data), silent = T)
            if (inherits(results, "try-error"))
              results <- try(data[[ ".candidates_id" ]])
            reference(mcn_dataset(x))[[ "specific_candidate" ]] <- 
              tibble::tibble(.features_id = data[[ ".features_id" ]],
                             .candidates_id = results)
            x
          })
