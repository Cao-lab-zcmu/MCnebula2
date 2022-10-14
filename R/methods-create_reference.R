# ==========================================================================
# create reference data based on mcn_dataset
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("create_reference", 
          signature = c(x = "mcnebula", fill = "logical"),
          function(x, from, subscript, data, columns, fill, MoreArgs){
            args <- as.list(environment())
            args <- args[!names(args) %in% c("fill", "MoreArgs")]
            args <- args[ !vapply(args, is.name, T) ]
            if (length(args) == 1)
              x <- create_reference(x, "structure")
            else
              x <- do.call(create_reference, args)
            if (fill) {
              .message_info("create_reference", "fill == T",
                        "\n\tfilling missing features with filtered formula")
              if (!missing(MoreArgs))
                x <- do.call(filter_formula, c(x, MoreArgs))
              else
                x <- filter_formula(x)
              if (any(duplicated(latest(x)$.features_id)))
                stop("the filtered formula must unique in `.features_id`")
              .ref <- specific_candidate(create_reference(x, data = latest(x)))
              reference(mcn_dataset(x))[[ "specific_candidate" ]] <- 
                dplyr::distinct(dplyr::bind_rows(specific_candidate(x), .ref),
                                .features_id, .keep_all = T)
            }
            return(x)
          })
setMethod("create_reference", 
          signature = setMissing("create_reference",
                                 x = "mcnebula"),
          function(x){
            create_reference(x, "structure", fill = T)
          })
setMethod("create_reference", 
          signature = setMissing("create_reference",
                                 x = "mcnebula", from = "character"),
          function(x, from){
            subscript <- switch(from,
                                structure = ".f3_fingerid",
                                formula = ".f2_formula",
                                ppcp = ".f3_canopus"
            )
            create_reference(x, subscript = subscript)
          })
setMethod("create_reference", 
          signature = setMissing("create_reference",
                                 x = "mcnebula",
                                 subscript = "character"),
          function(x, subscript){
            .message_info_formal("MCnebula2", "create_reference")
            data <- try(entity(dataset(mcn_dataset(x))[[ subscript ]]), silent = T)
            if (inherits(data, "try-error")) {
              stop(paste0("the specified dataset not exists. use, e.g., ",
                          "`filter_structure(x)` previously."))
            }
            if (subscript == ".f3_canopus") {
              data <- dplyr::distinct(data, .features_id, .candidates_id)
            }
            create_reference(x, data = data)
          })
setMethod("create_reference", 
          signature = setMissing("create_reference",
                                 x = "mcnebula",
                                 data = "data.frame",
                                 columns = "character"),
          function(x, data, columns){
            if (length(columns) != 2)
              stop( "length(`columns`) != 2" )
            colnames(data)[which(colnames(data) == columns)] <- 
              c(".features_id", ".candidates_id")
            create_reference(x, data = data)
          })
setMethod("create_reference", 
          signature = setMissing("create_reference",
                                 x = "mcnebula",
                                 data = "data.frame",
                                 columns = "integer"),
          function(x, data, columns){
            if (length(columns) != 2)
              stop( "length(`columns`) != 2" )
            colnames(data)[columns] <- c(".features_id", ".candidates_id")
            create_reference(x, data = data)
          })
setMethod("create_reference", 
          signature = setMissing("create_reference",
                                 x = "mcnebula",
                                 data = "data.frame"),
          function(x, data){
            if (any( duplicated(data[[ ".features_id" ]]) ))
              stop("`.features_id` in `data` were not unique")
            fun <- methods_match(project_api(x))[[ "generate_candidates_id" ]]
            data <- format_msframe(data, fun_format = fun)
            reference(mcn_dataset(x))[[ "specific_candidate" ]] <- 
              dplyr::as_tibble(dplyr::select(data, .features_id, .candidates_id))
            return(x)
          })
