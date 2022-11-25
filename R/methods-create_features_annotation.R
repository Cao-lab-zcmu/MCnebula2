# ==========================================================================
# create features annotation data.frame, involves formula and structure,
# based on `specific_candidate`
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases create_features_annotation
#'
#' @title merge annotation for 'features'
#'
#' @description
#' According to \code{specific_candidate(object)} data, merge the latest
#' filtered chemical formulae annotation, structural annotation. The ion mass
#' and retention time for each 'feature' would also be gathered.
#' User can also pass custom annotation for each 'feature', as long as the
#' 'data.frame' with column of '.features_id'.
#'
#' @details
#' The 'features_annotation' data created from:
#' - The 'specific_candidate' data: \code{specific_candidate(object)}
#' - The filtered chemical formula data: \code{latest(object, subscript = ".f2_formula")}
#' - The filtered structural data: \code{latest(object, subscript = ".f3_fingerid")}
#' - The ion mass and retention time (m/z and RT): latest(object, "project_dataset", ".f2_info")
#' 
#' The last would be collated via: \code{collate_data(object, subscript = ".f2_info")}
#' 
#' @name create_features_annotation-methods
#'
#' @order 1
NULL
#> NULL

#' @exportMethod create_features_annotation
#' @rdname create_features_annotation-methods
setMethod("create_features_annotation", 
          signature = setMissing("create_features_annotation",
                                 x = "mcnebula",
                                 extra_data = "data.frame",
                                 column = "numeric"),
          function(x, extra_data, column){
            colnames(extra_data)[column] <- ".features_id"
            create_features_annotation(x, extra_data)
          })

#' @exportMethod create_features_annotation
#' @rdname create_features_annotation-methods
setMethod("create_features_annotation", 
          signature = setMissing("create_features_annotation",
                                 x = "mcnebula",
                                 extra_data = "data.frame"),
          function(x, extra_data){
            if (is.null(features_annotation(x)))
              x <- create_features_annotation(x)
            if ( !".features_id" %in% colnames(extra_data) )
              stop( "id column not found" )
            reference(mcn_dataset(x))[[ "features_annotation" ]] <-
              merge(features_annotation(x), extra_data,
                    by = ".features_id", all.x = T)
            return(x)
          })

#' @exportMethod create_features_annotation
#'
#' @aliases create_features_annotation
#'
#' @param x [mcnebula-class] object.
#' @param extra_data data.frame.
#' @param column numeric(1). If name of columns not contain ".features_id",
#' used to specify ID column for 'features'.
#'
#' @rdname create_features_annotation-methods
#'
#' @examples
#' \dontrun{
#'   test <- mcn_5features
#'   
#'   ## the previous steps
#'   test1 <- filter_structure(test)
#'   test1 <- create_reference(test1)
#'   test1 <- filter_formula(test1, by_reference=T)
#'   test1 <- create_stardust_classes(test1)
#'   
#'   test1 <- create_features_annotation(test1)
#'   ## see results
#'   features_annotation(test1)
#'   ## or
#'   reference(test1)$features_annotation
#'   ## or
#'   reference(mcn_dataset(test1))$features_annotation
#'   
#'   ## merge additional data
#'   ids <- features_annotation(test1)$.features_id
#'   data <- data.frame(.features_id = ids, quant. = rnorm(length(ids), 1000, 200))
#'   test1 <- create_features_annotation(test1, extra_data = data)
#' }
setMethod("create_features_annotation", 
          signature = setMissing("create_features_annotation",
                                 x = "mcnebula"),
          function(x){
            .message_info_formal("MCnebula2", "create_features_annotation")
            .check_data(x, list(specific_candidate = "create_reference"))
            ref <- specific_candidate(x)
            ## formula dataset
            subscript <- c(".f2_formula", ".f3_fingerid")
            lst <- lapply(subscript, function(sub){
                            set <- latest(x, subscript = sub)
                            idcol <- dplyr::select(set, .features_id, .candidates_id)
                            check <- dplyr::distinct(dplyr::bind_rows(idcol, ref),
                                                     .features_id, .candidates_id)
                            if (any( duplicated(check[[ ".features_id" ]]) )) {
                              name <- gsub("^.*_", "", sub)
                              stop( "the filtered \"", sub, "\" set in `x` must match ",
                                   "with the id columns ",
                                   "(.features_id, .candidates_id) ",
                                   "in `specific_candidate(x)`, ",
                                   "use `filter_", name,
                                   "(x, by_reference = T)` previously.")
                            } else {
                              set <- merge(dplyr::select(ref, .features_id),
                                           dplyr::select(set, -.candidates_id),
                                           by = ".features_id", all.x = T)
                              return(set)
                            }
                                 })
            res <- checkColMerge(lst[[1]], lst[[2]], by = ".features_id", all = T)
            res <- merge(ref, res, by = ".features_id", all.x = T)
            ## add ionMass and retention time for features
            x <- collate_data(x, subscript = ".f2_info")
            mz_rt <- dplyr::select(latest(x, "project_dataset", ".f2_info"),
                                   .features_id, mz, rt.secound)
            reference(mcn_dataset(x))[[ "features_annotation" ]] <-
              dplyr::as_tibble(merge(res, mz_rt, by = ".features_id", all.x = T))
            return(x)
          })
