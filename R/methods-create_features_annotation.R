# ==========================================================================
# create features annotation data.frame, involves formula and structure,
# based on `specific_candidate`
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases create_features_annotation
#'
#' @title ...
#'
#' @description ...
#'
#' @details ...
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
#' @title ...
#'
#' @description ...
#'
#' @details ...
#'
#' @param x ...
#' @param extra_data ...
#' @param column ...
#'
# @inheritParams rdname
#'
#' @return ...
#'
# @seealso ...
#'
#' @rdname create_features_annotation-methods
#'
#' @order 1
#'
#' @examples
#' \dontrun{
#' create_features_annotation(...)
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
                              stop( paste0("the filtered \"", sub, "\" set in `x` must match ",
                                           "with the id columns ",
                                           "(.features_id, .candidates_id) ",
                                           "in `specific_candidate(x)`, ",
                                           "use `filter_", name,
                                           "(x, by_reference = T)` previously."))
                            } else {
                              set <- merge(dplyr::select(ref, .features_id),
                                           dplyr::select(set, -.candidates_id),
                                           by = ".features_id", all.x = T)
                              return(set)
                            }
                                 })
            res <- merge(lst[[1]], lst[[2]], by = ".features_id", all = T)
            res <- merge(ref, res, by = ".features_id", all.x = T)
            ## add ionMass and retention time for features
            x <- collate_data(x, subscript = ".f2_info")
            mz_rt <- dplyr::select(latest(x, "project_dataset", ".f2_info"),
                                   .features_id, mz, rt.secound)
            reference(mcn_dataset(x))[[ "features_annotation" ]] <-
              dplyr::as_tibble(merge(res, mz_rt, by = ".features_id", all.x = T))
            return(x)
          })
