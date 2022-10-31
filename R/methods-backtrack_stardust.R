# ==========================================================================
# comparation after filtering; add or remove classes for stardust_classes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportMethod backtrack_stardust
#'
#' @aliases backtrack_stardust
#'
#' @title ...
#'
#' @description ...
#'
#' @details ...
#'
#' @param x ...
#' @param class.name ...
#' @param rel.index ...
#' @param remove ...
#'
# @inheritParams rdname
#'
#' @return ...
#'
#' @seealso \code{\link{cross_filter_stardust}}
#'
#' @rdname backtrack_stardust-methods
#'
#' @order 1
#'
#' @examples
#' \dontrun{
#' backtrack_stardust(...)
#' }
setMethod("backtrack_stardust", 
          signature = setMissing("backtrack_stardust",
                                 x = "mcnebula"),
          function(x){
            .message_info("backtrack_stardust", "no args found",
                      "\n\tget filtered classes")
            set <- dplyr::filter(backtrack(mcn_dataset(x))[[ "stardust_classes" ]],
                                 !rel.index %in% stardust_classes(x)[[ "rel.index" ]])
            stat <- table(set$rel.index)
            df <- merge(data.frame(rel.index = as.integer(names(stat)),
                                   features_number = as.integer(stat)),
                        dplyr::select(classification(x),
                                      rel.index, class.name, description),
                        by = "rel.index", all.x = T)
            tibble::as_tibble(df)
          })
#' @exportMethod backtrack_stardust
#'
#' @description ...
#'
#' @rdname backtrack_stardust-methods
#'
#' @examples
#' \dontrun{
#' backtrack_stardust(...)
#' }
setMethod("backtrack_stardust", 
          signature = setMissing("backtrack_stardust",
                                 x = "mcnebula",
                                 class.name = "character",
                                 remove = "ANY"),
          function(x, class.name, remove){
            if (missing(remove))
              remove <- F
            rel.index <-
              dplyr::filter(classification(x),
                            class.name %in% !!class.name)[[ "rel.index" ]]
            backtrack_stardust(x, rel.index = rel.index, remove = remove)
          })
#' @exportMethod backtrack_stardust
#'
#' @description ...
#'
#' @rdname backtrack_stardust-methods
#'
#' @examples
#' \dontrun{
#' backtrack_stardust(...)
#' }
setMethod("backtrack_stardust", 
          signature = setMissing("backtrack_stardust",
                                 x = "mcnebula",
                                 rel.index = "numeric",
                                 remove = "ANY"),
          function(x, rel.index, remove){
            if (missing(remove))
              remove <- F
            else if (!is.logical(remove))
              stop( "`remove` must be logical or as missing as `FALSE`" )
            .message_info("backtrack_stardust", paste0("remove == ", remove))
            .check_data(x, list(stardust_classes = "create_stardust_classes"))
            if (remove) {
              reference(mcn_dataset(x))[[ "stardust_classes" ]] <- 
                dplyr::filter(stardust_classes(x), !rel.index %in% !!rel.index)
            } else {
              if (is.null(backtrack(mcn_dataset(x))[[ "stardust_classes" ]]))
                stop( "nothing in `backtrack(mcn_dataset(x))`" )
              set <- dplyr::filter(backtrack(mcn_dataset(x))[[ "stardust_classes" ]],
                                   rel.index %in% !!rel.index)
              if (nrow(set) == 0)
                stop( "no any record for specified classes in `backtrack(mcn_dataset(x))`" )
              reference(mcn_dataset(x))[[ "stardust_classes" ]] <- 
                dplyr::distinct(dplyr::bind_rows(stardust_classes(x), set))
            }
            return(x)
          })
