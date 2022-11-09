# ==========================================================================
# comparation after filtering; add or remove classes for stardust_classes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases backtrack_stardust
#'
#' @title Recover filtered chemical classses for 'stardust_classes'
#'
#' @description
#' These methods used for custom modify chemical classes in 'stardust_classes'
#' data. Users can use the method to recover classes which filtered out by
#' [cross_filter_stardust()] into 'stardust_classes' data.
#' In addition, users can use the method to delete chemical classes in
#' 'stardust_classes' data.
#' 
#' @description
#' \code{backtrack_stardust(object)}: get the filtered chemical classes after
#' using [cross_filter_stardust()].
#'
#' Run after [cross_filter_stardust()].
#'
#' @seealso [cross_filter_stardust()]
#'
#' @name backtrack_stardust-methods
#'
#' @order 1
NULL
#> NULL

#' @exportMethod backtrack_stardust
#'
#' @aliases backtrack_stardust
#'
#' @param x [mcnebula-class] object.
#' @param class.name character. The chemical classes name.
#' @param rel.index numeric. The index number of chemical classes.
#' See columns of 'rel.index' in 'nebula_index' or 'stardust_classes'.
#' 
#' @param remove logical. If \code{TRUE}, remove the specified chemical
#' classes in 'stardust_classes' data. If \code{FALSE}, recover the
#' data of specified chemical classes into 'stardust_classes'; the classes
#' must in slot \code{backtrack(mcn_dataset(object))}.
#'
#' @rdname backtrack_stardust-methods
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
