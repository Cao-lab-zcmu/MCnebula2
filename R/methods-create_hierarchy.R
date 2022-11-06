# ==========================================================================
# collate and build classification hierarchy annotation data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases create_hierarchy
#'
#' @title ...
#'
#' @description ...
#'
#' @details ...
#'
#' @name create_hierarchy-methods
#'
#' @order 1
NULL
#> NULL

#' @exportMethod create_hierarchy
#' @description \code{create_hierarchy()}: get the default parameters for the method
#' \code{create_hierarchy}.
#' @rdname create_hierarchy-methods
setMethod("create_hierarchy", 
          signature = setMissing("create_hierarchy"),
          function(){
            list(fun_organize = .build_hierarchy)
          })
#' @exportMethod create_hierarchy
#' @description \code{create_hierarchy(x, ...)}: use the default parameters whatever 'missing'
#' while performing the method \code{create_hierarchy}.
#' @rdname create_hierarchy-methods
setMethod("create_hierarchy", 
          signature = c(x = "mcnebula"),
          function(x, fun_organize){
            reCallMethod("create_hierarchy",
                         .fresh_param(create_hierarchy()))
          })
#' @exportMethod create_hierarchy
#'
#' @aliases create_hierarchy
#'
#' @title ...
#'
#' @description ...
#'
#' @details ...
#'
#' @param x ...
#' @param fun_organize ...
#'
# @inheritParams rdname
#'
#' @return ...
#'
#' @rdname create_hierarchy-methods
#'
#' @examples
#' \dontrun{
#' create_hierarchy(...)
#' }
setMethod("create_hierarchy", 
          signature = c(x = "mcnebula", fun_organize = "function"),
          function(x, fun_organize){
            class <- classification(x)
            if (is.null(class)) {
              x <- collate_data(x, ".canopus")
              class <- classification(x)
            }
            reference(mcn_dataset(x))[[ "hierarchy" ]] <- 
              fun_organize(class)
            return(x)
          })
.build_hierarchy <- 
  function(data){
    data <- dplyr::select(data, rel.index, chem.ont.id,
                           class.name, parent.chem.ont.id)
    root <- data[data$parent.chem.ont.id == "", ]
    list <- list()
    length(list) <- 12
    n <- 1
    list[[n]] <- root
    df <- data[data$parent.chem.ont.id %in% root$chem.ont.id, ]
    ## ---------------------------------------------------------------------- 
    while(nrow(df) > 0){
      n <- n + 1
      list[[n]] <- df
      df <- data[data$parent.chem.ont.id %in% df$chem.ont.id, ]
    }
    data <- data.table::rbindlist(list, idcol = T)
    data$.id <- data$.id - 1
    dplyr::rename(dplyr::as_tibble(data), hierarchy = .id)
  }
