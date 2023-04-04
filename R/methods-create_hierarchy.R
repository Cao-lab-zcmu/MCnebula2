# ==========================================================================
# collate and build classification hierarchy annotation data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases create_hierarchy
#'
#' @title Create hierarchy data of chemical classification
#'
#' @description
#' Methods used to create hierarchy data of chemical classification.
#' Annotate all chemical classes with hierarchy number.
#'
#' @name create_hierarchy-methods
#'
#' @order 1
NULL
#> NULL

#' @exportMethod create_hierarchy
#' @description \code{create_hierarchy()}:
#' get the default parameters for the method
#' \code{create_hierarchy}.
#' @rdname create_hierarchy-methods
setMethod("create_hierarchy", 
          signature = setMissing("create_hierarchy"),
          function(){
            list(fun_organize = .build_hierarchy)
          })

#' @exportMethod create_hierarchy
#' @description \code{create_hierarchy(x, ...)}:
#' use the default parameters whatever 'missing'
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
#' @param x [mcnebula-class] object.
#' @param fun_organize function. Normally not used.
#' Default is \code{MCnebula2:::.build_hierarchy}.
#'
#' @rdname create_hierarchy-methods
#'
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

    while(nrow(df) > 0){
      n <- n + 1
      list[[n]] <- df
      df <- data[data$parent.chem.ont.id %in% df$chem.ont.id, ]
    }
    data <- data.table::rbindlist(list, idcol = T)
    data$.id <- data$.id - 1
    dplyr::rename(dplyr::as_tibble(data), hierarchy = .id)
  }

#' @export get_parent_classes
#' @aliases get_parent_classes
#' @description \code{get_parent_classes}: For chemical classes to get
#' its parent chemical classes.
#' @param classes character. Names of chemical classes.
#' @param hierarchy_cutoff. numeric(1). The highest hierarchy of parent chemical classes
#' that needs to be searched.
#' @param re_class_no_parent logical(1). If \code{TRUE}, once a chemical class find with
#' no parent, the chemical class itself would be returned.
#' @rdname create_hierarchy-methods
get_parent_classes <- 
  function(classes, x,
           hierarchy_cutoff = 3,
           re_class_no_parent = F
           ){
    .check_data(x, list(hierarchy = "create_hierarchy"))
    db <- dplyr::filter(hierarchy(x), hierarchy >= hierarchy_cutoff)
    ## as 'dictionary'
    name2id <- .as_dic(db$chem.ont.id, db$class.name, fill = F)
    id2parent <- .as_dic(db$parent.chem.ont.id, db$chem.ont.id, fill = F)
    id2name <- .as_dic(db$class.name, db$chem.ont.id, fill = F)
    sapply(classes, simplify = F,
           function(class){
             set <- c()
             parent <- 0
             id <- name2id[[class]]
             test <- try(id2parent[[id]], silent = T)
             if (inherits(test, "try-error"))
               if(re_class_no_parent)
                 return(class)
               else
                 return()
             while(!is.null(parent)){
               if(parent != 0){
                 set <- c(set, id2name[[parent]])
                 id <- parent
               }
               parent <- id2parent[[id]]
             }
             if(length(set) == 0){
               if(re_class_no_parent)
                 return(class)
             }
             return(set)
           })
  }

