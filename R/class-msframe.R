# ==========================================================================
# msframe: class based on data.frame
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportClass msframe
#'
#' @aliases msframe
#'
#' @title format and filter table data
#'
#' @description
#' Class for table data manipulation inside this package.
#'
#' @family subscripts
#'
#' @slot entity data.frame.
#' @slot subscript character(1). See [subscript-class].
#'
#' @rdname msframe-class
#'
.msframe <- 
  setClass("msframe", 
           contains = "subscript",
           representation = 
             representation(entity = "data.frame"),
           prototype = NULL
  )

# ==========================================================================
# methods
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportMethod show
#' @aliases show
#' @rdname msframe-class
setMethod("show", 
          signature = c(object = "msframe"),
          function(object){
            cat( "A class of \"msframe\" of", subscript(object), "\n")
          })


#' @exportMethod msframe
#' @aliases msframe
#' @description \code{msframe}, \code{msframe<-}: getter and setter
#' for the \code{msframe} slot of the object.
#' @rdname msframe-class
setMethod("msframe", 
          signature = c(x = "ANY"),
          function(x){ x@msframe })

#' @exportMethod msframe<-
#' @aliases msframe<-
#' @param value The value for the slot.
#' @rdname msframe-class
setReplaceMethod("msframe", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, msframe = value)
                 })


#' @exportMethod latest
#' @aliases latest
#' @description \code{latest}: get data inside \code{entity(object)} and format as
#' 'tbl'.
#' @family latests
#' @seealso [tibble::as_tibble()]
#' @rdname msframe-class
setMethod("latest", 
          signature = c(x = "msframe"),
          function(x){
            tibble::as_tibble(entity(x))
          })


#' @exportMethod entity
#' @aliases entity
#' @description \code{entity}, \code{entity<-}: getter and setter
#' for the \code{entity} slot of the object.
#' @rdname msframe-class
setMethod("entity", 
          signature = c(x = "msframe"),
          function(x){ x@entity })

#' @exportMethod entity<-
#' @aliases entity<-
#' @param value The value for the slot.
#' @rdname msframe-class
setReplaceMethod("entity", 
                 signature = c(x = "msframe"),
                 function(x, value){
                   initialize(x, entity = value)
                 })


#' @exportMethod format_msframe
#' @aliases format_msframe
#' @rdname msframe-class
setMethod("format_msframe", 
          signature = setMissing("format_msframe",
                                 x = "msframe",
                                 fun_format = "function"),
          function(x, fun_format){
            entity(x) <- format_msframe(entity(x), fun_format = fun_format)
            return(x)
          })

#' @exportMethod format_msframe
#' @aliases format_msframe
#' @rdname msframe-class
setMethod("format_msframe", 
          signature = setMissing("format_msframe",
                                 x = "data.frame",
                                 fun_format = "function"),
          function(x, fun_format){
            results <- try(fun_format(x), silent = T)
            if (!inherits(results, "try-error")) {
              x[[ ".candidates_id" ]] <- results
            }
            return(x)
          })


#' @exportMethod format_msframe
#' @aliases format_msframe
#' @rdname msframe-class
setMethod("format_msframe", 
          signature = setMissing("format_msframe",
                                 x = "msframe",
                                 names = "character",
                                 types = "character"),
          function(x, names, types){
            if( !is.character(names(names)) )
              stop( "the `names` is unformat" )
            if( !is.character(names(types)) )
              stop( "the `types` is unformat" )
            .format_msframe(x, names, types)
          })

#' @exportMethod format_msframe
#' @aliases format_msframe
#' @rdname msframe-class
setMethod("format_msframe", 
          signature = setMissing("format_msframe",
                                 x = "msframe"),
          function(x){
            names <- .get_attribute_name_sirius.v4()
            types <- .get_attribute_type_sirius.v4()
            .format_msframe(x, names, types)
          })

#' @exportMethod format_msframe
#'
#' @aliases format_msframe
#'
#' @description
#' \code{format_msframe}:
#'
#' @param x [msframe-class] object.
#' @param names character with names.
#' e.g., c(tani.score = "tanimotoSimilarity", mol.formula = "molecularFormula").
#' @param fun_names function to get names.
#' e.g., \code{MCnebula2:::.get_attribute_name_sirius.v4()}
#' @param types character with names.
#' e.g., c(tani.score = "numeric", mol.formula = "character").
#' @param fun_types function to get types.
#' e.g., \code{MCnebula2:::.get_attribute_type_sirius.v4()}
#' @param fun_format function to format slot \code{entity}.
#' e.g., \code{MCnebula2:::.format_msframe()}
#'
#' @rdname msframe-class
#'
setMethod("format_msframe", 
          signature = setMissing("format_msframe",
                                 x = "msframe",
                                 fun_names = "function",
                                 fun_types = "function"),
          function(x, fun_names, fun_types){
            .format_msframe(x, fun_names(), fun_types())
          })

.format_msframe <- 
  function(x, names, types){
    if( any(names(names) == "...sig") ) {
      rs <- which( names == subscript(x) & names(names) == "...sig")
      if (length(rs) != 0) {
        rs <- rs + 1
        re <- length(names)
        for( i in rs:length(names) ){
          if( names(names)[i] == "...sig" ) {
            re <- i - 1
            break
          }
        }
        names <- c(names[rs:re], names)
      }
      names <- vec_unique_by_value(names)
      names <- names[names(names) != "...sig"]
    }
    x <- .format_msframe_names(x, names)
    names <- names[names(names) %in% colnames(entity(x))]
    .format_msframe_types(x, names, types)
  }

.format_msframe_names <- 
  function(x, names){
    pattern <- paste0("^", names, "$")
    colnames(entity(x)) <-
      mapply_rename_col(pattern, names(names), colnames(entity(x)))
    return(x)
  }

.format_msframe_types <- 
  function(x, names, types){
    for (i in names(names)) {
      if (i %in% names(types))
        target_type <- types[[i]]
      else
        target_type <- "character"
      fun <- match.fun(paste0("is.", target_type))
      if ( !fun(entity(x)[[i]]) ){
        fun <- match.fun(paste0("as.", target_type))
        entity(x)[[i]] <- fun(entity(x)[[i]])
      }
    }
    return(x)
  }


#' @exportMethod filter_msframe
#' @aliases filter_msframe
#' @rdname msframe-class
setMethod("filter_msframe", 
          signature = setMissing("filter_msframe",
                                 x = "msframe", fun_filter = "function"),
          function(x, fun_filter, ...){
            filter_msframe(x, fun_filter = fun_filter,
                           f = ~ .features_id, ...)
          })

#' @exportMethod filter_msframe
#'
#' @aliases filter_msframe
#'
#' @description \code{filter_msframe}: filter data in slot \code{entity} (data.frame).
#' @note The class is not for normal use of the package.
#'
#' @param x [msframe-class] object.
#' @param fun_filter function used to filter the slot \code{entity} (data.frame).
#' e.g., \code{dplyr::filter()}, \code{head()}.
#' @param f formula passed to \code{split()}.
#' @param ... extra parameter passed to fun_filter.
#'
#' @rdname msframe-class
#'
setMethod("filter_msframe", 
          signature = setMissing("filter_msframe",
                                 x = "msframe", fun_filter = "function",
                                 f = "formula"),
          function(x, fun_filter, f, ...){
            .message_info("msframe", "filter_msframe",
                      paste0("group_by: ", paste0(f, collapse = " "))
            )
            entity <- lapply( split(entity(x), f = f), FUN = fun_filter, ...)
            entity(x) <- data.table::rbindlist(entity, fill = T)
            return(x)
          })

