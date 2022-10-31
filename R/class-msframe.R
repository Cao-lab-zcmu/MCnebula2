# ==========================================================================
# msframe: class based on data.frame
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportClass msframe
#'
#' @aliases msframe
#'
#' @title ...
#'
#' @description ...
#'
#' @family subscripts
#'
#' @slot entity ...
#' @slot subscript ...
#'
#' @rdname msframe-class
#'
#' @examples
#' \dontrun{
#' new('msframe', ...)
#' }
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
## ------------------------------------- 
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
## ------------------------------------- 
#' @exportMethod latest
#' @aliases latest
#' @description \code{latest}: get data inside \code{entity(object)} and format as
#' 'tbl'.
#' @family latests
#' @seealso [tibble::as_tibble()]
#' @rdname msframe-class
#' @examples
#' \dontrun{
#' latest(...)
#' }
setMethod("latest", 
          signature = c(x = "msframe"),
          function(x){
            tibble::as_tibble(entity(x))
          })
## ------------------------------------- 
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
## ---------------------------------------------------------------------- 
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
## ------------------------------------- 
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
#' @title ...
#'
#' @description ...
#'
#' @details ...
#'
#' @param x ...
#' @param names ...
#' @param fun_names ...
#' @param types ...
#' @param fun_types ...
#' @param fun_format ...
#'
#' @rdname msframe-class
#'
#' @examples
#' \dontrun{
#' format_msframe(...)
#' }
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
      rs <- rs + 1
      re <- length(names)
      for( i in rs:length(names) ){
        if( names(names)[i] == "...sig" ) {
          re <- i - 1
          break
        }
      }
      names <- c(names[rs:re], names)
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
## ------------------------------------- 
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
#' @title ...
#'
#' @description ...
#'
#' @details ...
#'
#' @param x ...
#' @param fun_filter ...
#' @param f ...
#' @param ... ...
#'
# @inheritParams rdname
#'
#' @return ...
#'
# @seealso ...
#'
#' @rdname filter_msframe-methods
#'
#' @order 1
#'
#' @examples
#' \dontrun{
#' filter_msframe(...)
#' }
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

