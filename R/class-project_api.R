# ==========================================================================
# a class to store functions of reading or formating the target data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportClass project_api
#'
#' @aliases project_api
#'
#' @title Function set for extracting data
#'
#' @description 
#' This is a class object used to store various functions for extracting and formatting data.
#' See [project-class] for joint application with other related classes.
#' @family projects
#'
#' @note The class is not for normal use of the package.
#'
#' @slot methods_read list. Store a list of functions for reading data.
#' The list with the names: "read" + "subscript". e.g., "read.f3_fingerid".
#' @slot methods_format function. The function is used to format the data
#' (e.g., rename the column names; convert the columns of character type into numeric).
#' @slot methods_match list. Store a list of functions for matching and extracting string.
#'
#' @rdname project_api-class
#'
#' @examples
#' \dontrun{
#' new('project_api', ...)
#' }
.project_api <- 
  setClass("project_api", 
           contains = character(),
           representation = 
             representation(methods_read = "list",
                            methods_format = "function",
                            methods_match = "list"
                            ),
           prototype = NULL
           )

# ==========================================================================
# method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportMethod show
#' @aliases show
#' @rdname project_api-class
setMethod("show", 
          signature = c(object = "project_api"),
          function(object){
            .show(object)
          })

#' @exportMethod project_api
#' @aliases project_api
#' @description \code{project_api}, \code{project_api<-}: getter and setter
#' for the \code{project_api} slot of the object.
#' @rdname project_api-class
setMethod("project_api", 
          signature = c(x = "ANY"),
          function(x){ x@project_api })

#' @exportMethod project_api<-
#' @aliases project_api<-
#' @param value The value for the slot.
#' @rdname project_api-class
setReplaceMethod("project_api", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, project_api = value)
                 })

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportMethod methods_read
#' @aliases methods_read
#' @description \code{methods_read}, \code{methods_read<-}: getter and setter
#' for the \code{methods_read} slot of the object.
#' @rdname project_api-class
setMethod("methods_read", 
          signature = c(x = "project_api"),
          function(x){ x@methods_read })

#' @exportMethod methods_read<-
#' @aliases methods_read<-
#' @param value The value for the slot.
#' @rdname project_api-class
setReplaceMethod("methods_read", 
                 signature = c(x = "project_api"),
                 function(x, value){
                   initialize(x, methods_read = value)
                 })

## ------------------------------------- 
#' @exportMethod methods_format
#' @aliases methods_format
#' @description \code{methods_format}, \code{methods_format<-}: getter and setter
#' for the \code{methods_format} slot of the object.
#' @rdname project_api-class
setMethod("methods_format", 
          signature = c(x = "project_api"),
          function(x){ x@methods_format })

#' @exportMethod methods_format<-
#' @aliases methods_format<-
#' @param value The value for the slot.
#' @rdname project_api-class
setReplaceMethod("methods_format", 
                 signature = c(x = "project_api"),
                 function(x, value){
                   initialize(x, methods_format = value)
                 })

## ------------------------------------- 
#' @exportMethod methods_match
#' @aliases methods_match
#' @description \code{methods_match}, \code{methods_match<-}: getter and setter
#' for the \code{methods_match} slot of the object.
#' @rdname project_api-class
setMethod("methods_match", 
          signature = c(x = "project_api"),
          function(x){ x@methods_match })

#' @exportMethod methods_match<-
#' @aliases methods_match<-
#' @param value The value for the slot.
#' @rdname project_api-class
setReplaceMethod("methods_match", 
                 signature = c(x = "project_api"),
                 function(x, value){
                   initialize(x, methods_match = value)
                 })
