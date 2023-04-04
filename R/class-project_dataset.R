# ==========================================================================
# a class to store dataset extract from raw data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportClass project_dataset
#'
#' @aliases project_dataset
#'
#' @title Store extracted data
#'
#' @description 
#' This is a class object used to store extracted data (raw data).
#' See [project-class] for joint application with other related classes.
#'
#' @family projects
#' @family datasets
#'
#' @slot dataset list. See [dataset-class].
#'
#' @rdname project_dataset-class
#'
.project_dataset <- 
  setClass("project_dataset", 
           contains = "dataset",
           prototype = NULL
           )

# ==========================================================================
# method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportMethod project_dataset
#' @aliases project_dataset
#' @description \code{project_dataset}, \code{project_dataset<-}: getter and setter
#' for the \code{project_dataset} slot of the object.
#' @rdname project_dataset-class
setMethod("project_dataset", 
          signature = c(x = "ANY"),
          function(x){ x@project_dataset })

#' @exportMethod project_dataset<-
#' @aliases project_dataset<-
#' @param value The value for the slot.
#' @rdname project_dataset-class
setReplaceMethod("project_dataset", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, project_dataset = value)
                 })


#' @exportMethod latest
#' @aliases latest
#' @description \code{latest}: get the first data in \code{dataset} slot ('list') and
#' format as 'tbl'. Equals:
#' - \code{latest(object)}
#' - \code{tibble::as_tibble(entity(dataset(object)[[1]]))}
#' @family latests
#' @seealso [tibble::as_tibble()]
#' @rdname project_dataset-class
setMethod("latest", 
          signature = c(x = "project_dataset"),
          function(x){
            tibble::as_tibble(entity(dataset(x)[[1]]))
          })


#' @exportMethod extract_rawset
#' @aliases extract_rawset
#' @rdname project_dataset-class
setMethod("extract_rawset", 
          signature = c(x = "ANY", subscript = "character"),
          function(x, subscript){
            extract_rawset(x, subscript = subscript,
                           fun_collate = function(...){
                             stop("`subscript` not found in `dataset(project_dataset(x))`")
                           })
          })

#' @exportMethod extract_rawset
#' @aliases extract_rawset
#' @description \code{extract_rawset}: For fast extract data in object which containing
#' \code{project_dataset} slot. Normally not used.
#' @param x an object contain \code{project_dataset} slot.
#' @param subscript character. Specified the data in \code{dataset} slot
#' in \code{project_dataset} slot.
#' See [VIRTUAL_subscript-class].
#' @param fun_collate function. If the specified data not exists in \code{dataset} slot,
#' it will be used to collate data. This parameter is not for normal use.
#' @param ... parameters passed to 'fun_collate'.
#' @rdname project_dataset-class
setMethod("extract_rawset", 
          signature = c(x = "ANY",
                        subscript = "character",
                        fun_collate = "function"
                        ),
          function(x, subscript, fun_collate, ...){
            if ( any( subscript == names(dataset(project_dataset(x))) ) )
              msframe <- dataset(project_dataset(x))[[ subscript ]]
            else
              msframe <- fun_collate(x, subscript, ...)
            lst <- list(msframe)
            names(lst) <- subscript
            return(lst)
          })
