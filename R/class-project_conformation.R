# ==========================================================================
# a class to store the characters of files or data in raw project.
# These generally describe the file name, file path, and attributes name.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportClass project_conformation
#'
#' @aliases project_conformation
#'
#' @title Clarify the name, path and attribute name of files
#' in the project (directory)
#'
#' @description 
#' This is a class object used to record the name, path and attribute name of the file.
#' These records can be retrieved by "subscript" (see [subscript-class]).
#' See [project-class] for joint application with other related classes.
#'
#' @note The class is not for normal use of the package.
#'
#' @family projects
#' @family subscripts
#'
#' @slot file_name character with names.
#' Record the filenames or pattern string or function name (begin with "FUN_")
#' for each "subscript" (imply file names).
#' @slot file_api character with names.
#' Record the file path for each "subscript" (imply file names).
#' The path is descriped by "subscript" with "/".
#' @slot attribute_name character with names.
#' Record the attribute name for each "subscript" (imply column names).
#'
#' @rdname project_conformation-class
#'
#' @examples
#' \dontrun{
#' new('project_conformation', ...)
#' }
.project_conformation <- 
  setClass("project_conformation", 
           contains = character(),
           representation = 
             representation(file_name = "character",
                            file_api = "character",
                            attribute_name = "character"
                            ),
           prototype = NULL
           )

# ==========================================================================
# validity
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setValidity("project_conformation", 
            function(object){
              check <- 
                slots_mapply(object,
                             function(slot, name){
                               if (is.character(slot) & length(slot) == 0) {
                                 TRUE
                               } else {
                                 if ( is.character( names(slot) ))
                                   TRUE
                                 else FALSE
                               }
                             })
              if (any(!check))
                "the colnames not matched."
              else TRUE
            })

# ==========================================================================
# method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportMethod show
#' @aliases show
#' @rdname project_conformation-class
setMethod("show", 
          signature = c(object = "project_conformation"),
          function(object){
            .show(object)
          })

## ------------------------------------- 
#' @exportMethod project_conformation
#' @aliases project_conformation
#' @description \code{project_conformation}, \code{project_conformation<-}: getter and setter
#' for the \code{project_conformation} slot of the object.
#' @rdname project_conformation-class
setMethod("project_conformation", 
          signature = "ANY",
          function(x){ x@project_conformation })

#' @exportMethod project_conformation<-
#' @aliases project_conformation<-
#' @param value The value for the slot.
#' @rdname project_conformation-class
setReplaceMethod("project_conformation", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, project_conformation = value)
                 })

## ---------------------------------------------------------------------- 
#' @exportMethod file_name
#' @aliases file_name
#' @description \code{file_name}, \code{file_name<-}: getter and setter
#' for the \code{file_name} slot of the object.
#' @rdname project_conformation-class
setMethod("file_name", 
          signature = c(x = "project_conformation"),
          function(x){ x@file_name })

#' @exportMethod file_name<-
#' @aliases file_name<-
#' @param value The value for the slot.
#' @rdname project_conformation-class
setReplaceMethod("file_name", 
                 signature = c(x = "project_conformation"),
                 function(x, value){
                   initialize(x, file_name = value)
                 })

## ------------------------------------- 
#' @exportMethod file_api
#' @aliases file_api
#' @description \code{file_api}, \code{file_api<-}: getter and setter
#' for the \code{file_api} slot of the object.
#' @rdname project_conformation-class
setMethod("file_api", 
          signature = c(x = "project_conformation"),
          function(x){ x@file_api })

#' @exportMethod file_api<-
#' @aliases file_api<-
#' @param value The value for the slot.
#' @rdname project_conformation-class
setReplaceMethod("file_api", 
                 signature = c(x = "project_conformation"),
                 function(x, value){
                   initialize(x, file_api = value)
                 })

## ------------------------------------- 
#' @exportMethod attribute_name
#' @aliases attribute_name
#' @description \code{attribute_name}, \code{attribute_name<-}: getter and setter
#' for the \code{attribute_name} slot of the object.
#' @rdname project_conformation-class
setMethod("attribute_name", 
          signature = c(x = "project_conformation"),
          function(x){ x@attribute_name })

#' @exportMethod attribute_name<-
#' @aliases attribute_name<-
#' @param value The value for the slot.
#' @rdname project_conformation-class
setReplaceMethod("attribute_name", 
                 signature = c(x = "project_conformation"),
                 function(x, value){
                   initialize(x, attribute_name = value)
                 })

## ------------------------------------- 

