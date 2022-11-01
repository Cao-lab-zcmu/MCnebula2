# ==========================================================================
# a class to store information about files in target dir, and as well,
# to read these files and save data in slots.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportClass project
#'
#' @aliases project
#'
#' @title Collection of Interface for extracting data from raw directory
#'
#' @description ...
#'
#' @family projects
#'
#' @slot project_version character.
#' @slot project_path ...
#' @slot project_conformation ...
#' @slot project_metadata ...
#' @slot project_api ...
#' @slot project_dataset ...
#'
#' @rdname project-class
#'
#' @examples
#' \dontrun{
#' new('project', ...)
#' }
.project <- 
  setClass("project", 
           contains = character(),
           representation = 
             representation(project_version = "character",
                            project_path = "character",
                            project_conformation = "project_conformation",
                            project_metadata = "project_metadata",
                            project_api = "project_api",
                            project_dataset = "project_dataset"
                            ),
           prototype = prototype(project_version = character(),
                                 project_path = character())
           )
# ==========================================================================
# method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportMethod project_version
#' @aliases project_version
#' @description \code{project_version}, \code{project_version<-}: getter and setter
#' for the \code{project_version} slot of the object.
#' @rdname project-class
setMethod("project_version", 
          signature = c(x = "ANY"),
          function(x){ x@project_version })
#' @exportMethod project_version<-
#' @aliases project_version<-
#' @param value The value for the slot.
#' @rdname project-class
setReplaceMethod("project_version", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, project_version = value)
                 })
#' @exportMethod project_path
#' @aliases project_path
#' @description \code{project_path}, \code{project_path<-}: getter and setter
#' for the \code{project_path} slot of the object.
#' @rdname project-class
setMethod("project_path", 
          signature = c(x = "ANY"),
          function(x){ x@project_path })
#' @exportMethod project_path<-
#' @aliases project_path<-
#' @param value The value for the slot.
#' @rdname project-class
setReplaceMethod("project_path", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, project_path = value)
                 })
## ------------------------------------- 
#' @exportMethod file_name
#' @aliases file_name
#' @description  \code{file_name}, \code{file_api}, \code{attribute_name}:
#' fast channel to obtain
#' the downstream slot. e.g., getter
#' for the \code{file_name} slot in sub-object
#' of \code{project_conformation} slot of the object. Equals:
#' - \code{file_name(project_conformation(object))}
#' - \code{file_name(object)}.
#' @rdname project-class
setMethod("file_name", 
          signature = c(x = "ANY"),
          function(x){
            file_name(project_conformation(x))
          })
#' @exportMethod file_api
#' @aliases file_api
#' @rdname project-class
setMethod("file_api", 
          signature = c(x = "ANY"),
          function(x){
            file_api(project_conformation(x))
          })
#' @exportMethod attribute_name
#' @aliases attribute_name
#' @rdname project-class
setMethod("attribute_name", 
          signature = c(x = "ANY"),
          function(x){
            attribute_name(project_conformation(x))
          })
## ------------------------------------- 
#' @exportMethod project_metadata
#' @aliases project_metadata
#' @description  \code{project_metadata}: fast channel to obtain
#' the downstream slot, getter
#' for the \code{project_metadata} slot in sub-object
#' of \code{project_metadata} slot of the object. Equals:
#' - \code{project_metadata(project_metadata(object))}
#' - \code{project_metadata(object)}.
#' @rdname project-class
setMethod("metadata", 
          signature = c(x = "ANY"),
          function(x){
            metadata(project_metadata(x))
          })
## ------------------------------------- 
#' @exportMethod methods_read
#' @aliases methods_read
#' @description \code{methods_read}, \code{methods_format}, \code{methods_match}:
#' fast channel to obtain
#' the downstream slot. e.g., getter
#' for the \code{methods_read} slot in sub-object
#' of \code{project_api} slot of the object. Equals:
#' - \code{methods_read(project_api(object))}
#' - \code{methods_read(object)}.
#' @rdname project-class
setMethod("methods_read", 
          signature = c(x = "ANY"),
          function(x){
            methods_read(project_api(x))
          })
#' @exportMethod methods_format
#' @aliases methods_format
#' @rdname project-class
setMethod("methods_format", 
          signature = c(x = "ANY"),
          function(x){
            methods_format(project_api(x))
          })
#' @exportMethod methods_match
#' @aliases methods_match
#' @rdname project-class
setMethod("methods_match", 
          signature = c(x = "ANY"),
          function(x){
            methods_match(project_api(x))
          })
#' @exportMethod match.candidates_id
#' @aliases match.candidates_id
#' @description \code{match.candidates_id}, \code{match.features_id}:
#' fast channel to obtain
#' data (mostly 'tbl' or 'data.frame') inside the downstream slot ('list'), getter
#' for the data named \code{match.candidates_id} in
#' \code{methods_match} slot (a 'list') in sub-object
#' of \code{project_api} slot of the object. Equals:
#' - \code{methods_match(project_api(object))$match.candidates_id}
#' - \code{match.candidates_id(object)}.
#' @rdname project-class
setMethod("match.candidates_id", 
          signature = c(x = "ANY"),
          function(x){
            methods_match(project_api(x))[[ "match.candidates_id" ]]
          })
#' @exportMethod match.features_id
#' @aliases match.features_id
#' @rdname project-class
setMethod("match.features_id", 
          signature = c(x = "ANY"),
          function(x){
            methods_match(project_api(x))[[ "match.features_id" ]]
          })
## ---------------------------------------------------------------------- 
#' @exportMethod get_upper_dir_subscript
#' @aliases get_upper_dir_subscript
#' @description \code{get_upper_dir_subscript}: ...
#' @param x ...
#' @param subscript ...
#' @rdname project-class
#' @examples
#' \dontrun{
#' get_upper_dir_subscript(...)
#' }
setMethod("get_upper_dir_subscript", 
          signature = setMissing("get_upper_dir_subscript",
                                 x = "ANY",
                                 subscript = "character"),
          function(x, subscript){
            stringr::str_extract(file_api(project_conformation(x))[[ subscript ]],
                                 paste0("(?<=^|/)[^/]*(?=/", subscript, "|$)"))
          })
## ---------------------------------------------------------------------- 

