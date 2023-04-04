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
#' @description 
#' 
#' This is a class object designed to extract files in the project directory.
#' Its responsibility is to describe the name,
#' path and reading method of the file under the project directory;
#' Use these information to extract and store data.
#'
#' @details
#' It is a collection of classes whose names start with "project_":
#' - [project_conformation-class]: The name, path and attribute name of the file are described.
#' - [project_api-class]: Functions for reading and formatting data are provided.
#' - [project_metadata-class]: Metadata, which records the files stored in the project directory.
#' - [project_dataset-class]: The extracted data is stored here.
#'
#' The above class objects are coordinated into a whole through the "subscript" name
#' (see [subscript-class]).
#' For example, when a command (\code{collate_data(x, ".f3_fingerid")}) requests to
#' extract the files of subscript of ".f3_fingerid", the data extraction module:
#' - from slot of \code{project_conformation},
#' get the file name (pattern string) and path of subscript of ".f3_fingerid";
#' - match the files under the path with the pattern string (i.e., get the metadata of the files),
#' then stored the metadata into slot of \code{project_metadata};
#' - from slot of \code{project_api}, get the functions of subscript of ".f3_fingerid";
#' - use these functions to read and format the data in batches;
#' - store the extracted data into slot of \code{project_dataset}.
#'
#' This class is mainly designed for extracting files under the SIRIUS project directory.
#' These files are: mainly "tables" that can be read through functions such as \code{read.table};
#' numerous and have multiple directories; need to be processed in batches.
#' SIRIUS project may alter the name and path of internal files during version changes,
#' which is in fact deadly for MCnebula2.
#' To make the data extraction module of MCnebula2 free from version issues,
#' this class object is designed to flexibly handle the extraction of internal files.
#' Most contents need to be considered by MCnebula2 developers.
#' The only thing users need to know:
#' slot of [project_dataset-class] object stores the extracted data.
#'
#' @family projects
#'
#' @slot project_version character(1). The target project version. e.g., "sirius.v4".
#' @slot project_path character(1). The target project path.
#' @slot project_conformation [project_conformation-class] object.
#' @slot project_metadata [project_metadata-class] object.
#' @slot project_api [project_api-class] object.
#' @slot project_dataset [project_dataset-class] object.
#'
#' @rdname project-class
#'
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


#' @exportMethod project_metadata
#' @aliases project_metadata
#' @description \code{metadata}: fast channel to obtain
#' the downstream slot, getter
#' for the \code{metadata} slot in sub-object
#' of \code{project_metadata} slot of the object. Equals:
#' - \code{metadata(project_metadata(object))}
#' - \code{metadata(object)}.
#' @rdname project-class
setMethod("metadata", 
          signature = c(x = "ANY"),
          function(x){
            metadata(project_metadata(x))
          })


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


#' @exportMethod get_upper_dir_subscript
#' @aliases get_upper_dir_subscript
#' @description \code{get_upper_dir_subscript}: Get the "subscript" name of the folder.
#' @param x Maybe object of class inherit [project-class].
#' @param subscript the "subscript" name of file. See [subscript-class].
#' @rdname project-class
setMethod("get_upper_dir_subscript", 
          signature = setMissing("get_upper_dir_subscript",
                                 x = "ANY",
                                 subscript = "character"),
          function(x, subscript){
            stringr::str_extract(file_api(project_conformation(x))[[ subscript ]],
                                 paste0("(?<=^|/)[^/]*(?=/", subscript, "|$)"))
          })



