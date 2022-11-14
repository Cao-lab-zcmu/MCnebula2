# ==========================================================================
# a class to store the metadata of files in project directory, i.e.,
# whether the files exists.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportClass project_metadata
#'
#' @aliases project_metadata
#'
#' @title Metadata of files
#'
#' @description This is a class object used to store metadata of files.
#' See [project-class] for joint application with other related classes.
#'
#' @note The class is not for normal use of the package.
#'
#' @family projects
#'
#' @slot metadata a list with names of [subscript-class].
#' Each element of the list is a data.frame.
#'
#' @rdname project_metadata-class
#'
.project_metadata <- 
  setClass("project_metadata", 
           contains = character(),
           representation = 
             representation(metadata = "list"
                            ),
           prototype = NULL
           )

# ==========================================================================
# validity
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setValidity("project_metadata", 
            function(object){
              if ( is.character(names(object@metadata)) )
                TRUE
              else
                FALSE
            })

# ==========================================================================
# method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportMethod show
#' @aliases show
#' @rdname project_metadata-class
setMethod("show", 
          signature = c(object = "project_metadata"),
          function(object){
            .show(object)
          })

## ------------------------------------- 
#' @exportMethod project_metadata
#' @aliases project_metadata
#' @description \code{project_metadata}, \code{project_metadata<-}: getter and setter
#' for the \code{project_metadata} slot of the object.
#' @rdname project_metadata-class
setMethod("project_metadata", 
          signature = c(x = "ANY"),
          function(x){ x@project_metadata })

#' @exportMethod project_metadata<-
#' @aliases project_metadata<-
#' @param value The value for the slot.
#' @rdname project_metadata-class
setReplaceMethod("project_metadata", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, project_metadata = value)
                 })

## ------------------------------------- 
#' @exportMethod latest
#' @aliases latest
#' @description \code{latest}: get the first data in \code{metadata} slot and
#' format as "tbl".
#' @family latests
#' @rdname project_metadata-class
setMethod("latest", 
          signature = c(x = "project_metadata"),
          function(x){
            tibble::as_tibble(metadata(x)[[1]])
          })

## ---------------------------------------------------------------------- 
#' @exportMethod metadata
#' @aliases metadata
#' @description \code{metadata}, \code{metadata<-}: getter and setter
#' for the \code{metadata} slot of the object.
#' @rdname project_metadata-class
setMethod("metadata", 
          signature = c(x = "project_metadata"),
          function(x){ x@metadata })

#' @exportMethod metadata<-
#' @aliases metadata<-
#' @param value The value for the slot.
#' @rdname project_metadata-class
setReplaceMethod("metadata", 
                 signature = c(x = "project_metadata"),
                 function(x, value){
                   initialize(x, metadata = value)
                 })

## ---------------------------------------------------------------------- 
#' @exportMethod add_dataset
#' @aliases add_dataset
#' @description \code{add_dataset}: add the list into slot \code{metadata}.
#' @param list a list (with names) of metadata (data.frame) with names.
#' @rdname project_metadata-class
setMethod("add_dataset", 
          signature = c(x = "project_metadata",
                        list = "list"),
          function(x, list){
            metadata <- c(list, metadata(x))
            metadata(x) <- list_unique_by_names(metadata)
            return(x)
          })

#' @exportMethod extract_metadata
#' @aliases extract_metadata
#' @description \code{extract_metadata}: use "subscript" to extract metadata from an
#' object with slot \code{project_metadata},
#' and then return it as a new \code{project_metadata}.
#' @param subscript see [subscript-class].
#' @rdname project_metadata-class
setMethod("extract_metadata", 
          signature = c(x = "ANY", subscript = "character"),
          function(x, subscript){
            x <- get_metadata(x, subscript = subscript)
            path.set <- metadata(project_metadata(x))[[ subscript ]]
            ## build project_metadata
            path.set <- list(path.set)
            names(path.set) <- subscript
            new("project_metadata", metadata = path.set)
          })

#' @exportMethod get_metadata
#' @aliases get_metadata
#' @description \code{get_metadata}: for an object with slot of \code{project_metadata},
#' get the metadata of files of specified "subscript", then return the object.
#' @param project_metadata [project_metadata-class] object.
#' Used by \code{get_metadata()}. If 'missing', the slot \code{project_metadata} inside
#' the object will be used.
#' @param project_conformation [project_conformation-class] object.
#' Used by \code{get_metadata()}. If 'missing', the slot \code{project_conformation} inside
#' the object will be used.
#' @param path character. The path of the project directory (generally, SIRIUS project).
#' If 'missing', the slot \code{project_path} inside the object will be used.
#' @rdname project_metadata-class
setMethod("get_metadata", 
          signature = c(x = "ANY", subscript = "character"),
          function(x, subscript){
            exits_meta <- names( metadata(project_metadata(x)) )
            if (!subscript %in% exits_meta) {
              project_metadata(x) <-
                get_metadata(subscript = subscript,
                             project_metadata = project_metadata(x),
                             project_conformation = project_conformation(x),
                             path = project_path(x)
                )
            }
            return(x)
          })

#' @exportMethod get_metadata
#' @rdname project_metadata-class
setMethod("get_metadata", 
          signature = setMissing("get_metadata",
                                 subscript = "character",
                                 project_metadata = "project_metadata",
                                 project_conformation = "project_conformation",
                                 path = "character"),
          function(subscript, project_metadata, project_conformation, path){
            file_name <- file_name(project_conformation)
            file_api <- file_api(project_conformation)
            if (!subscript %in% names(file_api) )
              stop( "`subscript` not descriped in `names(file_api(project_conformation))`" )
            ## ------------------------------------- 
            api <- file_api[[ subscript ]]
            api <- strsplit(api, split = "/")[[1]]
            ## ------------------------------------- 
            for (i in 1:length(api)) {
              sub <- api[i]
              if ( any(sub == names(metadata(project_metadata))) )
                next
              if ( !sub %in% names(file_name) )
                stop( "`subscript` not descriped in `names(file_name(project_conformation))`" )
              ## get the name of file, or the function name to get file name
              target <- file_name[[sub]]
              ## get the target of filename
              if ( grepl("^FUN_", target) )
                target <- match.fun(target)()
              if ( i == 1 ) {
                df <- data.frame(files = list.files(path = path, pattern = target))
              } else {
                ## get the metadata of upper directory
                df <- metadata(project_metadata)[[ api[i - 1] ]]
                upper <- paste0(apply(df, 1, paste0, collapse = "/"))
                ## ------------------------------------- 
                .message_info("project_metadata", "get_metadata",
                          paste0(target, "(", sub, ")"))
                df <- .list_files(path, upper, target)
              }
              lst <- list( df )
              names(lst) <- sub
              project_metadata <- add_dataset(project_metadata, lst)
            }
            return(project_metadata)
          })

