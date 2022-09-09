# ==========================================================================
# project_metadata
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
setMethod("show", 
          signature = c(object = "project_metadata"),
          function(object){
            .show(object)
          })
## ------------------------------------- 
setMethod("project_metadata", 
          signature = c(x = "ANY"),
          function(x){ x@project_metadata })
setReplaceMethod("project_metadata", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, project_metadata = value)
                 })
## ------------------------------------- 
setMethod("latest", 
          signature = c(x = "project_metadata"),
          function(x){
            tibble::as_tibble(metadata(x)[[1]])
          })
## ---------------------------------------------------------------------- 
setMethod("metadata", 
          signature = c(x = "project_metadata"),
          function(x){ x@metadata })
setReplaceMethod("metadata", 
                 signature = c(x = "project_metadata"),
                 function(x, value){
                   initialize(x, metadata = value)
                 })
## ---------------------------------------------------------------------- 
setMethod("add_dataset", 
          signature = c(x = "project_metadata",
                        list = "list"),
          function(x, list){
            metadata <- c(list, metadata(x))
            metadata(x) <- list_unique_by_names(metadata)
            return(x)
          })
## ---------------------------------------------------------------------- 
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
## ---------------------------------------------------------------------- 
setMethod("get_metadata", 
          signature = c(x = "ANY", subscript = "character"),
          function(x, subscript){
            exits_meta <- names( metadata(project_metadata(x)) )
            if (!subscript %in% exits_meta) {
              project_metadata(x) <-
                get_metadata(subscript = subscript,
                             project_metadata = project_metadata(x),
                             project_conformation = project_conformation(x),
                             path = sirius_project(mcn_path(x))
                )
            }
            return(x)
          })
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
                .get_info("project_metadata", "get_metadata",
                          paste0(target, "(", sub, ")"))
                df <- .list_files(path, upper, target)
              }
              lst <- list( df )
              names(lst) <- sub
              project_metadata <- add_dataset(project_metadata, lst)
            }
            return(project_metadata)
          })
## ------------------------------------- 
