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
setMethod("add", 
          signature = c(x = "project_metadata",
                        list = "list"),
          function(x, list){
            metadata <- c(metadata(x), list)
            metadata(x) <- list_unique_by_names(metadata)
            return(x)
          })
## ---------------------------------------------------------------------- 
setMethod("get_metadata", 
          signature = c(x = "character",
                        meta = "project_metadata",
                        api = "project_conformation",
                        path = "character",
                        db = "missing"),
          function(x, meta, api, path){
            file_name <- file_name(api)
            file_api <- file_api(api)
            if (!x %in% names(file_api) )
              stop( "`x` not descriped in `names(file_api(api))`" )
            ## ------------------------------------- 
            api <- file_api[[x]]
            api <- strsplit(api, split = "/")[[1]]
            ## ------------------------------------- 
            for (i in 1:length(api)) {
              api.x <- api[i]
              if ( any(api.x == names(metadata(meta))) )
                next
              if ( !api.x %in% names(file_name) )
                stop( "`x` not descriped in `names(file_name(api))`" )
              ## get the name of file, or the function name to get file name
              target <- file_name[[api.x]]
              ## get the target of filename
              if ( grepl("^FUN_", target) )
                target <- match.fun(target)()
              if ( i == 1 ) {
                df <- data.frame(files = list.files(path = path, pattern = target))
              } else {
                ## get the metadata of upper directory
                df <- metadata(meta)[[ api[i - 1] ]]
                upper <- paste0(apply(df, 1, paste0, collapse = "/"), "/", target)
                ## ------------------------------------- 
                .get_info("project_metadata", "get_metadata",
                          paste0(target, "(", api.x, ")"))
                df <- .list_files(path, upper)
              }
              lst <- list( df )
              names(lst) <- api.x
              meta <- add(meta, lst)
            }
            return(meta)
          })
## ------------------------------------- 
