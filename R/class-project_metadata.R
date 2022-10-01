# ==========================================================================
# a class to store the metadata of files in project directory, i.e.,
# whether the files exists.
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
