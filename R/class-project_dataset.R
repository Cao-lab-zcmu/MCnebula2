# ==========================================================================
# class-project_dataset
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.project_dataset <- 
  setClass("project_dataset", 
           contains = "dataset",
           prototype = NULL
           )
# ==========================================================================
# method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("project_dataset", 
          signature = c(x = "ANY"),
          function(x){ x@project_dataset })
setReplaceMethod("project_dataset", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, project_dataset = value)
                 })
## ------------------------------------- 
setMethod("latest", 
          signature = c(x = "project_dataset"),
          function(x){
            tibble::as_tibble(entity(dataset(x)[[1]]))
          })
## ------------------------------------- 
setMethod("extract_rawset", 
          signature = c(x = "ANY", subscript = "character"),
          function(x, subscript){
            extract_rawset(x, subscript = subscript,
                           fun_collate = function(...){
                             stop("`subscript` not found in `dataset(project_dataset(x))`")
                           })
          })
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
