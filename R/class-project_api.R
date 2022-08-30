# ==========================================================================
# class-project_api
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.project_api <- 
  setClass("project_api", 
           contains = character(),
           representation = 
             representation(read_methods = "list",
                            format_methods = "function",
                            match_methods = "list"
                            ),
           prototype = NULL
           )
# ==========================================================================
# method
setMethod("show", 
          signature = c(object = "project_api"),
          function(object){
            .show(object)
          })
setMethod("project_api", 
          signature = c(x = "ANY"),
          function(x){ x@project_api })
setReplaceMethod("project_api", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, project_api = value)
                 })
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("read_methods", 
          signature = c(x = "project_api"),
          function(x){ x@read_methods })
setReplaceMethod("read_methods", 
                 signature = c(x = "project_api"),
                 function(x, value){
                   initialize(x, read_methods = value)
                 })
## ------------------------------------- 
setMethod("format_methods", 
          signature = c(x = "project_api"),
          function(x){ x@format_methods })
setReplaceMethod("format_methods", 
                 signature = c(x = "project_api"),
                 function(x, value){
                   initialize(x, format_methods = value)
                 })
## ------------------------------------- 
setMethod("match_methods", 
          signature = c(x = "project_api"),
          function(x){ x@match_methods })
setReplaceMethod("match_methods", 
                 signature = c(x = "project_api"),
                 function(x, value){
                   initialize(x, match_methods = value)
                 })
