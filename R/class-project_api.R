# ==========================================================================
# a class to store functions of reading or formating the target data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.project_api <- 
  setClass("project_api", 
           contains = character(),
           representation = 
             representation(methods_read = "list",
                            methods_format = "function",
                            methods_match = "list"
                            ),
           prototype = NULL
           )
# ==========================================================================
# method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
setMethod("methods_read", 
          signature = c(x = "project_api"),
          function(x){ x@methods_read })
setReplaceMethod("methods_read", 
                 signature = c(x = "project_api"),
                 function(x, value){
                   initialize(x, methods_read = value)
                 })
## ------------------------------------- 
setMethod("methods_format", 
          signature = c(x = "project_api"),
          function(x){ x@methods_format })
setReplaceMethod("methods_format", 
                 signature = c(x = "project_api"),
                 function(x, value){
                   initialize(x, methods_format = value)
                 })
## ------------------------------------- 
setMethod("methods_match", 
          signature = c(x = "project_api"),
          function(x){ x@methods_match })
setReplaceMethod("methods_match", 
                 signature = c(x = "project_api"),
                 function(x, value){
                   initialize(x, methods_match = value)
                 })
