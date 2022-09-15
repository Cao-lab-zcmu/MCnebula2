# ==========================================================================
# nebula save path
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.mcn_path <- 
  setClass("mcn_path", 
           contains = character(),
           representation = 
             representation(sirius_project = "character",
                            output_directory = "character"
                            ),
           prototype = prototype(sirius_project = ".")
  )
# ==========================================================================
# validity
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setValidity("mcn_path", 
            function(object){
              sig <- paste0(object@sirius_project, "/.format")
              content <- "%source_%name"
              if (file.exists(sig)) {
                if (identical(readLines(sig, warn = F), content)) {
                  TRUE
                } else {
                  paste0("the content of file \"", sig,
                         "\" is not identical to \"", content, "\"")
                }
              }else{
                paste0("file \"", sig, "\" not exists")
              }
            })
# ==========================================================================
# method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("mcn_path", 
          signature = c(x = "ANY"),
          function(x){ x@mcn_path })
setReplaceMethod("mcn_path", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, mcn_path = value)
                 })
## ------------------------------------- 
setMethod("sirius_project", 
          signature = c(x = "mcn_path"),
          function(x){ x@sirius_project })
setReplaceMethod("sirius_project", 
                 signature = c(x = "mcn_path"),
                 function(x, value){
                   initialize(x, sirius_project = value)
                 })
## ------------------------------------- 
setMethod("output_directory", 
          signature = c(x = "mcn_path"),
          function(x){ x@output_directory })
setReplaceMethod("output_directory", 
                 signature = c(x = "mcn_path"),
                 function(x, value){
                   initialize(x, output_directory = value)
                 })
