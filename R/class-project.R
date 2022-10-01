# ==========================================================================
# a class to store information about files in target dir, and as well,
# to read these files and save data in slots.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
setMethod("project_version", 
          signature = c(x = "ANY"),
          function(x){ x@project_version })
setReplaceMethod("project_version", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, project_version = value)
                 })
setMethod("project_path", 
          signature = c(x = "ANY"),
          function(x){ x@project_path })
setReplaceMethod("project_path", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, project_path = value)
                 })
## ------------------------------------- 
setMethod("file_name", 
          signature = c(x = "ANY"),
          function(x){
            file_name(project_conformation(x))
          })
setMethod("file_api", 
          signature = c(x = "ANY"),
          function(x){
            file_api(project_conformation(x))
          })
setMethod("attribute_name", 
          signature = c(x = "ANY"),
          function(x){
            attribute_name(project_conformation(x))
          })
## ------------------------------------- 
setMethod("metadata", 
          signature = c(x = "ANY"),
          function(x){
            metadata(project_metadata(x))
          })
## ------------------------------------- 
setMethod("methods_read", 
          signature = c(x = "ANY"),
          function(x){
            methods_read(project_api(x))
          })
setMethod("methods_format", 
          signature = c(x = "ANY"),
          function(x){
            methods_format(project_api(x))
          })
setMethod("methods_match", 
          signature = c(x = "ANY"),
          function(x){
            methods_match(project_api(x))
          })
setMethod("match.candidates_id", 
          signature = c(x = "ANY"),
          function(x){
            methods_match(project_api(x))[[ "match.candidates_id" ]]
          })
setMethod("match.features_id", 
          signature = c(x = "ANY"),
          function(x){
            methods_match(project_api(x))[[ "match.features_id" ]]
          })
## ---------------------------------------------------------------------- 
setMethod("get_upper_dir_subscript", 
          signature = setMissing("get_upper_dir_subscript",
                                 x = "ANY",
                                 subscript = "character"),
          function(x, subscript){
            stringr::str_extract(file_api(project_conformation(x))[[ subscript ]],
                                 paste0("(?<=^|/)[^/]*(?=/", subscript, "|$)"))
          })
## ---------------------------------------------------------------------- 

