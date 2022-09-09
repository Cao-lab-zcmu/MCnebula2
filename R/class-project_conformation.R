# ==========================================================================
# project_conformation
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.project_conformation <- 
  setClass("project_conformation", 
           contains = character(),
           representation = 
             representation(file_name = "character",
                            file_api = "character",
                            attribute_name = "character"
                            ),
           prototype = NULL
           )
# ==========================================================================
# validity
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setValidity("project_conformation", 
            function(object){
              check <- 
                slots_mapply(object,
                             function(slots, names){
                               if ( is.character( names(slots) ))
                                 TRUE
                               else
                                 FALSE
                             })
              if (!any(check))
                "the colnames not matched."
              else
                TRUE
            })
# ==========================================================================
# method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("show", 
          signature = c(object = "project_conformation"),
          function(object){
            .show(object)
          })
## ------------------------------------- 
setMethod("project_conformation", 
          signature = "ANY",
          function(x){ x@project_conformation })
setReplaceMethod("project_conformation", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, project_conformation = value)
                 })
## ---------------------------------------------------------------------- 
setMethod("file_name", 
          signature = c(x = "project_conformation"),
          function(x){ x@file_name })
setReplaceMethod("file_name", 
                 signature = c(x = "project_conformation"),
                 function(x, value){
                   initialize(x, file_name = value)
                 })
## ------------------------------------- 
setMethod("file_api", 
          signature = c(x = "project_conformation"),
          function(x){ x@file_api })
setReplaceMethod("file_api", 
                 signature = c(x = "project_conformation"),
                 function(x, value){
                   initialize(x, file_api = value)
                 })
## ------------------------------------- 
setMethod("attribute_name", 
          signature = c(x = "project_conformation"),
          function(x){ x@attribute_name })
setReplaceMethod("attribute_name", 
                 signature = c(x = "project_conformation"),
                 function(x, value){
                   initialize(x, attribute_name = value)
                 })
## ------------------------------------- 
setMethod("get_upper_dir_subscript", 
          signature = setMissing("get_upper_dir_subscript",
                                 x = "ANY",
                                 subscript = "character"),
          function(x, subscript){
            stringr::str_extract(file_api(project_conformation(x))[[ subscript ]],
                                 paste0("(?<=^|/)[^/]*(?=/", subscript, "|$)"))
          })
