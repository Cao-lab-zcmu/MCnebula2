# ==========================================================================
# MCnebula overall object
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.mcnebula <- 
  setClass("mcnebula", 
           contains = character(),
           representation = 
             representation(creation_time = "character",
                            mcn_path = "mcn_path",
                            mcn_palette = "mcn_palette",
                            project_conformation = "project_conformation",
                            project_metadata = "project_metadata",
                            project_api = "project_api",
                            sirius_version = "character"
                            ),
           prototype = prototype(creation_time = date(),
                                 sirius_version = "sirius.v4",
                                 project_conformation = new("project_conformation"))
           )
# ==========================================================================
# method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("show", 
          signature = c(object = "mcnebula"),
          function(object){
            cat( "A project of MCnebula2\n" )
          })
## ------------------------------------- 
setMethod("creation_time", 
          signature = c(x = "mcnebula"),
          function(x){ x@creation_time })
setReplaceMethod("creation_time", 
                 signature = c(x = "mcnebula"),
                 function(x, value){
                   initialize(x, creation_time = value)
                 })
## ------------------------------------- 
setMethod("sirius_version", 
          signature = c(x = "mcnebula"),
          function(x){ x@sirius_version })
setReplaceMethod("sirius_version", 
                 signature = c(x = "mcnebula"),
                 function(x, value){
                   initialize(x, sirius_version = value)
                 })
