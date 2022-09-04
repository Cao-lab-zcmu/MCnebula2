# ==========================================================================
# MCnebula overall object
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.mcnebula <- 
  setClass("mcnebula", 
           contains = character(),
           representation = 
             representation(creation_time = "character",
                            ion_mode = "character",
                            mcn_path = "mcn_path",
                            mcn_palette = "mcn_palette",
                            project_conformation = "project_conformation",
                            project_metadata = "project_metadata",
                            project_api = "project_api",
                            project_dataset = "project_dataset",
                            mcn_dataset = "mcn_dataset",
                            sirius_version = "character"
                            ),
           prototype = prototype(creation_time = date(),
                                 ion_mode = "pos",
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
setMethod("latest", 
          signature = c(x = "mcnebula"),
          function(x){
            tibble::as_tibble(entity(dataset(mcn_dataset(x))[[1]]))
          })
setMethod("latest", 
          signature = c(x = "mcnebula", switch = "logical"),
          function(x, switch){
            if (switch)
              tibble::as_tibble(entity(dataset(project_dataset(x))[[1]]))
            else
              latest(x)
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
setMethod("ion_mode", 
          signature = c(x = "mcnebula"),
          function(x){ x@ion_mode })
setReplaceMethod("ion_mode", 
                 signature = c(x = "mcnebula"),
                 function(x, value){
                   initialize(x, ion_mode = value)
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
