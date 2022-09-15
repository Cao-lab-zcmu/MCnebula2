# ==========================================================================
# MCnebula overall object
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.mcnebula <- 
  setClass("mcnebula", 
           contains = c("nebula"),
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
            latest(x, "mcn_dataset", 1)
          })
setMethod("latest", 
          signature = c(x = "mcnebula", slot = "character",
                        subscript = "missing"),
          function(x, slot){
            latest(x, slot, 1)
          })
setMethod("latest", 
          signature = c(x = "mcnebula", slot = "missing",
                        subscript = "character"),
          function(x, subscript){
            latest(x, "mcn_dataset", subscript)
          })
setMethod("latest", 
          signature = c(x = "mcnebula", slot = "character",
                        subscript = "ANY"),
          function(x, slot, subscript){
            fun <- match.fun(slot)
            res <- dataset(fun(x))
            if (length(res) == 0)
              return()
            res <- res[[ subscript ]]
            if (is.null(res))
              return()
            else
              return(tibble::as_tibble(entity(res)))
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
# ==========================================================================
# get infrustructure object
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("palette_set", 
          signature = c(x = "mcnebula"),
          function(x){
            palette_set(mcn_palette(x))
          })
setMethod("palette_stat", 
          signature = c(x = "mcnebula"),
          function(x){
            palette_stat(mcn_palette(x))
          })
setMethod("palette_ppcp", 
          signature = c(x = "mcnebula"),
          function(x){
            palette_ppcp(mcn_palette(x))
          })
setMethod("palette_label", 
          signature = c(x = "mcnebula"),
          function(x){
            palette_label(mcn_palette(x))
          })
## ------------------------------------- 
setMethod("sirius_project", 
          signature = c(x = "mcnebula"),
          function(x){
            sirius_project(mcn_path(x))
          })
setMethod("output_directory", 
          signature = c(x = "mcnebula"),
          function(x){
            output_directory(mcn_path(x))
          })
## ------------------------------------- 
setMethod("file_name", 
          signature = c(x = "mcnebula"),
          function(x){
            file_name(project_conformation(x))
          })
setMethod("file_api", 
          signature = c(x = "mcnebula"),
          function(x){
            file_api(project_conformation(x))
          })
setMethod("attribute_name", 
          signature = c(x = "mcnebula"),
          function(x){
            attribute_name(project_conformation(x))
          })
## ------------------------------------- 
setMethod("metadata", 
          signature = c(x = "mcnebula"),
          function(x){
            metadata(project_metadata(x))
          })
## ------------------------------------- 
setMethod("methods_read", 
          signature = c(x = "mcnebula"),
          function(x){
            methods_read(project_api(x))
          })
setMethod("methods_format", 
          signature = c(x = "mcnebula"),
          function(x){
            methods_format(project_api(x))
          })
setMethod("methods_match", 
          signature = c(x = "mcnebula"),
          function(x){
            methods_match(project_api(x))
          })
setMethod("match.candidates_id", 
          signature = c(x = "mcnebula"),
          function(x){
            methods_match(project_api(x))[[ "match.candidates_id" ]]
          })
setMethod("match.features_id", 
          signature = c(x = "mcnebula"),
          function(x){
            methods_match(project_api(x))[[ "match.features_id" ]]
          })
## ------------------------------------- 
setMethod("reference", 
          signature = c(x = "mcnebula"),
          function(x){
            reference(mcn_dataset(x))
          })
setMethod("specific_candidate", 
          signature = c(x = "mcnebula"),
          function(x){
            reference(x)[[ "specific_candidate" ]]
          })
## ------------------------------------- 
setMethod("classification", 
          signature = c(x = "mcnebula"),
          function(x){
            res <- dataset(project_dataset(x))[[ ".canopus" ]]
            if (is.null(res))
              return()
            else
              return(dplyr::as_tibble(entity(res)))
          })
setMethod("hierarchy", 
          signature = c(x = "mcnebula"),
          function(x){
            reference(x)[[ "hierarchy" ]]
          })
setMethod("stardust_classes", 
          signature = c(x = "mcnebula"),
          function(x){
            reference(x)[[ "stardust_classes" ]]
          })
setMethod("nebula_index", 
          signature = c(x = "mcnebula"),
          function(x){
            reference(x)[[ "nebula_index" ]]
          })
setMethod("spectral_similarity", 
          signature = c(x = "mcnebula"),
          function(x){
            reference(x)[[ "spectral_similarity" ]]
          })
setMethod("features_annotation", 
          signature = c(x = "mcnebula"),
          function(x){
            reference(x)[[ "features_annotation" ]]
          })
