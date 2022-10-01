# ==========================================================================
# MCnebula overall object
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.mcnebula <- 
  setClass("mcnebula", 
           contains = c("project", "nebula", "export"),
           representation = 
             representation(creation_time = "character",
                            ion_mode = "character",
                            melody = "melody",
                            mcn_dataset = "mcn_dataset"
                            ),
           prototype = prototype(project_version = "sirius.v4",
                                 project_path = ".",
                                 creation_time = date(),
                                 ion_mode = "pos")
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
# ==========================================================================
# get infrustructure object
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("palette_set", 
          signature = c(x = "mcnebula"),
          function(x){
            palette_set(melody(x))
          })
setMethod("palette_gradient", 
          signature = c(x = "mcnebula"),
          function(x){
            palette_gradient(melody(x))
          })
setMethod("palette_stat", 
          signature = c(x = "mcnebula"),
          function(x){
            palette_stat(melody(x))
          })
setMethod("palette_col", 
          signature = c(x = "mcnebula"),
          function(x){
            palette_col(melody(x))
          })
setMethod("palette_label", 
          signature = c(x = "mcnebula"),
          function(x){
            palette_label(melody(x))
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
