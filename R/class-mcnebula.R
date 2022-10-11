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
            message( "A project of MCnebula2", ": ",
                    format(object.size(object), units = "MB"))
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
setReplaceMethod("spectral_similarity", 
                 signature = c(x = "mcnebula"),
                 function(x, value){
                   .check_columns(value, list(".features_id1", ".features_id2",
                                              "similarity"),
                                  "spectral_similarity")
                   reference(mcn_dataset(x))$spectral_similarity <- value
                   return(x)
                 })
setMethod("features_annotation", 
          signature = c(x = "mcnebula"),
          function(x){
            reference(x)[[ "features_annotation" ]]
          })
setMethod("features_quantification", 
          signature = c(x = "mcnebula"),
          function(x){
            reference(x)[[ "features_quantification" ]]
          })
#' @importFrom dplyr select
setReplaceMethod("features_quantification", 
                 signature = c(x = "mcnebula"),
                 function(x, value){
                   .check_columns(value, list(".features_id"),
                                  "features_quantification")
                   .check_type(dplyr::select(value, -.features_id),
                               "numeric", "features_quantification")
                   reference(mcn_dataset(x))$features_quantification <- value
                   return(x)
                 })
setMethod("sample_metadata", 
          signature = c(x = "mcnebula"),
          function(x){
            reference(x)[[ "sample_metadata" ]]
          })
setReplaceMethod("sample_metadata", 
                 signature = c(x = "mcnebula"),
                 function(x, value){
                   .check_data(x, list(features_quantification =
                                       "features_quantification"), "(x) <-")
                   .check_columns(value, list("sample", "group"), "sample_metadata")
                   if (any(!value$sample %in% colnames(features_quantification(x))))
                     stop(paste0("the name in 'sample' column in 'sample_metadata' ",
                                 "must all involved in 'features_quantification'"))
                   reference(mcn_dataset(x))$sample_metadata <- value
                   return(x)
                 })
