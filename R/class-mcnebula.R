# ==========================================================================
# MCnebula overall object
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases MCnebula2
#'
#' @title ...
#'
#' @description ...
#'
#' @param ... ...
#'
#' @details ...
#'
#' @name MCnebula2
NULL
#> NULL

#' @exportClass mcnebula
#'
#' @aliases mcnebula
#'
#' @title ...
#'
#' @description ...
#'
#' @family nebulae
# @seealso ...
#'
#' @slot creation_time ...
#' @slot ion_mode ...
#' @slot melody ...
#' @slot mcn_dataset ...
#' @slot statistic_set ...
#' @slot project_version ...
#' @slot project_path ...
#' @slot project_conformation ...
#' @slot project_metadata ...
#' @slot project_api ...
#' @slot project_dataset ...
#' @slot parent_nebula ...
#' @slot child_nebulae ...
#' @slot export_path ...
#' @slot export_name ...
#'
#' @rdname mcnebula-class
#'
#' @examples
#' \dontrun{
#' new('mcnebula', ...)
#' }
.mcnebula <- 
  setClass("mcnebula", 
           contains = c("project", "nebula", "export"),
           representation = 
             representation(creation_time = "character",
                            ion_mode = "character",
                            melody = "melody",
                            mcn_dataset = "mcn_dataset",
                            statistic_set = "statistic_set"
                            ),
           prototype = prototype(project_version = "sirius.v4",
                                 project_path = ".",
                                 creation_time = date(),
                                 ion_mode = "pos")
           )
# ==========================================================================
# method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportMethod show
#' @aliases show
#' @rdname mcnebula-class
setMethod("show", 
          signature = c(object = "mcnebula"),
          function(object){
            message( "A project of MCnebula2", ": ",
                    format(object.size(object), units = "MB"))
          })
## ------------------------------------- 
#' @exportMethod latest
#' @aliases latest
#' @description \code{latest(x, slot, subscript)}: get the data in slot
#' (\code{mcn_dataset(object)} or \code{prject_dataset(object)})
#' and format as 'tbl'.
#' @param x [mcnebula-class] object
#' @param slot Character. Slot name.
#' @param subscript numeric or character. The sequence or name for dataset in the 'list'.
#' @family latests
#' @family subscripts
#' @seealso [tibble::as_tibble()]
#' @rdname mcnebula-class
#' @examples
#' \dontrun{
#' latest(x)
#' latest(x, "project_dataset")
#' latest(x, "mcn_dataset")
#' }
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
#' @exportMethod latest
#' @description \code{latest()}: get the default parameters for the method \code{latest}.
#' @rdname mcnebula-class
setMethod("latest", 
          signature = setMissing("latest"),
          function(){
            list(slot = "mcn_dataset",
                 subscript = 1)
          })
#' @exportMethod latest
#' @description \code{latest(x, ...)}: use the default parameters whatever 'missing'
#' while performing the method \code{latest}.
#' @rdname mcnebula-class
setMethod("latest", 
          signature = c(x = "mcnebula"),
          function(x, slot, subscript){
            reCallMethod("latest", .fresh_param(latest()))
          })
## ------------------------------------- 
#' @exportMethod creation_time
#' @aliases creation_time
#' @description \code{creation_time}, \code{creation_time<-}: getter and setter
#' for the \code{creation_time} slot of the object.
#' @rdname mcnebula-class
setMethod("creation_time", 
          signature = c(x = "mcnebula"),
          function(x){ x@creation_time })
#' @exportMethod creation_time<-
#' @aliases creation_time<-
#' @param value The value for the slot.
#' @rdname mcnebula-class
setReplaceMethod("creation_time", 
                 signature = c(x = "mcnebula"),
                 function(x, value){
                   initialize(x, creation_time = value)
                 })
## ------------------------------------- 
#' @exportMethod ion_mode
#' @aliases ion_mode
#' @description \code{ion_mode}, \code{ion_mode<-}: getter and setter
#' for the \code{ion_mode} slot of the object.
#' @rdname mcnebula-class
setMethod("ion_mode", 
          signature = c(x = "mcnebula"),
          function(x){ x@ion_mode })
#' @exportMethod ion_mode<-
#' @aliases ion_mode<-
#' @param value The value for the slot.
#' @rdname mcnebula-class
setReplaceMethod("ion_mode", 
                 signature = c(x = "mcnebula"),
                 function(x, value){
                   initialize(x, ion_mode = value)
                 })
# ==========================================================================
# get infrustructure object
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportMethod palette_set
#' @aliases palette_set
#' @description  \code{palette_set}, \code{palette_gradient}, \code{palette_stat},
#' \code{palette_col}: fast channel to obtain the downstream slot.
#' For \code{palette_set}, e.g., getter for the \code{palette_set} slot in sub-object
#' of \code{melody} slot of the object. \code{palette_set(melody(object))}
#' equals to \code{palette_set(object)}.
#' @rdname mcnebula-class
setMethod("palette_set", 
          signature = c(x = "mcnebula"),
          function(x){
            palette_set(melody(x))
          })
#' @exportMethod palette_gradient
#' @aliases palette_gradient
#' @rdname mcnebula-class
setMethod("palette_gradient", 
          signature = c(x = "mcnebula"),
          function(x){
            palette_gradient(melody(x))
          })
#' @exportMethod palette_stat
#' @aliases palette_stat
#' @rdname mcnebula-class
setMethod("palette_stat", 
          signature = c(x = "mcnebula"),
          function(x){
            palette_stat(melody(x))
          })
#' @exportMethod palette_col
#' @aliases palette_col
#' @rdname mcnebula-class
setMethod("palette_col", 
          signature = c(x = "mcnebula"),
          function(x){
            palette_col(melody(x))
          })
#' @exportMethod palette_label
#' @aliases palette_label
#' @rdname mcnebula-class
setMethod("palette_label", 
          signature = c(x = "mcnebula"),
          function(x){
            palette_label(melody(x))
          })
## ------------------------------------- 
#' @exportMethod reference
#' @aliases reference
#' @description  \code{reference}: fast channel to obtain
#' the downstream slot, getter
#' for the \code{reference} slot in sub-object
#' of \code{mcn_dataset} slot of the object.
#' \code{reference(mcn_dataset(object))}
#' equals to \code{reference(object)}.
#' @rdname mcnebula-class
setMethod("reference", 
          signature = c(x = "mcnebula"),
          function(x){
            reference(mcn_dataset(x))
          })
#' @exportMethod specific_candidate
#' @aliases specific_candidate
#' @description \code{specific_candidate}, \code{hierarchy}, \code{stardust_classes},
#' \code{nebula_index}, \code{spectral_similarity}, \code{features_annotation},
#' \code{features_quantification}, \code{sample_metadata}:
#' fast channel to obtain data (mostly 'tbl' or 'data.frame')
#' inside the downstream slot ('list'). e.g., getter
#' for the data named \code{specific_candidate} in
#' \code{reference} slot (a 'list') in sub-object
#' of \code{mcn_dataset} slot of the object.
#' \code{reference(mcn_dataset(object))$specific_candidate}
#' equals to \code{specific_candidate(object)}.
#' @rdname mcnebula-class
setMethod("specific_candidate", 
          signature = c(x = "mcnebula"),
          function(x){
            reference(x)[[ "specific_candidate" ]]
          })
#' @exportMethod hierarchy
#' @aliases hierarchy
#' @rdname mcnebula-class
setMethod("hierarchy", 
          signature = c(x = "mcnebula"),
          function(x){
            reference(x)[[ "hierarchy" ]]
          })
#' @exportMethod stardust_classes
#' @aliases stardust_classes
#' @rdname mcnebula-class
setMethod("stardust_classes", 
          signature = c(x = "mcnebula"),
          function(x){
            reference(x)[[ "stardust_classes" ]]
          })
#' @exportMethod nebula_index
#' @aliases nebula_index
#' @rdname mcnebula-class
setMethod("nebula_index", 
          signature = c(x = "mcnebula"),
          function(x){
            reference(x)[[ "nebula_index" ]]
          })
#' @exportMethod spectral_similarity
#' @aliases spectral_similarity
#' @rdname mcnebula-class
setMethod("spectral_similarity", 
          signature = c(x = "mcnebula"),
          function(x){
            reference(x)[[ "spectral_similarity" ]]
          })
#' @exportMethod spectral_similarity<-
#' @aliases spectral_similarity<-
#' @description  \code{spectral_similarity<-}, \code{features_quantification<-},
#' \code{sample_metadata<-}: fast channel to replace
#' data (mostly 'tbl' or 'data.frame') inside the downstream slot ('list'). e.g., setter
#' for the data named \code{spectral_similarity} in
#' \code{reference} slot (a 'list') in sub-object
#' of \code{mcn_dataset} slot of the object.
#' \code{reference(mcn_dataset(object))$spectral_similarity<-}
#' similar to \code{spectral_similarity(object)<-}.
#' But the latter not only replace and also validate.
#' @rdname mcnebula-class
setReplaceMethod("spectral_similarity", 
                 signature = c(x = "mcnebula"),
                 function(x, value){
                   .check_columns(value, list(".features_id1", ".features_id2",
                                              "similarity"),
                                  "spectral_similarity")
                   reference(mcn_dataset(x))$spectral_similarity <- value
                   return(x)
                 })
#' @exportMethod features_annotation
#' @aliases features_annotation
#' @rdname mcnebula-class
setMethod("features_annotation", 
          signature = c(x = "mcnebula"),
          function(x){
            reference(x)[[ "features_annotation" ]]
          })
#' @exportMethod features_quantification
#' @aliases features_quantification
#' @rdname mcnebula-class
setMethod("features_quantification", 
          signature = c(x = "mcnebula"),
          function(x){
            reference(x)[[ "features_quantification" ]]
          })
.features_quantification <- 
  function(x){
    data <- features_quantification(x)
    .features_id <- data$.features_id
    data$.features_id <- NULL
    data <- as.matrix(data)
    rownames(data) <- .features_id
    data
  }
#' @importFrom dplyr select
#' @exportMethod features_quantification
#' @aliases features_quantification
#' @rdname mcnebula-class
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
#' @exportMethod sample_metadata
#' @aliases sample_metadata
#' @rdname mcnebula-class
setMethod("sample_metadata", 
          signature = c(x = "mcnebula"),
          function(x){
            reference(x)[[ "sample_metadata" ]]
          })
#' @exportMethod sample_metadata<-
#' @aliases sample_metadata<-
#' @rdname mcnebula-class
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
#' @exportMethod classification
#' @aliases classification
#' @description  \code{classification}: fast channel to obtain
#' data deeply inside the downstream slot ('list'), getter
#' for the data named \code{".canopus"} in
#' \code{dataset} slot (a 'list') in sub-object
#' of \code{project_dataset} slot of the object.
#' \code{tibble::as_tibble(entity(dataset(project_dataset(object))$.canopus))}
#' equals to \code{classification(object)}.
#' @rdname mcnebula-class
setMethod("classification", 
          signature = c(x = "mcnebula"),
          function(x){
            res <- dataset(project_dataset(x))[[ ".canopus" ]]
            if (is.null(res))
              return()
            else
              return(dplyr::as_tibble(entity(res)))
          })
