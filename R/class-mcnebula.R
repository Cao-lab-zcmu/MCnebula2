# ==========================================================================
# MCnebula2 overall object
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases MCnebula2
#'
#' @title Overview of MCnebula2
#'
#' @description 
#'
#' MCnebula2 was used for metabonomics data analysis.
#' It is written in the S4 system of object-oriented programming,
#' and starts with a "class", namely "mcnebula".
#' The whole process takes the "mcnebula" as the operating object to obtain visual
#' results or operating objects.
#'
#' Most methods of MCnebula2 are S4 methods and have the characteristics of
#' parameterized polymorphism, that is, different functions will be used for
#' processing according to different parameters passed to the same method.
#'
#' MCnebula workflow is a complete metabolomics data analysis process,
#' including initial data preprocessing (data format conversion, feature detection),
#' compound identification based on MS/MS,
#' statistical analysis,
#' compound structure and chemical class focusing,
#' multi-level data visualization, output report, etc.
#'
#' It should be noted that the MCnebula2 R package currently cannot realize
#' the entire analysis process of MCnebula workflow.
#' If users want to complete the entire workflow,
#' other software beyond the R console
#' (for example, the MSconvert tool of proteowizard is used for data format conversion,
#' which is a tool widely applicable to metabonomics and proteomics) should be used.
#' This is a pity, but we will gradually integrate all parts of the workflow into this R package
#' in the future to achieve one-stop analysis.
#'
#' The analysis process in R is integrated into the following methods:
#'
#' - [initialize_mcnebula()]
#' - [filter_structure()]
#' - [create_reference()]
#' - [filter_formula()]
#' - [create_stardust_classes()]
#' - [create_features_annotation()]
#' - [cross_filter_stardust()]
#' - [create_nebula_index()]
#' - [compute_spectral_similarity()]
#' - [create_parent_nebula()]
#' - [create_child_nebulae()]
#' - [create_parent_layout()]
#' - [create_child_layouts()]
#' - [activate_nebulae()]
#' - [visualize()]
#' - [binary_comparison()]
#' - ...
#'
#' @details
#'
#' \bold{Overall.} We know that the analysis of untargeted LC-MS/MS dataset generally
#' begin with feature detection.
#' It detects 'peaks' as features in MS1 (MASS level 1) data.
#' Each feature may represents a compound, and assigned with MS2 (MASS level 2) spectra.
#' The MS2 spectra was used to find out the compound identity.
#' The difficulty lies in annotating these features to discover their compound identity,
#' mining out meaningful information, so as to serve further biological research.
#' In addition, the un-targeted LC-MS/MS dataset is general a huge dataset,
#' which leads to time-consuming analysis of the whole process.
#' Herein, a classified visualization method, called MCnebula,
#' was used for addressing these difficulty.
#'
#' MCnebula utilizes the state-of-the-art computer prediction technology,
#' SIRIUS workflow (SIRIUS, ZODIAC, CSI:fingerID, CANOPUS),
#' for compound formula prediction, structure retrieve and classification prediction
#' (\url{https://bio.informatik.uni-jena.de/software/sirius/}).
#' MCnebula integrates an abundance-based classes (ABC) selection algorithm
#' into features annotation:
#' depending on the user,
#' MCnebula focuses chemical classes with more or less features in the dataset
#' (the abundance of classes), visualizes them, and displays the features they involved;
#' these classes can be dominant structural classes or sub-structural classes.
#' With MCnebula, we can switch from untargeted to targeted analysis,
#' focusing precisely on the compound or chemical class of interest to the researcher.
#'
#' \bold{MCnebula2.} The MCnebula2 package itself does not contain any part of 
#' molecular formula prediction, structure prediction and chemical prediction of compounds,
#' so the accuracy of these parts is not involved.
#' MCnebula2 performs downstream analysis by extracting the prediction data from SIRIUS project.
#' The core of MCnebula2 is its chemical filtering algorithm, called ABC selection algorithm.
#'
#' \bold{Chemical structure and formula.} To explain the ABC selection algorithm in detail,
#' we need to start with MS/MS spectral analysis and identification of compounds:
#' The analysis of MS/MS spectrum is a process of inference and prediction.
#' For example, we speculate the composition of elements based on the molecular weight of MS1;
#' combined with the possible fragmentation pattern of MS2 spectrum,
#' we speculate the potential molecular formula of a compound;
#' finally, we look for the exact compound from the compound structure database.
#' Sometimes, this process is full of uncertainty,
#' because there are too many factors that affect the reliability of MS/MS data
#' and the correctness of inference.
#' It can be assumed that there are complex candidates
#' for the potential chemical molecular formula,
#' chemical structure and chemical class behind MS/MS spectrum.
#' Suppose we have these data of candidates now,
#' MCnebula2 extracted these candidates and obtained the unique
#' molecular formula and chemical structure for each MS/MS spectrum
#' based on the highest score of
#' chemical structure prediction; in this process, as most algorithms do,
#' we make a choice based on the score,
#' and only select the result of highest score.
#'
#' The chemical formula and structure candidates can obtain by methods:
#'
#' - [filter_formula()]
#' - [filter_structure()]
#'
#' In order to obtain the best (maybe), corresponding and unique chemical formula
#' and structure from complex candidates, an important intermediate link:
#'
#' - [create_reference()]
#'
#' Above, we talked about chemical molecular formula,
#' chemical structural formula and chemical classes.
#' We obtained the unique chemical molecular formula and chemical structure formula
#' for reference by scoring and ranking.
#' But for chemical classes, we can't adopt such a simple way to get things done.
#' 
#' \bold{Chemical classification.} Chemical classification is a complex system.
#' Here, we only discuss the structure based chemotaxonomy system,
#' because the MS/MS spectrum is more indicative of the structure of compounds
#' than biological activity and other information.
#'
#' According to the division of the overall structure and local structure of compounds,
#' we can call the structural characteristics as the dominant structure and substructure.
#' (\url{https://jcheminf.biomedcentral.com/articles/10.1186/s13321-016-0174-y}).
#' Correspondingly, in the chemical classification system,
#' we can not only classify according to the dominant structure,
#' but also classify according to the substructure.
#' The chemical classification based on the dominant structure of compounds is easy to understand,
#' because we generally define it in this way.
#' For example, we will classify Taxifolin as "flavones", not "phenols",
#' although its local structure has a substructure of "phenol".
#'
#' We hope to classify a compound by its dominant structure rather than substructure,
#' because such classify is more concise and contains more information.
#' However, in the process of MS/MS spectral analysis,
#' we sometimes can only make chemical classification based on the substructure of compounds,
#' which may be due to: uncertainty in the process of structural analysis;
#' it may be an unknown compound; MS/MS spectral fragment information is insufficient.
#' In this case, it is necessary for us to classify the compounds with the aid of
#' substructure information, otherwise we have no knowledge of the compounds
#' for which we cannot obtain dominant structure information.
#'
#' Above, we discussed the complex chemical classification
#' for the substructure and dominant structure of compounds.
#' We must also be clear about the complexity of another aspect of chemotaxonomy,
#' i.e., the hierarchy of classification.
#' This is easy to understand. For example, "Flavones" belongs to its superior, "Flavonoids";
#' its next higher level, "Phynylpropanoids and polyketides";
#' the further upward classification is "organic compounds".
#'
#' \bold{ABC selection.}
#' The above section discusses the inferential prediction of individual MS/MS spectrum.
#' In the un-targeted LC-MS/MS dataset, each feature has a corresponding MS/MS spectrum,
#' and there are thousands of features in total.
#' The ABC selection algorithm regards all features as a whole,
#' examines the number and abundance of features of each chemical classification
#' (classification at different levels, classification of substructure and dominant structure),
#' and then selects representative classes
#' (mainly screening the classes according to the number or abundance range of features)
#' to serve the subsequent analysis.
#' The core methods for ABC selection algorithm are:
#'
#' - [create_stardust_classes()]
#' - [cross_filter_stardust()]
#' - [create_nebula_index()]
#'
#' @name ABSTRACT-MCnebula2
NULL
#> NULL

#' @export mcnebula
#' @exportClass mcnebula
#'
#' @aliases mcnebula
#'
#' @title Overall object class of MCnebula2
#'
#' @description For analysis of MCnebula2, all data stored in this class object,
#' all main methods performed with this object.
#'
#' @family nebulae
#'
#' @slot creation_time character(1).
#' @slot ion_mode character(1).
#' @slot melody [melody-class] object.
#' @slot mcn_dataset [mcn_dataset-class] object.
#' @slot statistic_set [statistic_set-class] object.
#' @slot ... Slots inherit from [project-class], [nebula-class], [export-class].
#'
#' @rdname mcnebula-class
#'
#' @examples
#' \dontrun{
#' new('mcnebula', ...)
#' mcnebula()
#' }
mcnebula <- 
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
#' of \code{melody} slot of the object. Equals:
#' - \code{palette_set(melody(object))}
#' - \code{palette_set(object)}.
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
#' of \code{mcn_dataset} slot of the object. Equals:
#' - \code{reference(mcn_dataset(object))} 
#' - \code{reference(object)}
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
#' of \code{mcn_dataset} slot of the object. Equals:
#' - \code{reference(mcn_dataset(object))$specific_candidate}
#' - \code{specific_candidate(object)}.
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
#' of \code{mcn_dataset} slot of the object. Similar:
#' - \code{reference(mcn_dataset(object))$spectral_similarity<-}
#' - \code{spectral_similarity(object)<-}.
#'
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
#' of \code{project_dataset} slot of the object. Equals:
#' - \code{tibble::as_tibble(entity(dataset(project_dataset(object))$.canopus))}
#' - \code{classification(object)}.
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
