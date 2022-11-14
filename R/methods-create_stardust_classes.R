# ==========================================================================
# filter classification for each features, as stardust classes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases create_stardust_classes
#'
#' @title 'Inner' filter for PPCP data
#'
#' @description
#' Perform 'inner' filter for PPCP
#' (posterior probability of classification prediction) data of each 'feature',
#' then gathered as 'stardust_classes' data.
#' Run after [create_reference()].
#' Standby for next step [cross_filter_stardust()].
#'
#' @details
#' The PPCP data for each 'feature' contains the prediction of thousands of classes
#' for the potential compound (even if the chemical structure was unknown).
#' See \url{http://www.nature.com/articles/s41587-020-0740-8}
#' for details about the prediction.
#' The data contains attributes of:
#' - \code{class.name}: name of classes.
#' - \code{pp.value}: value of posterior probability.
#' - \code{hierarchy}: hierarchy of classes in the taxonomy.
#' See \url{https://jcheminf.biomedcentral.com/articles/10.1186/s13321-016-0174-y}
#' for details about hierarchy and taxonomy of chemical classification.
#' - ...
#'
#' The method [create_stardust_classes()] use these inner attributes to
#' filter classes candidates for each 'feature'.
#'
#' @name create_stardust_classes-methods
#'
#' @order 1
NULL
#> NULL

#' @exportMethod create_stardust_classes
#' @description \code{create_stardust_classes()}: get the default parameters for the method
#' \code{create_stardust_classes}.
#' @rdname create_stardust_classes-methods
setMethod("create_stardust_classes", 
          signature = setMissing("create_stardust_classes",
                                 x = "missing"),
          function(){
            list(pp.threshold = 0.5,
                 hierarchy_priority = 5:2,
                 position_isomerism = T,
                 inherit_dataset = F)
          })

#' @exportMethod create_stardust_classes
#' @description \code{create_stardust_classes(x, ...)}:
#' use the default parameters whatever 'missing'
#' while performing the method \code{create_stardust_classes}.
#' @rdname create_stardust_classes-methods
setMethod("create_stardust_classes", 
          signature = c(x = "mcnebula"),
          function(x, pp.threshold, hierarchy_priority,
                   position_isomerism, inherit_dataset){
            reCallMethod("create_stardust_classes",
                         .fresh_param(create_stardust_classes()))
          })

#' @exportMethod create_stardust_classes
#'
#' @aliases create_stardust_classes
#'
#' @param x [mcnebula-class] object.
#' @param pp.threshold numeric(1) Threshold for PPCP. \code{pp.threshold = 0.5} may
#' work well.
#'
#' @param hierarchy_priority numeric. The specified hierarchy of classes to retain.
#' The other hierarchy would be filtered out. The hierarchy:
#' - n: ...
#' - 5: Classes of Level 5.
#' - 4: Classes of Subclass.
#' - 3: Classes of Class.
#' - 2: Classes of Super Class.
#' - ...
#'
#' @param position_isomerism logical. If \code{TRUE}, use pattern match
#' to filter out all classes names contains Arabic numerals.
#' Generally, these classes describe about the position of chemical functional group,
#' which were too subtle for machine to predict from LC-MS/MS spectrum.
#'
#' @param inherit_dataset logical. If \code{TRUE}, use latest PPCP data
#' formed by [filter_ppcp()]. i.e., data of:
#' - latest(x, subscript = ".f3_canopus")
#'
#' Else, run [filter_ppcp()].
#'
#' @rdname create_stardust_classes-methods
#'
#' @examples
#' \dontrun{
#'   test <- mcn_5features
#'   
#'   ## the previous steps
#'   test1 <- filter_structure(test)
#'   test1 <- create_reference(test1)
#'   
#'   test1 <- create_stardust_classes(test1)
#'   ## see results
#'   stardust_classes(test1)
#'   ## or
#'   reference(test1)$stardust_classes
#'   ## or
#'   reference(mcn_dataset(test1))$stardust_classes
#'   
#'   ## the default parameters
#'   create_stardust_classes()
#' }
setMethod("create_stardust_classes", 
          signature = c(x = "mcnebula",
                        pp.threshold = "numeric",
                        hierarchy_priority = "numeric",
                        position_isomerism = "logical",
                        inherit_dataset = "logical"),
          function(x, pp.threshold, hierarchy_priority,
                   position_isomerism, inherit_dataset){
            .message_info_formal("MCnebula2", "create_stardust_classes")
            if (is.null(hierarchy(x)))
              x <- create_hierarchy(x)
            hierarchy <- hierarchy(x)
            if (inherit_dataset) {
              dataset <- latest(x, subscript = ".f3_canopus")
              check <- dplyr::distinct(dataset, .features_id, .candidates_id)
              if (any( duplicated(check[[ ".features_id" ]]) ))
                stop("`.candidates_id` for features in ppcp dataset were not unique")
            } else {
              x <- filter_ppcp(x, pp.threshold = pp.threshold)
              dataset <- latest(x)
            }
            dataset <-
              merge(dplyr::select(dataset, .features_id, .candidates_id,
                                  pp.value, rel.index),
                    hierarchy, by = "rel.index", all.x = T)
            dataset <- dplyr::filter(dataset, hierarchy %in% hierarchy_priority)
            if (position_isomerism) {
              dataset <- dplyr::filter(dataset, !grepl("[0-9]", class.name))
            }
            reference(mcn_dataset(x))[[ "stardust_classes" ]] <- 
              dplyr::relocate(dplyr::as_tibble(dataset), .features_id, .candidates_id)
            return(x)
          })
