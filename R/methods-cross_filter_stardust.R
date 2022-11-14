# ==========================================================================
# across attributes of each other features to filter classes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases cross_filter_stardust
#'
#' @title 'Cross' filter for 'stardust_classes' data
#'
#' @description
#' 'Cross' filter for 'stardust_classes' data.
#' Use 'features_annotation' data and 'stardust_classes' data for
#' chemical classes filtering. Run after [create_stardust_classes()].
#' Methods \code{cross_filter_stardust} are integration of the following three method:
#' - \code{cross_filter_quantity}
#' - \code{cross_filter_score}
#' - \code{cross_filter_identical}
#'
#' @details
#' Compared to the chemical class filtering within PPCP data by [create_stardust_classes()],
#' the filtering within 'stardust_classes' data by [cross_filter_stardust()] is
#' fundamentally different.
#'
#' - For [create_stardust_classes()],
#' the PPCP data belongs to each 'feature'. When performing the filtering,
#' only simple threshold conditions or absolute conditions
#' are set to filter the chemical classes;
#' there is no crossover between the different attributes and
#' no crossover between the 'features'.
#' Therefore, we consider this as 'inner' filtering.
#' - For [cross_filter_stardust()],
#' the data of the chemical classes and their classified 'features', i.e.
#' 'stardust_classes' data, were combined and then grouped upon the chemical classes.
#' After grouping, each chemical class has a certain quantity of "features".
#' When filtering, statistics may be performed on 'features' data within a group;
#' statistics may be performed on these data in conjunction with 'features_annotation' data;
#' and statistics may be performed to compare groups with each other.
#' As its crossover, we consider this as 'cross' filtering.
#' 
#' @param x [mcnebula-class] object.
#' @param min_number numeric(1). Value in (1, ).
#' For classified 'features' of chemical classes, minimum quantity.
#' 
#' @param max_ratio numeric(1). Value in (0, 1].
#' For classified 'features' of chemical classes,
#' maximum proportion: the 'features' quantity versus
#' all 'features' (unique) quantity of all classes.
#' 
#' @param types character.
#' The target attributes for Goodness assessment. See details.
#' There can be plural ones.
#' 
#' @param cutoff numeric. 
#' For Goodness assessment of target attributes.
#' The size of the value depends on the target attribute.
#' Note, the \code{cutoff} must be 'vector' the same length as \code{types}.
#'
#' @param tolerance numeric. Value in (0, 1).
#' For Goodness assessment of target attributes. The thresholds of Goodness.
#' Note, the \code{tolerance} must be 'vector' the same length as \code{types}.
#' 
#' @param hierarchy_range numeric(2).
#' The hierarchy range of chemical classification passed for similarity assessment
#' of chemical classes. The hierarchy:
#' - 10: ...
#' - n: ...
#' - 5: Classes of Level 5.
#' - 4: Classes of Subclass.
#' - 3: Classes of Class.
#' - 2: Classes of Super Class.
#' - 1: ...
#' - 0: ...
#' 
#' @param identical_factor numeric(1). Value in (0, 1).
#' Threshold value for classes similarity assessment. 
#'
#' @name cross_filter_stardust-methods
#'
#' @order 1
#'
#' @examples
#' \dontrun{
#'   test <- mcn_5features
#'   
#'   ## the previous steps
#'   test1 <- filter_structure(test)
#'   test1 <- create_reference(test1)
#'   test1 <- filter_formula(test1, by_reference = T)
#'   test1 <- create_stardust_classes(test1)
#'   test1 <- create_features_annotation(test1)
#'   
#'   ## the default parameters
#'   cross_filter_stardust()
#'   # This is a simulated dataset with only 5 'features',
#'   # so the default parameters are meaningless for it.
#'   
#'   # Note that real datasets often contain thousands of "features"
#'   # and the following 'min_number' and 'max_ratio' parameter values are not suitable.
#'   test1 <- cross_filter_stardust(
#'     test1,
#'     min_number = 2,
#'     max_ratio = 1
#'   )
#'   ## see results
#'   stardust_classes(test1)
#'   ## or
#'   reference(test1)$stardust_classes
#'   ## or
#'   reference(mcn_dataset(test1))$stardust_classes
#'   
#'   e1 <- stardust_classes(test1)
#'   
#'   ## see the filtered classes
#'   backtrack_stardust(test1)
#'   
#'   ## reset the 'stardust_classes'
#'   test1 <- create_stardust_classes(test1)
#'   
#'   ## customized filtering
#'   # Note that real datasets often contain thousands of "features"
#'   # and the following 'min_number' and 'max_ratio' parameter values are not suitable.
#'   test1 <- cross_filter_quantity(test1, min_number = 2, max_ratio = 1)
#'   test1 <- cross_filter_score(test1,
#'     types = "tani.score",
#'     cutoff = 0.3,
#'     tolerance = 0.6
#'   )
#'   test1 <- cross_filter_identical(
#'     test1,
#'     hierarchy_range = c(3, 11),
#'     identical_factor = 0.7
#'   )
#'   e2 <- stardust_classes(test1)
#'   
#'   identical(e1, e2)
#'   
#'   ## reset
#'   test1 <- create_stardust_classes(test1)
#'   ## targeted plural attributes
#'   test1 <- cross_filter_stardust(
#'     test1,
#'     min_number = 2,
#'     max_ratio = 1,
#'     types = c("tani.score", "csi.score"),
#'     cutoff = c(0.3, -150),
#'     tolerance = c(0.6, 0.3)
#'   )
#' }
NULL
#> NULL

#' @exportMethod cross_filter_stardust
#' @description \code{cross_filter_stardust()}:
#' get the default parameters for the method
#' \code{cross_filter_stardust}.
#' @rdname cross_filter_stardust-methods
setMethod("cross_filter_stardust", 
          signature = setMissing("cross_filter_stardust",
                                 x = "missing"),
          function(){
            list(min_number = 30,
                 max_ratio = 0.1,
                 types = "tani.score",
                 cutoff = 0.3,
                 tolerance = 0.6,
                 hierarchy_range = c(3, 11),
                 identical_factor = 0.7
            )
          })

#' @exportMethod cross_filter_stardust
#' @description \code{cross_filter_stardust(x, ...)}:
#' use the default parameters whatever 'missing'
#' while performing the method \code{cross_filter_stardust}.
#'
#' @rdname cross_filter_stardust-methods
setMethod("cross_filter_stardust", 
          signature = c(x = "mcnebula"),
          function(x, min_number, max_ratio,
                   types, cutoff, tolerance,
                   hierarchy_range, identical_factor){
            .message_info_formal("MCnebula2", "cross_filter_stardust")
            .check_data(x, list(stardust_classes = "create_stardust_classes"))
            new_args <- .fresh_param(cross_filter_stardust())
            methods <- c("cross_filter_quantity", "cross_filter_score",
                         "cross_filter_identical")
            for (i in methods) {
              args <- new_args[names(new_args) %in% formalArgs(i)]
              new_args[[ "x" ]] <- do.call(match.fun(i), args)
            }
            backtrack(mcn_dataset(new_args[[ "x" ]]))[[ "stardust_classes" ]] <- 
              stardust_classes(x)
            return(new_args[[ "x" ]])
          })

#' @exportMethod cross_filter_quantity
#'
#' @aliases cross_filter_quantity
#'
#' @description
#' \code{cross_filter_quantity}:
#' Filter chemical classes in 'stardust_classes' data according to:
#' - Absolute quantity: the classified 'features' of the class.
#' - Relative proportion: the classified 'features' of the class
#' comparing with all features of all classes.
#'
#' @details
#' \bold{Cross_filter_quantity}
#' Set 'features' quantity limitation for each group.
#' The groups with too many 'features' or too few 'features' would be filtered out.
#' This means the chemical class would be filtered out.
#' These thresholds are about:
#' - Minimum quantity: the 'features'.
#' - Maximum proportion: the 'features' quantity versus
#' all 'features' (unique) quantity of all groups.
#' 
#' The purpose of this step is to filter out chemical classes that have
#' too large or too subtle a conceptual scope. For example, 'Organic compounds',
#' which covers almost all compounds that can be detected in metabolomics data,
#' is too large in scope to be of any help to our biological research.
#' The setting of parameters is not absolute, and there is no optimal solution.
#' Users can draw up thresholds according to the necessity of the study.
#'
#' @rdname cross_filter_stardust-methods
#'
setMethod("cross_filter_quantity", 
          signature = setMissing("cross_filter_quantity",
                                 x = "mcnebula", min_number = "numeric",
                                 max_ratio = "numeric"),
          function(x, min_number, max_ratio){
            .message_info("cross_filter_stardust", "quantity")
            if (min_number < 1) {
              stop( "`min_number` must be a numeric greater or equal to 1" )
            }
            if (!(max_ratio <= 1 & max_ratio > 0)) {
              stop( "`max_ratio` must be a numeric within (0, 1]" )
            }
            sum <- length( unique(stardust_classes(x)[[ ".features_id" ]]) )
            set <- split(stardust_classes(x), ~ rel.index)
            set <- lapply(set, function(df)
                          if (nrow(df) >= min_number &
                              nrow(df) / sum <= max_ratio) df)
            reference(mcn_dataset(x))[[ "stardust_classes" ]] <- 
              dplyr::as_tibble(data.table::rbindlist(set))
            return(x)
          })

#' @exportMethod cross_filter_score
#'
#' @aliases cross_filter_score
#'
#' @description
#' \code{cross_filter_score}
#' Filter chemical classes in 'stardust_classes' data according to the attributes
#' in 'features_annotation' data.
#' Set cut-off value of attributes for all 'features', then inspect overall
#' satisfaction of the classified 'features' of the class.
#'
#' @details
#' \bold{Cross_filter_score}
#' This step associate 'stardust_classes' data with 'features_annotation' data.
#' For each group, the Goodness assessment is performed
#' for each target attribute (continuous attribute, generally be a scoring
#' attribute of compound identification, such as 'tani.score'). If the group met all the
#' expected Goodness, the chemical class would be retained; otherwise,
#' the chemical class would be filtered out.
#' The Goodness (G) related with the 'features' within the group:
#' - n: the quantity of 'features' of which target attributes satisfied with the cut-off.
#' - N: the quantity of all 'features'.
#'
#' The Goodness: G = n / N.
#' 
#' The assessment of Goodness is related to the parameters of \code{cutoff}
#' and \code{tolerance}:
#' - Expected Goodness, i.e. value of \code{tolerance}.
#' - Actual Goodness, related to parameter \code{cutoff}. G = n / N.
#'
#' Goodness assessment can be given to plural target attributes.
#' Note that the chemical class would retained only if
#' it passed the Goodness assessment of all target attributes.
#' 
#' The main purpose of this step is to filter out those chemical classes with
#' too many 'features' of low structural identification.
#'
#' @rdname cross_filter_stardust-methods
#'
setMethod("cross_filter_score", 
          signature = setMissing("cross_filter_score",
                                 x = "mcnebula", types = "character",
                                 cutoff = "numeric", tolerance = "numeric"),
          function(x, types, cutoff, tolerance){
            .message_info("cross_filter_stardust", "score")
            .check_data(x, list(features_annotation = "create_features_annotation"))
            set <- split(stardust_classes(x), f = ~ rel.index)
            features <- features_annotation(x)
            res <- mapply(types, cutoff, tolerance,
                          SIMPLIFY = F, USE.NAMES = F,
                          FUN = function(type, cutoff, tolerance){
                            if (!is.numeric(features[[ type ]]))
                              stop("the columns of `types` were not numeric")
                            express <- parse(text = type)
                            ref <- dplyr::filter(features, eval(express) >= cutoff)
                            ref <- ref[[ ".features_id" ]]
                            vapply(set, FUN.VALUE = T, USE.NAMES = F,
                                   function(data){
                                     ref <- ref[ref %in% data[[ ".features_id" ]]]
                                     if (length(ref) / nrow(data) >= tolerance)
                                       return(T)
                                     else
                                       return(F)
                                   })
                          })
            if (length(res) == 1) {
              set <- set[unlist(res)]
            } else {
              logic  <- res[[1]]
              for (i in res[2:length(res)]) {
                logic <- logic & i
              }
              set <- set[logic]
            }
            reference(mcn_dataset(x))[[ "stardust_classes" ]] <- 
              dplyr::as_tibble(data.table::rbindlist(set))
            return(x)
          })

#' @exportMethod cross_filter_identical
#'
#' @aliases cross_filter_identical
#'
#' @description
#' \code{cross_filter_identical}
#' Filter chemical classes in 'stardust_classes' data by comparing the classified 'features'.
#'
#' @details
#' \bold{Cross_filter_identical}
#' A similarity assessment of chemical classes.
#' Set a hierarchical range for chemical classification and let groups (
#' each group, i.e. a chemical class with its classified 'features')
#' within this range be compared for similarity to each other. For two groups,
#' if the classified 'features' almost identical to each other, the chemical
#' class represented by one of the groups would be discarded.
#' The assessment of identical degree of two groups (A and B):
#' - x: ratio of the classified 'features' of A belonging to B
#' - y: ratio of the classified 'features' of B belonging to A
#' - i: value of parameter \code{identical_factor}
#'
#' If x > i and y > i, the two groups would be considered as identical.
#' Then the group with fewer 'features' would be discarded.
#' 
#' The purpose of this step is to filter out classes that may incorporate
#' each other and are similar in scope. The in silico prediection approach may not be able
#' to distinguish which class the potential compound belongs to from the LC-MS/MS spectra.
#' 
#' @rdname cross_filter_stardust-methods
#'
setMethod("cross_filter_identical", 
          signature = setMissing("cross_filter_identical",
                                 x = "mcnebula", hierarchy_range = "numeric",
                                 identical_factor = "numeric"),
          function(x, hierarchy_range, identical_factor){
            .message_info("cross_filter_stardust", "identical")
            set <- dplyr::filter(stardust_classes(x),
                                 hierarchy %in% hierarchy_range)
            set <- split(set, f = ~ rel.index)
            ids <- lapply(set, `[[`, ".features_id")
            groups <- combn(1:length(ids), 2, simplify = F)
            discard <- lapply(groups,
                          function(group){
                            if (any( ids[group[1]] %in% ids[group[2]] )) {
                              p <- mapply(c(1, 2), c(2, 1),
                                          SIMPLIFY = F,
                                          FUN = function(x, y){
                                            table(ids[group[x]] %in% ids[group[y]])[[ "TRUE" ]]
                                          })
                              if (p[[1]] > identical_factor & p[[2]] > identical_factor) {
                                if (length(ids[group[1]]) < length(ids[group[2]]))
                                  return(group[1])
                                else
                                  return(group[2])
                              }
                            }
                          })
            discard_index <-
              unique( data.table::rbindlist( set[unlist(discard)] )[[ "rel.index" ]]
              )
            reference(mcn_dataset(x))[[ "stardust_classes" ]] <- 
              dplyr::filter(stardust_classes(x), !rel.index %in% discard_index)
            return(x)
          })
