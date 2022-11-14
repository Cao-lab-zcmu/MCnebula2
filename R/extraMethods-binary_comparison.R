# ==========================================================================
# use 'limma' package to conduct binary comparison between sample group
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases binary_comparison
#'
#' @title Binary comparison for 'features' quantification data
#'
#' @description
#' Use the functions in the 'limma' package for simple binary statistical analysis.
#'
#' @name binary_comparison-methods
#' 
#' @seealso [stats::model.matrix()], [limma::makeContrasts()], [limma::lmFit()],
#' [limma::eBayes()], [limma::contrasts.fit()], [limma::topTable()]...
#'
#' @order 1
NULL
#> NULL

#' @exportMethod binary_comparison
#' @description \code{binary_comparison()}:
#' get the default parameters for the method
#' \code{binary_comparison}.
#' @rdname binary_comparison-methods
setMethod("binary_comparison", 
          signature = setMissing("binary_comparison",
                                 x = "missing"),
          function(){
            list(formula = ~ 0 + group,
                 fun_norm = function(x) {
                   scale(log2(x + 1), center = T, scale = F)
                 },
                 top_coef = "all"
            )
          })

#' @exportMethod binary_comparison
#' @description \code{binary_comparison(x, ...)}:
#' use the default parameters whatever 'missing'
#' while performing the method \code{binary_comparison}.
#' @rdname binary_comparison-methods
setMethod("binary_comparison", 
          signature = c(x = "ANY"),
          function(x, ..., formula, fun_norm, top_coef, contrasts){
            if (missing(...) & missing(contrasts)) {
              stop("`...` (group contrast) should be specified, ",
                   "e.g., model - control")
            } else if (missing(contrasts)) {
              contrasts <- as.character(substitute(list(...)))[-1]
            }
            reCallMethod("binary_comparison",
                         .fresh_param(binary_comparison()))
          })

#' @exportMethod binary_comparison
#'
#' @aliases binary_comparison
#'
#' @param x [mcnebula-class] object.
#' @param ... expressions, or character strings which can be parsed to
#' expressions, specifying contrasts. See parameter of \code{...} in
#' [limma::makeContrasts()].
#'
#' @param formula formula. Passed to [model.matrix()].
#' @param fun_norm function. For normalization of 'features' quantification
#' data.
#' 
#' @param top_coef list, NULL or character(1). Specified the parameter of
#' \code{coef} in [limma::topTable()]. If \code{"all"}, all coefficient in
#' contrast matrix would be used one by one.
#'
#' @param contrasts character vector specifying contrasts.
#' See parameter \code{contrasts} in [limma::makeContrasts()].
#'
#' @rdname binary_comparison-methods
#'
#' @examples
#' \dontrun{
#'   test <- mcn_5features
#'   
#'   ## the previous steps
#'   test1 <- filter_structure(test)
#'   test1 <- create_reference(test1)
#'   test1 <- filter_formula(test1, by_reference = T)
#'   test1 <- create_features_annotation(test1)
#'   
#'   ## set up a simulated quantification data.
#'   test1 <- .simulate_quant_set(test1)
#'   ## the simulated data
#'   features_quantification(test1)
#'   sample_metadata(test1)
#'   
#'   test1 <- binary_comparison(
#'     test1, control - model,
#'     model - control, 2 * model - control
#'   )
#'   ## see results
#'   top_table(statistic_set(test1))
#'   
#'   ## the default parameters
#'   binary_comparison()
#' }
setMethod("binary_comparison", 
          signature = c(x = "ANY", formula = "formula",
                        fun_norm = "function", top_coef = "ANY",
                        contrasts = "character"),
          function(x, formula, fun_norm, top_coef, contrasts){
            .message_info_formal("MCnebula2", "binary_comparison")
            .suggest_bio_package("limma")
            .check_data(x, list(features_quantification = "features_quantification",
                                sample_metadata = "sample_metadata"), "(x) <-")
            ## design matrix
            design <- model.matrix(formula, sample_metadata(x))
            colnames(design) <- gsub("group", "", colnames(design))
            design_matrix(statistic_set(x)) <- design
            ## contrast matrix
            contrast <-
              limma::makeContrasts(contrasts = contrasts, levels = design)
            contrast_matrix(statistic_set(x)) <- contrast
            ## format data
            data <- fun_norm(.features_quantification(x))
            col <- colnames(data)
            col <- col %in% sample_metadata(x)$sample
            data <- data[, col]
            ## fit with linear model
            fit <- limma::lmFit(data, design)
            if (!formula == ~ 0 + group) {
              dataset(statistic_set(x)) <- fit
              message("identical(`formula`, ~ 0 + group) == F, ",
                             "stop downstream analysis.",
                             "\n\tuse `dataset(statistic_set(x))` ",
                             "to get `limma::limFit(data, design)` output object ",
                             "for custom 'limma' analysis.")
              return(x)
            }
            fit <- limma::contrasts.fit(fit, contrast)
            fit <- limma::eBayes(fit)
            dataset(statistic_set(x)) <- fit
            if (is.null(top_coef)) {
              return(x)
            } else if (any(top_coef == "all")) {
              top_coef <- as.list(1:ncol(contrast))
            } else if (!is.list(top_coef)) {
              top_coef <- list(top_coef)
            }
            lst <- 
              lapply(top_coef, 
                     function(i){
                       top <- limma::topTable(fit, coef = i, adjust = "BH",
                                              number = Inf)
                       top <- dplyr::mutate(top, .features_id = rownames(top))
                       dplyr::as_tibble(dplyr::relocate(top, .features_id))
                     })
            names(lst) <-
              vapply(top_coef, FUN.VALUE = "ch", USE.NAMES = F,
                     function(i){
                       paste0(colnames(contrast)[i], collapse = " & ")
                     })
            top_table(statistic_set(x)) <- lst
            return(x)
          })
