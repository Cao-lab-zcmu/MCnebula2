# ==========================================================================
# use 'limma' package to conduct binary comparison between sample group
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("binary_comparison", 
          signature = setMissing("binary_comparison",
                                 x = "missing"),
          function(){
            list(formula = ~ 0 + group,
                 fun_norm = function(x) scale(x, center = T, scale = F),
                 top_coef = "all"
            )
          })
setMethod("binary_comparison", 
          signature = c(x = "ANY"),
          function(x, ..., formula, fun_norm, top_coef, contrasts){
            if (missing(...) & missing(contrasts)) {
              stop("`...` (group contrast) should be specified, ",
                   "e.g., model - control")
            } else if (missing(contrasts)) {
              contrasts <- as.character(substitute(list(...)))[-1]
            }
            do.call(binary_comparison,
                    .fresh_param(binary_comparison()))
          })
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
