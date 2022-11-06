# ==========================================================================
# create reference data based on mcn_dataset
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases create_reference
#'
#' @title Establish 'specific candidate' for each 'feature'
#'
#' @description
#' According to the filtered data, whether obtained by [filter_formula()],
#' [filter_structure()] or [filter_ppcp()],
#' establishing specific candidate of each 'feature' for subsequent data filtering.
#' This step is an important intermediate link for the three part of data filtering,
#' makes the final filtered results of chemical formula, structure and classification
#' consistent.
#'
#' @details
#' \bold{Establish reference upon top candidate}
#' Suppose we predicted a potential compound represented by LC-MS/MS spectrum,
#' and obtained the candidates of chemical molecular formula,
#' structure and chemical class.
#' These candidates include both positive and negative results:
#' for chemical molecular formula and chemical structure,
#' the positive prediction was unique; for chemical class,
#' multiple positive predictions that belong to various classification were involved.
#' We did not know the exact negative and positive.
#' Normally, we ranked and filtered these according to the scores.
#' There were numerious scores, for isotopes, for mass error, for structural similarity,
#' for chemical classes...
#' Which score selected to rank candidates depends on the purpose
#' of research. Such as:
#' - To find out the chemical structure mostly be positive, ranking the candidates
#' by structural score.
#' - To determine whether the potential compound may be of a certain chemical classes,
#' ranking the candidates by the classified score.
#' 
#' Ether by [filter_formula()], [filter_structure()] or [filter_ppcp()], the
#' candidate with top score can be obtained.
#' However, for the three module (formula, structure, classes), sometimes
#' thier top score candidates were not in line with each other.
#' That is, thier top score towards different chemical molecular formulas.
#' To find out the corresponding data in other modules,
#' \code{create_reference} should be performed to establish the
#' 'specific_candidate' for subsequent filtering.
#'
#' @name create_reference-methods
#'
#' @order 1
NULL
#> NULL

#' @exportMethod create_reference
#'
#' @aliases create_reference
#'
#' @param x [mcnebula-class] object.
#' @param from character(1). "structure", "formula" or "ppcp".
#' @param subscript character(1). ".f3_fingerid", ".f2_formula" or ".f3_canopus".
#' See [subscript-class].
#' @param data data.frame. An external channel for user to specify candidate customarily.
#' Normally not used.
#' @param columns character(2) or numeric(2). Specify the key columns in the parameter
#' of data. Normally not used.
#' @param fill logical. If \code{TRUE}, run post modification.
#' Run [filter_formula(object)], and use its results to fill the data
#' \code{specific_candidate} for 'features' without specified top candidate.
#' Only useful when the data \code{specific_candidate} were
#' based on scores of chemical structure or classes, as for some 'features'
#' there may be no chemical structural
#' or classified candidates but candidates for chemical formula.
#' @param MoreArgs list. Used only \code{fill = T}. Parameters passed to [filter_formula()].
#'
#' @rdname create_reference-methods
#'
#' @examples
#' \dontrun{
#' create_reference(...)
#' }
setMethod("create_reference", 
          signature = c(x = "mcnebula", fill = "logical"),
          function(x, from, subscript, data, columns, fill, MoreArgs){
            args <- as.list(environment())
            args <- args[!names(args) %in% c("fill", "MoreArgs")]
            args <- args[ !vapply(args, is.name, T) ]
            if (length(args) == 1)
              x <- create_reference(x, "structure")
            else
              x <- do.call(create_reference, args)
            if (fill) {
              .message_info("create_reference", "fill == T",
                        "\n\tfilling missing features with filtered formula")
              if (!missing(MoreArgs))
                x <- do.call(filter_formula, c(x, MoreArgs))
              else
                x <- filter_formula(x)
              if (any(duplicated(latest(x)$.features_id)))
                stop("the filtered formula must unique in `.features_id`")
              .ref <- specific_candidate(create_reference(x, data = latest(x)))
              reference(mcn_dataset(x))[[ "specific_candidate" ]] <- 
                dplyr::distinct(dplyr::bind_rows(specific_candidate(x), .ref),
                                .features_id, .keep_all = T)
            }
            return(x)
          })
#' @exportMethod create_reference
#' @rdname create_reference-methods
setMethod("create_reference", 
          signature = setMissing("create_reference",
                                 x = "mcnebula"),
          function(x){
            create_reference(x, "structure", fill = T)
          })
#' @exportMethod create_reference
#' @rdname create_reference-methods
setMethod("create_reference", 
          signature = setMissing("create_reference",
                                 x = "mcnebula", from = "character"),
          function(x, from){
            subscript <- switch(from,
                                structure = ".f3_fingerid",
                                formula = ".f2_formula",
                                ppcp = ".f3_canopus"
            )
            create_reference(x, subscript = subscript)
          })
#' @exportMethod create_reference
#' @rdname create_reference-methods
setMethod("create_reference", 
          signature = setMissing("create_reference",
                                 x = "mcnebula",
                                 subscript = "character"),
          function(x, subscript){
            .message_info_formal("MCnebula2", "create_reference")
            data <- try(entity(dataset(mcn_dataset(x))[[ subscript ]]), silent = T)
            if (inherits(data, "try-error")) {
              stop(paste0("the specified dataset not exists. use, e.g., ",
                          "`filter_structure(x)` previously."))
            }
            if (subscript == ".f3_canopus") {
              data <- dplyr::distinct(data, .features_id, .candidates_id)
            }
            create_reference(x, data = data)
          })
#' @exportMethod create_reference
#' @rdname create_reference-methods
setMethod("create_reference", 
          signature = setMissing("create_reference",
                                 x = "mcnebula",
                                 data = "data.frame",
                                 columns = "character"),
          function(x, data, columns){
            if (length(columns) != 2)
              stop( "length(`columns`) != 2" )
            colnames(data)[which(colnames(data) == columns)] <- 
              c(".features_id", ".candidates_id")
            create_reference(x, data = data)
          })
#' @exportMethod create_reference
#' @rdname create_reference-methods
setMethod("create_reference", 
          signature = setMissing("create_reference",
                                 x = "mcnebula",
                                 data = "data.frame",
                                 columns = "integer"),
          function(x, data, columns){
            if (length(columns) != 2)
              stop( "length(`columns`) != 2" )
            colnames(data)[columns] <- c(".features_id", ".candidates_id")
            create_reference(x, data = data)
          })
#' @exportMethod create_reference
#' @rdname create_reference-methods
setMethod("create_reference", 
          signature = setMissing("create_reference",
                                 x = "mcnebula",
                                 data = "data.frame"),
          function(x, data){
            if (any( duplicated(data[[ ".features_id" ]]) ))
              stop("`.features_id` in `data` were not unique")
            fun <- methods_match(project_api(x))[[ "generate_candidates_id" ]]
            data <- format_msframe(data, fun_format = fun)
            reference(mcn_dataset(x))[[ "specific_candidate" ]] <- 
              dplyr::as_tibble(dplyr::select(data, .features_id, .candidates_id))
            return(x)
          })
