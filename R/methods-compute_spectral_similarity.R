# ==========================================================================
# compute spectral similarity of features within each nebula
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases compute_spectral_similarity
#'
#' @title Compute MS2 spectral similarity
#'
#' @description These are methods stripped from [MSnbase::compareSpectra].
#' The unnecessary parts were removed, [compute_spectral_similarity()] only
#' calculate the 'dotproduct' for two spectra and get the similarity value.
#' It allows faster results for batch comparisons.
#'
#' @name compute_spectral_similarity-methods
#' 
#' @seealso [MSnbase::compareSpectra()].
#'
#' @order 1
NULL
#> NULL

#' @importFrom pbapply pbapply
#' @importFrom pbapply pblapply
#' @importFrom pbapply pbmapply
#' @exportMethod compute_spectral_similarity
#' @exportMethod compute_spectral_similarity
#' @description \code{compute_spectral_similarity()}: get the default parameters for the method
#' \code{compute_spectral_similarity}.
#' @rdname compute_spectral_similarity-methods
setMethod("compute_spectral_similarity", 
          signature = setMissing("compute_spectral_similarity",
                                 x = "missing"),
          function(){
            list(within_nebula = T,
                 recompute = F
            )
          })

#' @exportMethod compute_spectral_similarity
#' @description \code{compute_spectral_similarity(x, ...)}:
#' use the default parameters whatever 'missing'
#' while performing the method \code{compute_spectral_similarity}.
#' @rdname compute_spectral_similarity-methods
setMethod("compute_spectral_similarity", 
          signature = c(x = "mcnebula"),
          function(x, within_nebula, recompute, sp1, sp2){
            reCallMethod("compute_spectral_similarity",
                         .fresh_param(compute_spectral_similarity()))
          })

#' @exportMethod compute_spectral_similarity
#' @rdname compute_spectral_similarity-methods
setMethod("compute_spectral_similarity", 
          signature = setMissing("compute_spectral_similarity",
                                 sp1 = "lightSpectrum",
                                 sp2 = "lightSpectrum"),
          function(sp1, sp2){
            compareSpectra(sp1, sp2)
          })

#' @exportMethod compute_spectral_similarity
#' @rdname compute_spectral_similarity-methods
setMethod("compute_spectral_similarity", 
          signature = setMissing("compute_spectral_similarity",
                                 sp1 = "data.frame",
                                 sp2 = "data.frame"),
          function(sp1, sp2){
            if (ncol(sp1) == 2 & ncol(sp2) == 2) {
              .message_info("compute_spectral_similarity", "ncol(sp) == 2",
                        "\n\tguess columns are c('mz', 'intensity')")
              colnames(sp1) <- colnames(sp2) <- c("mz", "intensity")
            } else {
              .message_info("compute_spectral_similarity", "ncol(sp) > 2",
                        "\n\t select columns of c('mz', 'intensity')"
              )
            }
            sp1 <- new("lightSpectrum", mz = sp1[[ "mz" ]],
                       intensity = sp1[[ "intensity" ]])
            sp2 <- new("lightSpectrum", mz = sp2[[ "mz" ]],
                       intensity = sp2[[ "intensity" ]])
            compareSpectra(sp1, sp2)
          })

#' @exportMethod compute_spectral_similarity
#'
#' @aliases compute_spectral_similarity
#'
#' @param x [mcnebula-class] object.
#' @param within_nebula logical. If \code{TRUE}, 
#' only 'features' that exist in a Child-Nebula are compared
#' for spectral similarity. Data of 'nebula_index' (\code{nebula_index(object)})
#' would be used for assigning and combining the 'features' of comparison.
#'
#' @param recompute logical. If \code{TRUE}, discard the existing data in the object,
#' and recompute the spectral similarity.
#'
#' @param sp1 data.frame. An additional channel for comparing two spectrum.
#' Contains 'mz' and 'intensity' for spectral comparison.
#' @param sp2 data.frame. An additional channel for comparing two spectrum.
#' Contains 'mz' and 'intensity' for spectral comparison.
#'
#' @rdname compute_spectral_similarity-methods
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
#'   test1 <- cross_filter_stardust(test1, 2, 1)
#'   test1 <- create_nebula_index(test1)
#'   
#'   test1 <- compute_spectral_similarity(test1)
#'   ## see results
#'   spectral_similarity(test1)
#'   ## or
#'   reference(test1)$spectral_similarity
#'   ## or
#'   reference(mcn_dataset(test1))$spectral_similarity
#'   
#'   ## compare the two spectra individually
#'   spectra <- latest(test1, "project_dataset", ".f3_spectra")
#'   data1 <- dplyr::select(
#'     dplyr::filter(spectra, .features_id == "gnps1537"),
#'     mz, int.
#'   )
#'   data2 <- dplyr::select(
#'     dplyr::filter(spectra, .features_id == "gnps1539"),
#'     mz, int.
#'   )
#'   e1 <- compute_spectral_similarity(sp1 = data1, sp2 = data2)
#'   e1
#'   # [1] 0.7670297
#'   
#'   ## MSnbase
#'   if (requireNamespace("MSnbase")) {
#'     MSnbase::compareSpectra
#'     spec1 <- new("Spectrum2", mz = data1$mz, intensity = data1$int.)
#'     spec2 <- new("Spectrum2", mz = data2$mz, intensity = data2$int.)
#'     e2 <- MSnbase::compareSpectra(spec1, spec2, fun = "dotproduct")
#'     identical(e1, e2)
#'   }
#' }
setMethod("compute_spectral_similarity", 
          signature = setMissing("compute_spectral_similarity",
                                 x = "mcnebula",
                                 within_nebula = "logical",
                                 recompute = "logical"),
          function(x, within_nebula, recompute){
            .message_info_formal("MCnebula2", "compute_spectral_similarity")
            .check_data(x, list(nebula_index = "create_nebula_index"))
            if (!is.null(spectral_similarity(x))) {
              if (recompute) {
                .message_info("compute_spectral_similarity", "recompute == T")
              } else {
                .message_info("compute_spectral_similarity", "recompute == F",
                          "\n\tdata already existed.")
                return(x)
              }
            }
            ## collate spectra
            x <- collate_data(x, ".f3_spectra", reference = specific_candidate(x))
            all_spectra <- dplyr::select(latest(x, "project_dataset", ".f3_spectra"),
                                         .features_id, mz, rel.int.)
            lst_lightSpectrum <- lapply(split(all_spectra, ~.features_id),
                                        function(df){
                                          new("lightSpectrum", mz = df[[ "mz" ]],
                                              intensity = df[[ "rel.int." ]]
                                          )
                                        })
            ## combn and compute dotproduct of spectra
            if (within_nebula) {
              combn <- lapply(split(nebula_index(x), ~ rel.index),
                              function(df){
                                if (nrow(df) < 2)
                                  return()
                                data.frame(t(combn(df[[ ".features_id" ]], 2)))
                              })
              combn <- t(apply(data.table::rbindlist(combn), 1, sort))
            } else {
              combn <- t(combn(unique(nebula_index(x)[[ ".features_id" ]]), 2))
            }
            combn <- dplyr::rename(dplyr::distinct(data.frame(combn)),
                                   .features_id1 = 1, .features_id2 = 2)
            .message_info("compute_spectral_similarity", "compareSpectra")
            combn[[ "similarity" ]] <-
              pbapply::pbapply(combn, 1, function(vec){
                                 compareSpectra(lst_lightSpectrum[[ vec[1] ]],
                                                lst_lightSpectrum[[ vec[2] ]])
                                   })
            ## compute ionMass difference
            if (!is.null(features_annotation(x))) {
              mz <- dplyr::select(features_annotation(x),
                                  .features_id, mz, rt.secound)
              combn <- merge(combn,
                             dplyr::rename(mz, .features_id2 = .features_id,
                                           mz2 = mz, rt.secound2 = rt.secound),
                             by = ".features_id2", all.x = T)
              combn <- merge(combn,
                             dplyr::rename(mz, .features_id1 = .features_id,
                                           mz1 = mz, rt.secound1 = rt.secound),
                             by = ".features_id1", all.x = T)
              combn <-
                dplyr::select(dplyr::mutate(combn,
                                            mass_difference = mz2 - mz1,
                                            rt.min_difference = 
                                              round((rt.secound2 - rt.secound1) / 60, 2)
                                            ),
                              -mz1, -mz2, -rt.secound1, -rt.secound2)
            }
            reference(mcn_dataset(x))[[ "spectral_similarity" ]] <-
              dplyr::as_tibble(combn)
            return(x)
          })
