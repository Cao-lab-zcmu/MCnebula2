# ==========================================================================
# compute spectral similarity of features within each nebula
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @importFrom pbapply pbapply
#' @importFrom pbapply pblapply
#' @importFrom pbapply pbmapply
#' @exportMethod compute_spectral_similarity
setMethod("compute_spectral_similarity", 
          signature = setMissing("compute_spectral_similarity",
                                 x = "mcnebula"),
          function(x){
            compute_spectral_similarity(x, T, F)
          })
setMethod("compute_spectral_similarity", 
          signature = setMissing("compute_spectral_similarity",
                                 x = "mcnebula",
                                 within_nebula = "logical"),
          function(x, within_nebula){
            compute_spectral_similarity(x, within_nebula, F)
          })
setMethod("compute_spectral_similarity", 
          signature = setMissing("compute_spectral_similarity",
                                 x = "mcnebula",
                                 recompute = "logical"),
          function(x, recompute){
            compute_spectral_similarity(x, T, recompute)
          })
setMethod("compute_spectral_similarity", 
          signature = setMissing("compute_spectral_similarity",
                                 sp1 = "lightSpectrum",
                                 sp2 = "lightSpectrum"),
          function(sp1, sp2){
            compareSpectra(sp1, sp2)
          })
setMethod("compute_spectral_similarity", 
          signature = setMissing("compute_spectral_similarity",
                                 sp1 = "data.frame",
                                 sp2 = "data.frame"),
          function(sp1, sp2){
            if (ncol(sp1) == 2 & ncol(sp2) == 2) {
              .get_info("compute_spectral_similarity", "ncol(sp) == 2",
                        "\n\tguess columns are c('mz', 'intensity')")
              colnames(sp1) <- colnames(sp2) <- c("mz", "intensity")
            } else {
              .get_info("compute_spectral_similarity", "ncol(sp) > 2",
                        "\n\t select columns of c('mz', 'intensity')"
              )
            }
            sp1 <- new("lightSpectrum", mz = sp1[[ "mz" ]],
                       intensity = sp1[[ "intensity" ]])
            sp2 <- new("lightSpectrum", mz = sp2[[ "mz" ]],
                       intensity = sp2[[ "intensity" ]])
            compareSpectra(sp1, sp2)
          })
setMethod("compute_spectral_similarity", 
          signature = setMissing("compute_spectral_similarity",
                                 x = "mcnebula",
                                 within_nebula = "logical",
                                 recompute = "logical"),
          function(x, within_nebula, recompute){
            .get_info_formal("MCnebula2", "compute_spectral_similarity")
            .check_data(x, list(nebula_index = "create_nebula_index"))
            if (!is.null(spectral_similarity(x))) {
              if (recompute) {
                .get_info("compute_spectral_similarity", "recompute == T")
              } else {
                .get_info("compute_spectral_similarity", "recompute == F",
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
            .get_info("compute_spectral_similarity", "compareSpectra")
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
