# ==========================================================================
# generic and methods of `compareSpectra` stripped from package of 
# MSnbase and ProtGenerics
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## from ProtGenerics
setGeneric("compareSpectra", function(x, y, ...)
    standardGeneric("compareSpectra"))
setGeneric("mz", function(object, ...) standardGeneric("mz"))
setGeneric("intensity", function(object, ...) standardGeneric("intensity"))

## from MSnbase and modified
setClass("lightSpectrum",
         representation =
           representation(mz = "numeric",
                          intensity = "numeric"),
         prototype =
           prototype(mz = numeric(),
                     intensity = numeric())
)
setMethod("mz", "lightSpectrum",
          function(object) object@mz)
setMethod("intensity", "lightSpectrum",
          function(object) object@intensity)
setMethod("compareSpectra", c("lightSpectrum", "lightSpectrum"),
          function(x, y) {
            binnedSpectra <- bin_Spectra(x, y)
            do.call(dotproduct, binnedSpectra)
          })

# ==========================================================================
# function
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

bin_Spectra <-
  function(object1, object2, binSize = 1L,
           breaks = seq(floor(min(c(mz(object1), mz(object2)))),
                        ceiling(max(c(mz(object1), mz(object2)))),
                        by = binSize)) {
    breaks <- .fix_breaks(breaks, range(mz(object1), mz(object2)))
    list(bin_Spectrum(object1, breaks = breaks),
         bin_Spectrum(object2, breaks = breaks))
}

bin_Spectrum <-
  function(object, binSize = 1L, breaks, fun = sum) {
    ints <- .bin_values(object@intensity, object@mz, binSize = binSize,
                        breaks = breaks, fun = fun)
    return(ints)
}

#' The function aggregates `x` for `toBin` falling into bins defined
#' by `breaks` using the `fun` function.
#'
#' @details
#' This is a combination of the code from the former bin_Spectrum.
#'
#' @param x `numeric` with the values that should be binned.
#'
#' @param toBin `numeric`, same length than `x`, with values to be used for the
#'     binning.
#'
#' @param binSize `numeric(1)` with the size of the bins.
#'
#' @param breaks `numeric` defining the breaks/bins.
#'
#' @param fun `function` to be used to aggregate values of `x` falling into the
#'     bins defined by `breaks`.
#'
#' @return `list` with elements `x` and `mids` being the aggregated values
#'     of `x` for values in `toBin` falling within each bin and the bin mid
#'     points. --been modified, only return `x`.
#'
#' @noRd

.bin_values <-
  function(x, toBin, binSize = 1L, breaks, fun) {
    breaks <- .fix_breaks(breaks, range(toBin))
    nbrks <- length(breaks)
    idx <- findInterval(toBin, breaks)
    ## Ensure that indices are within breaks.
    idx[which(idx < 1L)] <- 1L
    idx[which(idx >= nbrks)] <- nbrks - 1L

    ints <- double(nbrks - 1L)
    ints[unique(idx)] <- unlist(lapply(base::split(x, idx), fun),
                                use.names = FALSE)
    return(ints)
}

#' Simple function to ensure that breaks (for binning) are span al leat the
#' expected range.
#'
#' @param brks `numeric` with *breaks* such as calculated by `seq`.
#'
#' @param rng `numeric(2)` with the range of original numeric values on which
#'     the breaks were calculated.
#'
#' @noRd

.fix_breaks <-
  function(brks, rng) {
    ## Assuming breaks being sorted.
    if (brks[length(brks)] <= rng[2]) {
      brks <- c(brks, max((rng[2] + 1e-6),
                          brks[length(brks)] + mean(diff(brks))))
    }
    brks
}

#' calculate the dot product between two vectors
#'
#' Stein, S. E., and Scott, D. R. (1994).
#' Optimization and testing of mass spectral library search algorithms for
#' compound identification.
#' Journal of the American Society for Mass Spectrometry, 5(9), 859-866.
#' doi: https://doi.org/10.1016/1044-0305(94)87009-8
#'
#' Lam, H., Deutsch, E. W., Eddes, J. S., Eng, J. K., King, N., Stein, S. E.
#' and Aebersold, R. (2007)
#' Development and validation of a spectral library searching method for peptide
#' identification from MS/MS.
#' Proteomics, 7: 655-667.
#' doi: https://doi.org/10.1002/pmic.200600625
#'
#' @param x double
#' @param y double
#' @return double, length == 1
#' @noRd

dotproduct <- function(x, y) {
  as.vector(x %*% y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
}

