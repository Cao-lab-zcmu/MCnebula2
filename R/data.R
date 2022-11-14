#' Example object containing only five 'features'.
#'
#' This is a pre-extracted data from the SIRIUS project of example data using MCnebula2,
#' containing chemical formulae, chemical structure,
#' chemical classification candidates, etc. for five 'features'
#' (It is assumed to be a pre-processed metabolomic dataset).
#' In order to reduce the memory footprint, some of its data columns have been
#' removed, for example, the 'links' data column has been converted to character(1)
#' for the chemical structure data.
#'
#' @details
#' Data extracted via MCnebula2 package from path:
#' - \code{system.file("extdata", "raw_instance.tar.gz", package = "MCnebula2")}.
#' 
#' The MS/MS spectra were source from MoNA (MassBank of North America).
#' The 5 MS/MS spectra were randomly extracted from GNPS spectral library of that.
#' The candidates data were predicted via SIRIUS version 4 ...
#' 
#' @format ## `mcn_5features`
#' [mcnebula-class] object.
#'
#' @source The related website:
#' - <https://mona.fiehnlab.ucdavis.edu/downloads>.
#' - <https://bio.informatik.uni-jena.de/software/sirius/>
"mcn_5features"
