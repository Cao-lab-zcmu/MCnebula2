# ==========================================================================
# use XCMS to perform Feature Dectection
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# the following classes would import while used...
# @importClassesFrom MSnbase OnDiskMSnExp
# @importClassesFrom MSnbase MSpectra
# @importClassesFrom S4Vectors DFrame
# @importClassesFrom xcms XCMSnExp

# setClass("OnDiskMSnExp")
# setClass("MSpectra")
# setClass("DFrame")
# setClass("XCMSnExp")

#' @exportClass detectFlow
#'
#' @aliases detectFlow
#'
#' @title Steps in sequence to perform Feature Detection
#'
#' @description A class inherits from [layerSet-class] to store steps for
#' Feature Detection.
#'
#' @seealso [layerSet-class], [mcmass-class]
#'
#' @slot layers list. a list of objects [command-class].
#'
#' @rdname detectFlow-class
#'
#' @examples
#' \dontrun{
#' new('detectFlow', ...)
#' }
.detectFlow <- 
  setClass("detectFlow", 
    contains = "layerSet",
    representation = representation(),
    prototype = NULL
  )

#' @exportMethod show_layers
#' @description See \code{show_layers} in [ggset-class].
#' @rdname detectFlow-class
setMethod("show_layers", 
  signature = c(x = "detectFlow"),
  function(x){
    selectMethod("show_layers", "ggset")(x)
  })

#' @exportClass toBeEval
#'
#' @aliases toBeEval
#'
#' @description character(1) to be eval while perform [run_lcms()].
#' @rdname detectFlow-class
toBeEval <- setClass("toBeEval", contains = c("character"), prototype = "pro_data(x)")

#' @exportClass mcmass
#'
#' @aliases mcmass
#'
#' @title storage of XCMS processed data of 'feature detection'
#'
#' @description This class provides a process template for pre-processing
#' non-targeted mass spectrometry data with package \code{xcms}.
#' In default, the steps of Feature Detection were refer to:
#' \link{https://github.com/DorresteinLaboratory/XCMS3_FeatureBasedMN}
#'
#' @slot raw_data [OnDiskMSnExp-class] object.
#' @slot pro_data [XCMSnExp-class] object.
#' @slot sample_metadata data.frame. User supplied data about the mass data
#' files to process.
#' @slot features_defination [DFrame-class] object. Non user supplied data.
#' See \code{xcms::featureDefinitions}.
#' @slot features_quantification data.frame. Non user supplied data.
#' Peak area were used to quantify the feature level.
#' @slot ms2_spectra [MSpectra-class] object. Non user supplied data.
#' See \code{xcms::filteredMs2Spectra}.
#' @slot parameter_set list. Passed to repalace the parameters in slot
#' \code{detectFlow} of [mcmass-class] for performing [run_lcms()].
#' @slot detectFlow [detectFlow-class] object.
#'
#' @rdname mcmass-class
#'
#' @examples
#' \dontrun{
#' new('mcmass', ...)
#' }
.mcmass <- 
  suppressWarnings(setClass("mcmass", 
    contains = character(),
    representation = representation(
      raw_data = "OnDiskMSnExp",
      pro_data = "XCMSnExp",
      sample_metadata = "data.frame",
      features_defination = "DFrame",
      features_quantification = "data.frame",
      ms2_spectra = "MSpectra",
      parameter_set = "list",
      detectFlow = "detectFlow"
      ),
    prototype = prototype(detectFlow = .detectFlow())
  ))

# ==========================================================================
# methods (getter or setter)
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

setGeneric("raw_data", 
  function(x) standardGeneric("raw_data"))
setGeneric("raw_data<-", 
  function(x, value) standardGeneric("raw_data<-"))

#' @exportMethod raw_data
#' @aliases raw_data
#' @description \code{raw_data}, \code{raw_data<-}: getter and setter
#' for the \code{raw_data} slot of the object.
#' @rdname mcmass-class
setMethod("raw_data", 
  signature = c(x = "mcmass"),
  function(x){
    x@raw_data
  })

setGeneric("pro_data", 
  function(x) standardGeneric("pro_data"))
setGeneric("pro_data<-", 
  function(x, value) standardGeneric("pro_data<-"))

#' @exportMethod pro_data
#' @aliases pro_data
#' @description \code{pro_data}, \code{pro_data<-}: getter and setter
#' for the \code{pro_data} slot of the object.
#' @rdname mcmass-class
setMethod("pro_data", 
  signature = c(x = "mcmass"),
  function(x){
    x@pro_data
  })

#' @exportMethod pro_data<-
#' @aliases pro_data<-
#' @param value The value for the slot.
#' @rdname mcmass-class
setReplaceMethod("pro_data", 
  signature = c(x = "mcmass"),
  function(x, value){
    initialize(x, pro_data = value)
  })

#' @exportMethod raw_data<-
#' @aliases raw_data<-
#' @param value The value for the slot.
#' @rdname mcmass-class
setReplaceMethod("raw_data", 
  signature = c(x = "mcmass"),
  function(x, value){
    initialize(x, raw_data = value)
  })

#' @exportMethod sample_metadata
#' @aliases sample_metadata
#' @rdname mcmass-class
setMethod("sample_metadata", 
  signature = c(x = "mcmass"),
  function(x){
    x@sample_metadata
  })

#' @exportMethod sample_metadata<-
#' @aliases sample_metadata<-
#' @rdname mcmass-class
setReplaceMethod("sample_metadata", 
  signature = c(x = "mcmass"),
  function(x, value){
    .check_columns(value, list("file", "sample", "group"),
      "sample_metadata")
    x@sample_metadata <- value
    return(x)
  })

#' @exportMethod features_quantification
#' @aliases features_quantification
#' @description \code{features_quantification}, \code{features_quantification<-}: getter and setter
#' for the \code{features_quantification} slot of the object.
#' @rdname mcmass-class
setMethod("features_quantification", 
  signature = c(x = "mcmass"),
  function(x){
    x@features_quantification
  })

#' @exportMethod features_quantification<-
#' @aliases features_quantification<-
#' @param value The value for the slot.
#' @rdname mcmass-class
setReplaceMethod("features_quantification", 
  signature = c(x = "mcmass", value = "data.frame"),
  function(x, value){
    initialize(x, features_quantification = value)
  })

setGeneric("features_defination", 
  function(x) standardGeneric("features_defination"))
setGeneric("features_defination<-", 
  function(x, value) standardGeneric("features_defination<-"))

#' @exportMethod features_defination
#' @aliases features_defination
#' @description \code{features_defination}, \code{features_defination<-}: getter and setter
#' for the \code{features_defination} slot of the object.
#' @rdname mcmass-class
setMethod("features_defination", 
  signature = c(x = "mcmass"),
  function(x){
    x@features_defination
  })

#' @exportMethod features_defination<-
#' @aliases features_defination<-
#' @param value The value for the slot.
#' @rdname mcmass-class
setReplaceMethod("features_defination", 
  signature = c(x = "mcmass"),
  function(x, value){
    initialize(x, features_defination = value)
  })

setGeneric("ms2_spectra", 
  function(x) standardGeneric("ms2_spectra"))
setGeneric("ms2_spectra<-", 
  function(x, value) standardGeneric("ms2_spectra<-"))

#' @exportMethod ms2_spectra
#' @aliases ms2_spectra
#' @description \code{ms2_spectra}, \code{ms2_spectra<-}: getter and setter
#' for the \code{ms2_spectra} slot of the object.
#' @rdname mcmass-class
setMethod("ms2_spectra", 
  signature = c(x = "mcmass"),
  function(x){
    x@ms2_spectra
  })

#' @exportMethod ms2_spectra<-
#' @aliases ms2_spectra<-
#' @param value The value for the slot.
#' @rdname mcmass-class
setReplaceMethod("ms2_spectra", 
  signature = c(x = "mcmass"),
  function(x, value){
    initialize(x, ms2_spectra = value)
  })

setGeneric("parameter_set", 
  function(x) standardGeneric("parameter_set"))
setGeneric("parameter_set<-", 
  function(x, value) standardGeneric("parameter_set<-"))

#' @exportMethod parameter_set
#' @aliases parameter_set
#' @description \code{parameter_set}, \code{parameter_set<-}: getter and setter
#' for the \code{parameter_set} slot of the object.
#' @rdname mcmass-class
setMethod("parameter_set", 
  signature = c(x = "mcmass"),
  function(x){
    x@parameter_set
  })

#' @exportMethod parameter_set<-
#' @aliases parameter_set<-
#' @param value The value for the slot.
#' @rdname mcmass-class
setReplaceMethod("parameter_set", 
  signature = c(x = "mcmass", value = "list"),
  function(x, value){
    initialize(x, parameter_set = value)
  })

setGeneric("detectFlow", 
  function(x) standardGeneric("detectFlow"))
setGeneric("detectFlow<-", 
  function(x, value) standardGeneric("detectFlow<-"))

#' @exportMethod detectFlow
#' @aliases detectFlow
#' @description \code{detectFlow}, \code{detectFlow<-}: getter and setter
#' for the \code{detectFlow} slot of the object.
#' @rdname mcmass-class
setMethod("detectFlow", 
  signature = c(x = "mcmass"),
  function(x){
    x@detectFlow
  })

#' @exportMethod detectFlow<-
#' @aliases detectFlow<-
#' @param value The value for the slot.
#' @rdname mcmass-class
setReplaceMethod("detectFlow", 
  signature = c(x = "mcmass", value = "detectFlow"),
  function(x, value){
    initialize(x, detectFlow = value)
  })

# ==========================================================================
# main methods
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

setGeneric("run_lcms", 
  function(x) standardGeneric("run_lcms"))

#' @exportMethod run_lcms
#' 
#' @title Perform Feature Detection with steps in 'detectFlow'
#' @description This would use package mainly 'xcms' to perform
#' Feature Detection.
#'
#' @aliases run_lcms
#'
#' @param x [mcmass-class] object.
#'
#' @rdname run_lcms-methods
#'
#' @examples
#' \dontrun{
#' run_lcms(...)
#' }
setMethod("run_lcms", 
  signature = c(x = "mcmass"),
  function(x){
    .message_info_formal("run_lcms", "")
    steps <- layers(detectFlow(x))
    .message_info("run_lcms", command_name(steps[[ 1 ]]))
    raw_data(x) <- call_command(steps[[ 1 ]])
    env <- environment()
    for (i in 2:length(steps)) {
      command_args(steps[[ i ]]) <- freshToBe(command_args(steps[[ i ]]), env)
      name <- command_name(steps[[ i ]])
      param <- parameter_set(x)[[ name ]]
      if (is.null(param)) {
        param <- parameter_set(x)[[ sub("^[.a-zA-Z0-9_]*::", "", name) ]]
      }
      if (!is.null(param)) {
        for (j in names(param)) {
          if (any(j == names(command_args(steps[[ i ]])))) {
            command_args(steps[[ i ]])[[ j ]] <- param[[ j ]]
          }
        }
      }
      .message_info("run_lcms", name)
      pro_data(x) <- call_command(steps[[i]])
    }
    layers(detectFlow(x)) <- steps
    return(x)
  })

freshToBe <- function(lst, envir) {
  lapply(lst,
    function(obj) {
      if (is(obj, "toBeEval")) {
        eval(parse(text = obj), envir = envir)
      } else {
        obj
      }
    })
}

#' @export run_export
#' @aliases run_export
#' @description \code{run_export}: get \code{features_quantification}
#' and \code{ms2_spectra} in [mcmass-class] object.
#' @param keep_onlyWithMs2 logical(1). If \code{TRUE}, the data
#' \code{features_quantification} only keep features which possess MS2.
#' @param mzd passed to \code{MSnbase::combineSpectra}
#' @param minProp passed to \code{MSnbase::combineSpectra}
#' @param ppm passed to \code{MSnbase::combineSpectra}
#' @param ... passed to \code{MSnbase::combineSpectra}
#' @rdname run_lcms-methods
run_export <- function(x, keep_onlyWithMs2 = T, mzd = 0, minProp = .3, ppm = 20, ...)
{
  ## mass level 2
  ms2 <- xcms::featureSpectra(pro_data(x), return.type = "MSpectra")
  ms2 <- MSnbase::clean(ms2, all = TRUE)
  .message_info("run_export", "MSnbase::combineSpectra")
  ms2_spectra(x) <- MSnbase::combineSpectra(
    ms2, fcol = "feature_id", method = MSnbase::consensusSpectrum,
    mzd = 0, minProp = 0.3, ppm = 20, ...
  )
  ## mass level 1
  features_defination(x) <- xcms::featureDefinitions(pro_data(x))
  quant <- xcms::featureValues(pro_data(x), value = "into")
  quant <- data.frame(quant)
  colnames(quant) <- sample_metadata(x)$sample
  quant$.features_id <- rownames(quant)
  quant <- dplyr::relocate(tibble::as_tibble(quant), .data$.features_id)
  if (keep_onlyWithMs2) {
    quant <- dplyr::filter(
      quant, .features_id %in% ms2_spectra(x)@elementMetadata$feature_id
    )
  }
  features_quantification(x) <- quant
  return(x)
}

# ==========================================================================
# default detectFlow
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#' @export default_detectFlow
#' @aliases default_detectFlow
#' @param x [mcmass-class] object.
#' @param snthresh passed to \code{xcms::CentWaveParam} for
#' \code{xcms::findChromPeaks}
#' @param noise passed to \code{xcms::CentWaveParam} for
#' \code{xcms::findChromPeaks}
#' @param peakwidth passed to \code{xcms::CentWaveParam} for
#' \code{xcms::findChromPeaks}
#' @param ppm passed to \code{xcms::CentWaveParam} for
#' \code{xcms::findChromPeaks}
#' @param minFraction passed to \code{xcms::PeakDensityParam} for
#' \code{xcms::fillChromPeaks}
#' @description \code{default_detectFlow}: Create [detectFlow-class] object for
#' Feature Detection.
#' @rdname detectFlow-class
default_detectFlow <- function(x,
  snthresh = 5, noise = 50000, peakwidth = c(3, 30), ppm = 20,
  minFraction = .1)
{
  if (!is(x, "mcmass")) {
    stop("`x` must be a 'mcmass' object.")
  }
  args <- list(
    new_command(MSnbase::readMSData,
      files = sample_metadata(x)$file,
      centroided. = TRUE, mode = "onDisk",
      pdata = new("NAnnotatedDataFrame", sample_metadata(x))
      ),
    new_command(xcms::findChromPeaks, object = toBeEval("raw_data(x)"),
      param = xcms::CentWaveParam(snthresh = snthresh,
        noise = noise, peakwidth = peakwidth, ppm = ppm)
      ), 
    new_command(xcms::adjustRtime, object = toBeEval(),
      param = xcms::ObiwarpParam()
      ),
    new_command(xcms::groupChromPeaks, object = toBeEval(),
      param = xcms::PeakDensityParam(
        sampleGroups = sample_metadata(x)$group,
        minFraction = minFraction)
      ),
    new_command(xcms::fillChromPeaks, object = toBeEval(), 
      param = xcms::ChromPeakAreaParam()
    )
  )
  names(args) <- vapply(args, command_name, "ch")
  new("detectFlow", layers = args)
}

#' @importFrom methods new
#' @importFrom methods getClass
#' @export new_mcmass
#' @aliases new_mcmass
#' @description \code{new_mcmass}: Create a [mcmass-class] object.
#' @param sample_metadata data.frame. Contains columns of 'file', 'sample',
#' 'group'
#' @param snthresh passed to \code{xcms::CentWaveParam} for
#' \code{xcms::findChromPeaks}
#' @param noise passed to \code{xcms::CentWaveParam} for
#' \code{xcms::findChromPeaks}
#' @param peakwidth passed to \code{xcms::CentWaveParam} for
#' \code{xcms::findChromPeaks}
#' @param ppm passed to \code{xcms::CentWaveParam} for
#' \code{xcms::findChromPeaks}
#' @param minFraction passed to \code{xcms::PeakDensityParam} for
#' \code{xcms::fillChromPeaks}
#' @rdname mcmass-class
new_mcmass <- function(sample_metadata,
  snthresh = 5, noise = 50000, peakwidth = c(3, 30),
  ppm = 20, minFraction = .1)
{
  .suggest_bio_package("xcms")
  mcm <- .mcmass(
    raw_data = new(getClass("OnDiskMSnExp", where = "MSnbase")),
    pro_data = new(getClass("XCMSnExp", where = "xcms")),
    features_defination = new(getClass("DFrame", where = "S4Vectors")),
    ms2_spectra = new(getClass("MSpectra", where = "MSnbase")),
    sample_metadata = sample_metadata,
    detectFlow = .detectFlow()
  )
  detectFlow(mcm) <- default_detectFlow(
    mcm, snthresh = snthresh, noise = noise, peakwidth = peakwidth,
    ppm = ppm, minFraction = minFraction
  )
  return(mcm)
}

# ==========================================================================
# the following functions were source from 
# https://raw.githubusercontent.com/jorainer/xcms-gnps-tools/master/customFunctions.R
# but revised
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#' @title Format MS2 spectra for export in GNPS-MGF format
#'
#' @description
#'
#' Re-format MS2 spectrum information for export of the data in Mgf format
#' supported by GNPS. In detail, the function replaces the acquisition number
#' of each spectrum with the feature ID (expected to be present in the
#' `"feature_id"` column of `mcols(x)`) converted to an integer by removing
#' the ID's leading `"FT"`.
#'
#' @param x `Spectra`.
#'
#' @return `Spectra` with the acquisition number replaced.
#'
#' @author Johannes Rainer
#' 
#' @noRd
formatSpectraForGNPS <- function(x) {
  fids <- S4Vectors::mcols(x)$feature_id
  if (!length(fids))
    stop("No column named 'feature_id' present in 'mcols(x)'")
  fids <- as.integer(sub("^FT", "", fids))
  S4Vectors::mendoapply(x, fids, FUN = function(z, id) {
    z@acquisitionNum <- id
    z
  })
}

#' @title Plot multiple spectra into the same plot
#'
#' @description
#'
#' Plot multiple spectra into the same plot.
#'
#' @param x `Spectra` that should be plotted.
#'
#' @param col color to be used for the individual peaks.
#'
#' @param type `character(1)` defining the plot type. Defaults to `"h"` to plot
#'     vertical lines. For more details see documentation of `plot`.
#'
#' @param main `character(1)` defining the title.
#' 
#' @param ... additional arguments to be passed to `points`.
#'
#' @author Johannes Rainer
#' 
#' @noRd
plotSpectra <- function(x, col = "#00000040", type = "h", main, ...) {
  xcms::plot(3, 3, pch = NA, xlab = "m/z", ylab = "intensity",
    xlim = range(ProtGenerics::mz(x)),
    ylim = range(ProtGenerics::intensity(x)), main = main)
  tmp <- lapply(x,
    function(z) {
      graphics::points(ProtGenerics::mz(z), ProtGenerics::intensity(z),
        type = type, col = col,
        ...)
    })
}

#' @title Select spectrum with maximal intensity from a list of spectra
#'
#' @description
#'
#' `maxTic` can be used with the `combineSpectra` method to select the spectrum
#' with the largest overall signal from a list of spectra.
#'
#' @param z `Spectra` object.
#'
#' @return `Spectrum`
#'
#' @author Johannes Rainer
#'
#' @noRd
maxTic <- function(z) {
  z[[BiocGenerics::which.max(lapply(ProtGenerics::intensity(z), sum))]]
}

#' @title Convert CAMERA output to an edge list for GNPS
#'
#' @description
#'
#' `getEdgelist` takes the output from the `getPeaklist` function from `CAMERA`
#' and converts it to a `data.frame` with edge definitions for *GNPS*. Each
#' row in that `data.frame` contains in columns `"ID1"` and `"ID2"` the
#' identifiers (i.e. `rownames` of the input `data.frame`) of the features.
#' Column `"EdgeType"` is always `"MS1 annotation"` and column `"Score"` `NA`
#' (since no score can be exported from `CAMERA`). Columns `"Annotation"`
#' contains the adduct names and their difference in m/z if **both** edges
#' (features) were predicted to be an adduct of the **same** compound. If
#' isotope annotations are available, these are also added to the column.
#' Column `"CorrelationGroup"` provides the information which
#' features were grouped by `CAMERA` into the same group.
#' 
#' @param peaklist `data.frame` as returned by the [getPeaklist()] function
#'   from `CAMERA` package or an `xsAnnotate` object.
#'
#' @return `data.frame` with edge definitions (see description for more
#'     details).
#'
#' @author Mar Garcia-Aloy
#' 
#' @examples
#' 
#' res <- getEdgelist(getPeaklist(xsaFA))
#' @noRd
getEdgelist <- function(peaklist) {
  if (is(peaklist, "xsAnnotate")) {
    peaklist <- CAMERA::getPeaklist(peaklist)
    if (!nrow(peaklist))
      stop("Got an empty peak list.")
  }
  pl <- split(peaklist, factor(peaklist$pcgroup,
      levels = unique(peaklist$pcgroup)))
  res <- do.call(rbind, lapply(pl, .process_pcgroup))
  rownames(res) <- NULL
  res
}

#' @title Extract feature annotations from CAMERA results
#'
#' @description
#'
#' Similar to the `getEdgelist` function, this function extracts information
#' from a `CAMERA` result for use in GNPS.
#'
#' @param x `xsAnnotate` object after calling `findAdducts`.
#'
#' @return
#'
#' `data.frame` with columns:
#' - `"annotation network number"`: ion identity network (IIN) number. All
#'   features predicted by `CAMERA` to be an adduct of a (co-eluting) compound
#'   with the same mass are part of this IIN. If a feature was predicted to be
#'   an adduct of two different compounds (with different masses) the ID of the
#'   larger network is reported. All features for which no adduct annotation is
#'   available will have an `NA` in this column.
#' - `"best ion"`: the adduct definition of the feature.
#' - `"correlation group ID"`: this corresponds to the `"pcgroup"` column in
#'   `getPeaklist(x)`.
#' - `"auto MS2 verify"`: always `NA`.
#' - `"identified by n="`: the size of the IIN.
#' - `"partners"`: all other features (rows in the feature table) of this IIN.
#' - `"neutral M mass"`: the mass of the compound.
#'
#' @author Johannes Rainer
#'
#' @noRd
getFeatureAnnotations <- function(x) {
  if (!length(x@annoID))
    stop("No adduct information present. Please call 'findAdducts' on ",
      "the object.")
  corr_group <- rep(seq_along(x@pspectra), lengths(x@pspectra))
  corr_group <- corr_group[order(unlist(x@pspectra, use.names = FALSE))]

  ## Get the all ids (feature rows) for which an adduct was defined
  ids <- unique(x@annoID[, "id"])

  ## Note: @pspectra contains the "correlation groups", @annoID the
  ## adduct groups, but it can happen that two ids are in the same adduct
  ## group without being in the same correlation group!

  ## loop through the adduct definitions and build the output data.frame
  adduct_def <- lapply(ids, function(id) {
    ## IDs of the same correlation group
    ids_pcgroup <- x@pspectra[[corr_group[id]]]
    ## ID of the adduct annotation groups this id is part of 
    anno_grp <- x@annoID[x@annoID[, "id"] == id, "grpID"]
    ## Subset the adduct annotation to rows matching the annotation group
    ## of the present ID and to ids present in the same correlation group.
    adduct_ann <- x@annoID[x@annoID[, "id"] %in% ids_pcgroup &
      x@annoID[, "grpID"] %in% anno_grp, , drop = FALSE]
    ## if we have more than one annotation group, select the bigger one
    if (length(anno_grp) > 1) {
      cnts <- BiocGenerics::table(adduct_ann[, "grpID"])
      adduct_ann <- adduct_ann[
        adduct_ann[, "grpID"] ==
          names(cnts)[order(cnts, decreasing = TRUE)][1], ,
        drop = FALSE]
    }
    grp_id <- adduct_ann[1, "grpID"]
    ## different adduct rules can match the same m/z - we're just taking
    ## the first one (with lower ruleID)
    rule_id <- adduct_ann[adduct_ann[, "id"] == id, "ruleID"][1]
    ids_grp <- adduct_ann[, "id"]
    df <- data.frame(
      `row ID` = id,
      `annotation network number` = unname(grp_id),
      `best ion` = as.character(x@ruleset[rule_id, "name"]),
      `identified by n=` = nrow(adduct_ann),
      `partners` = paste0(ids_grp[ids_grp != id], collapse = ";"),
      `neutral M mass` = unname(
        x@annoGrp[x@annoGrp[, "id"] == grp_id, "mass"]),
      stringsAsFactors = FALSE, check.names = FALSE)
      })
  adduct_def <- do.call(rbind, adduct_def)
  res <- adduct_def[rep("other", length(corr_group)), ]
  res[, "row ID"] <- seq_along(corr_group)
  rownames(res) <- as.character(res[, "row ID"])
  res[ids, ] <- adduct_def
  res$`correlation group ID` <- corr_group
  res$`auto MS2 verify` <- NA
  res
}

#' Helper function to extract the adduct annotation from a pair of adducts
#' from the same *pcgroup*.
#'
#' @author Mar Garcia-Aloy
#'
#' @noRd
.define_annot <- function(y) {
  if (any(y$adduct == "")) return(NA)
  mass_1 <- .extract_mass_adduct(y$adduct[1])
  mass_2 <- .extract_mass_adduct(y$adduct[2])
  mass <- BiocGenerics::intersect(mass_1, mass_2)
  if (length(mass)) {
    def1 <- unlist(strsplit(y$adduct[1], " "))
    def2 <- unlist(strsplit(y$adduct[2], " "))
    paste0(def1[grep(mass[1], def1) - 1], " ",
      def2[grep(mass[1], def2) - 1], " dm/z=",
      round(abs(y$mz[1] - y$mz[2]), 4))
  } else NA
}

#' Helper function to extract the isotope annotation from a pair of adducts
#' from the same *pcgroup*.
#'
#' @author Mar Garcia-Aloy
#'
#' @noRd
.define_isotop <- function(w) {
  if (any(w$isotopes == "")) return(NA)
  if (unlist(strsplit(w$isotopes[1], '\\]\\[') )[1] == 
    unlist(strsplit(w$isotopes[2], '\\]\\[') )[1]) {
    a = paste0("[", do.call(rbind, strsplit(w$isotopes, "\\]\\["))[, 2], 
      collapse = " ")
    b = paste0(unlist(strsplit(w$isotopes[2], '\\]') )[1], "]")
    paste0(b, a, " dm/z=", round(abs(w$mz[1] - w$mz[2]), 4))
  } else NA
}

#' Simple helper to extract the mass(es) from strings such as
#' [M+NH4]+ 70.9681 [M+H]+ 87.9886
#'
#' @author Johannes Rainer
#' 
#' @noRd
#' 
#' @examples
#'
#' .extract_mass_adduct("[M+NH4]+ 70.9681 [M+H]+ 87.9886")
#' .extract_mass_adduct("some 4")
.extract_mass_adduct <- function(x) {
  if (!length(x) || x == "") return(NA)
  spl <- unlist(strsplit(x, " ", fixed = TRUE))
  spl[seq(2, by = 2, length.out = length(spl)/2)]
}

#' Helper function to process features from the same *pcgroup*
#'
#' @author Mar Garcia-Aloy
#'
#' @noRd
.process_pcgroup <- function(x) {
  if (nrow(x) > 1) {
    res <- combn(seq_len(nrow(x)), 2, FUN = function(z) {
      anno <- .define_annot(x[z, ])
      iso <- .define_isotop(x[z, ])
      if (is.na(anno[1])) anno <- character()
      if (is.na(iso[1])) iso <- character()
      data.frame(ID1 = rownames(x)[z[1]],
        ID2 =  rownames(x)[z[2]],
        EdgeType = if (length(anno) || length(iso)) "MS1 annotation" else "MS1 correlation",
        Score = 0.0,
        Annotation = paste0(anno, iso, collapse = " "),
        CorrelationGroup = x$pcgroup[1],
        stringsAsFactors = FALSE)
      }, simplify = FALSE)
    do.call(rbind, res)
  } else NULL
}
