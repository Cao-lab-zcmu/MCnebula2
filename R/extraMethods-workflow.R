# ==========================================================================
# Quickly create analysis templates.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases workflow
#'
#' @title Quickly create analysis templates.
#'
#' @description Quickly create analysis templates and quickly construct a report.
#' This template contains a number of pre-defined sections, each with a fixed
#' description and code. The flexible approach is to use this method to obtain the
#' codes that form these sections and then modify these codes, which is more
#' likely to result in analysis results and reports that meet the requirements.
#'
#' @name workflow-methods
#'
#' @order 1
NULL
#> NULL

setOldClass("environment")

#' @exportMethod workflow
#' @description \code{workflow()}: get the available section names and
#' its heading level.
#' @rdname workflow-methods
setMethod("workflow", 
  signature = setMissing("workflow"),
  function()
  {
    codes <- c(
      "workflow(sections = ",
      deparse(.workflow_name),
      ", mode = 'templ')"
    )
    writeLines(codes)
  }
)

#' @exportMethod workflow
#' @description \code{workflow(...)}: use the default parameters whatever 'missing'
#' while performing the method \code{workflow}.
#' @rdname workflow-methods
setMethod("workflow", 
  signature = c(mode = "character"),
  function(sections, mode, envir, sirius_version, sirius_project, ion_mode, ...)
  {
    reCallMethod("workflow",
      .fresh_param(
        list(
          sections = eval(.workflow_name),
          envir = parent.frame(),
          sirius_version = "sirius.v4",
          sirius_project = ".",
          ion_mode = "pos"
        )), ...)
  }
)

#' @importFrom utils menu
#' @exportMethod workflow
#'
#' @param sections numeric with names. Use \code{workflow()}
#' to show the available sections.
#' @param mode character(1). "print" or "run". If "print", print the
#' template of the select workflow; If "run", the codes would be eval,
#' and return with a [report-class] object.
#' @param envir The environment to eval the codes. Default is \code{parent.frame()}.
#' @param sirius_version character(1). Passed to [initialize_mcnebula()].
#' @param sirius_project character(1). Passed to [initialize_mcnebula()].
#' @param ion_mode character(1). Set this using \code{ion_mode<-}.
#' @param ... ...
#' 
#' @rdname workflow-methods
setMethod("workflow", 
  signature = setMissing("workflow",
    sections = "numeric", mode = "character", envir = "environment",
    sirius_version = "character", sirius_project = "character",
    ion_mode = "character"),
  function(sections, mode, envir, sirius_version, sirius_project, ion_mode, ...)
  {
    if (is.null(names(sections)))
      stop("`sections` must be numeric with names.")
    else if (!any(names(sections) %in% names(eval(.workflow_name))))
      stop("`sections` should be part or all of these in `workflow()`.")
    env.args <- as.environment(
      c(list(sirius_version = sirius_version,
          sirius_project = sirius_project,
          ion_mode = ion_mode), list(...))
    )
    sections <- lapply(names(sections),
      function(name) {
        ch <- .workflow_templ[[ name ]]
        vapply(ch, cl, character(1), .envir = env.args)
      })
    if (mode == "print") {
      lapply(sections,
        function(section) {
          writeLines(c(section, ""))
        })
      .workflow_gather()
      return(message("\n## Done"))
    } else if (mode == "run") {
      conflict <- gather_sections(get = F)
      if (length(conflict) > 0) {
        cl("Conflicting variable names detected in environment `envir`, ",
          "confirm to clear them?")
        input <- utils::menu(c("yes", "no"))
        if (input == 1)
          rm(list = conflict, envir = envir)
        else
          stop("Terminate the operation.")
      }
      lapply(sections,
        function(section) {
          eval(parse(text = section), envir = envir)
        })
      sections <- gather_sections(envir = envir)
      report <- do.call(new_report, sections)
      return(report)
    } else {
      return(NA)
    }
  }
)

.workflow_gather <- function(){
  codes <- substitute({
    sections <- gather_sections()
    report <- do.call(new_report, sections)
    render_report(report, file <- paste0(tempdir(), "/report.Rmd"))
    rmarkdown::render(file)
  })
  codes <- unlist(lapply(2:length(codes), function(n) deparse(codes[[n]])))
  writeLines(codes)
}

.workflow_name <-
  substitute(
    c("Abstract" = 1, "Introduction" = 1, "Set-up" = 1,
      "Integrate data and Create Nebulae" = 1,
      "Initialize analysis" = 2,
      "Filter candidates" = 2,
      "Filter chemical classes" = 2,
      "Create Nebulae" = 2,
      "Visualize Nebulae" = 2,
      "Session infomation" = 1
      ))

#' @importFrom glue glue
cl <- function(..., .open = "<<<", .close = ">>>", .envir = parent.frame()) {
  glue::glue(..., .open = .open, .close = .close, .envir = .envir)
}
