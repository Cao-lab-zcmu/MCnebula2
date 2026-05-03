# ==========================================================================
# load Sirius data using RSirius
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

.env_api <- new.env()

.validate_sirius.v6 <- function(path) {
  if (is.null(.env_api$sirius) || is.null(.env_api$port)) {
    message(
      "You are run on SIRIUS version 6, this need SIRIUS clients support."
    )
    message(
      "But `initialize_sirius_api` has not been setup. Before using `initialize_mcnebula`,"
    )
    stop(
      "you should setup SIRIUS first via:\n\t`initialize_sirius_api('/path/to/sirius')`"
    )
  }
  if (!file.exists(path)) {
    stop('!file.exists(path), not a valid path to file?')
  }
}

# ==========================================================================
# todo:
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# used:
# .f3_fingerid, .f2_formula, .canopus, .f2_info, .f2_msms
# .f3_canopus, .f3_spectra

.get_file_name_sirius.v6 <- function(){
  set <- c(
    # `.id` is not used
    .id = "GetAlignedFeatures",
    .canopus = "GetCanopusPrediction",
    .canopus_neg = "GetCanopusPrediction",
    .canopus_summary = "",
    .compound_identifications = "",
    .formula_identifications = "",
    .csi_fingerid = "",
    .csi_fingerid_neg = "",
    .f2_ms = "",
    .f2_msms = "GetMsData",
    .f2_info = "GetAlignedFeatures",
    .f2_formula = "GetFormulaCandidates",
    .f3_canopus = "GetCanopusPrediction",
    .f3_fingerid = "GetStructureCandidates",
    .f3_scores = "",
    .f3_spectra = "GetFormulaAnnotatedSpectrum"
  )
}

.get_file_api_sirius.v6 <- function(){
  set <- c(.id = ".id",
    .canopus = ".id/.canopus",
    .canopus_summary = "",
    .compound_identifications = "",
    .formula_identifications = "",
    .canopus_neg = ".id/.canopus_neg",
    .csi_fingerid = "",
    .csi_fingerid_neg = "",
    .f2_ms = "",
    .f2_msms = ".id/.f2_msms",
    .f2_info = ".id/.f2_info",
    .f2_formula = ".id/.f2_formula",
    .f3_canopus = ".id/.f3_canopus",
    .f3_fingerid = ".id/.f3_fingerid",
    .f3_scores = "",
    .f3_spectra = ".id/.f3_spectra"
  )
}

.arguments_type_sirius_api <- function() {
  list(
    feature = c("GetFormulaCandidates", "GetStructureCandidates", "GetMsData"),
    candidate = c("GetCanopusPrediction", "GetFormulaAnnotatedSpectrum"),
    project = c("GetAlignedFeatures")
  )
}

list_files.sirius.v6 <- function(path, upper, pattern, ...)
{
  if (!is.character(pattern) && !nchar(pattern)) {
    message(glue::glue("Invalid API function name (valid is such as: `GetAlignedFeatures`)."))
  }
  id <- .touch_project.sirius.v6(path)
  types <- .arguments_type_sirius_api()
  if (pattern %in% types$candidate) {
    path <- paste0(normalizePath(path), "/", upper, "/GetFormulaCandidates")
    data <- suppressMessages(
      .collate_candidates_via_sirius_api(path, bind = TRUE)
    )
    data <- dplyr::mutate(
      data, upper = paste0(.features_id, ",", formulaId),
      files = paste0(!!pattern, ",", formulaId)
    )
    dplyr::select(data, upper, files)
  } else if (pattern %in% types$feature) {
    data.table::data.table(upper = upper, files = pattern)
  } else if (pattern %in% types$project) {
    # tibble::data.table(upper = upper, files = "")
    data.table::data.table(upper = "NULL", files = pattern)
  } else {
    stop('Not a valid API name')
  }
}

.get_methods_match_sirius.v6 <- function(){
  set <- c(
    match.features_id = function(x) {
      if (any(grepl(",", head(x)))) {
        .split_args(x, 1L)
      } else {
        x
      }
    },
    match.candidates_id = function(x) {
      if (all(!is.na(suppressWarnings(as.double(head(x)))))) {
        x
      } else if (any(grepl(",", head(x)))) {
        x <- .split_args(x, 2L)
        if (all(!is.na(suppressWarnings(as.double(head(x)))))) {
          x
        } else {
          NULL
        }
      } else {
        NULL
      }
    },
    generate_candidates_id = function(x) x$.candidates_id
  )
}

setMethod("read_data", signature = setMissing("read_data",
    subscript = "character", path = "character",
    .features_id = "character", .candidates_id = "NULL",
    fun_read = "function", fun_format = "function"),
  function(subscript, path, .features_id, .candidates_id, fun_read, fun_format)
  {
    # fun_read is: .collate_candidates_via_sirius_api
    entity <- fun_read(path, bind = TRUE)
    if (all(c(".features_id", ".candidates_id") %in% colnames(entity))) {
      entity <- dplyr::relocate(
        entity, .features_id, .candidates_id
      )
    } else if (".features_id" %in% colnames(entity)) {
      entity <- dplyr::relocate(entity, .features_id)
    }
    msframe <- new("msframe", subscript = subscript, entity = entity)
    fun_format(msframe)
  })

.fakeFun_canopus <- setClass("fakeFun_canopus", contains = c("NULL"))

setMethod("read_data", signature = setMissing("read_data",
    subscript = "character", path = "character",
    .features_id = "character", .candidates_id = "character",
    fun_read = "fakeFun_canopus", fun_format = "function"),
  function(subscript, path, .features_id, .candidates_id, fun_read, fun_format)
  {
    if (!subscript %in% c(".canopus_neg", ".canopus")) {
      stop('!subscript %in% c(".canopus_neg", ".canopus").')
    }
    message("Try obtain one CANOPUS records as metadata for all features.")
    message("Please ensure that CANOPUS has been executed and at least one feature has a corresponding result.")
    groups <- split(path, ceiling(seq_along(path) / 10L))
    for (pathTest in groups) {
      dataTest <- .collate_canopus_via_sirius_api(
        pathTest, bind = FALSE, get_index = TRUE
      )
      dataTest <- dataTest[ !vapply(dataTest, is.null, logical(1)) ]
      if (length(dataTest)) {
        break
      }
    }
    entity <- dataTest[[ 1L ]]
    entity <- dplyr::mutate(
      entity, .features_id = NA_character_, .candidates_id = NA_character_
    )
    msframe <- new("msframe", subscript = subscript, entity = entity)
    fun_format(msframe)
  })

.split_args <- function(x, which, split = ",") {
  vapply(strsplit(x, ","), function(x) x[[which]], character(1))
}

.collate_features_via_sirius_api <- function(path, ...) {
  name_fun <- basename(path)
  path <- dirname(dirname(path))
  id <- .touch_project.sirius.v6(path)
  data <- .map_to_extId_run(name_fun, id)
  if (is(data, "list")) {
    lst <- lapply(data,
      function(x) {
        x <- x$toList()
        dplyr::bind_rows(x[ !vapply(x, is, logical(1L), "list") ])
      })
    data.table::rbindlist(lst, fill = TRUE)
  } else {
    dplyr::bind_rows(data$toList())
  }
}

.collate_spectra_annotated_via_sirius_api <- function(path, ...) {
  .collate_candidates_via_sirius_api(path, ...,
    fun_as_data_R6 = function(x) {
      x <- lapply(x$peaks,
        function(x) {
          int <- x$intensity
          mz <- x$mz
          x <- x$peakAnnotation
          if (is.null(x) || !length(x)) {
            NULL
          } else {
            x <- x$toList()
            x <- x[ !vapply(x, is, logical(1L), "list") ]
            if (length(x)) {
              append(x, list(mz = mz, intensity = int, rel.intensity = int))
            } else {
              NULL
            }
          }
        })
      dplyr::bind_rows(x)
    })
}

.collate_msms_via_sirius_api <- function(path, ...) {
  .collate_candidates_via_sirius_api(path, ...,
    fun_as_data_R6 = function(x) {
      x <- x$mergedMs2$peaks
      if (is(x, "list")) {
        x <- lapply(x,
          function(x) {
            x$toList()
          })
        dplyr::bind_rows(x)
      }
    })
}

.collate_canopus_via_sirius_api <- function(path, ..., get_index = FALSE)
{
  .collate_candidates_via_sirius_api(path, ...,
    fun_filter_data = function(data) {
      if (!is.null(data) && nrow(data)) {
        data <- dplyr::filter(data, type == "ClassyFire")
        if (get_index) {
          data <- dplyr::select(data, -probability)
        } else {
          data <- dplyr::select(data, probability)
        }
        data <- dplyr::mutate(data, rel.index = seq_len(nrow(data)))
      }
      data
    })
}

.collate_candidates_via_sirius_api <- function(path, 
  bind = FALSE, get_index = FALSE,
  fun_as_data_R6 = NULL, fun_filter_data = NULL
)
{
  name_fun <- basename(path)
  if (any(grepl(",", head(name_fun)))) {
    name_fun <- .split_args(name_fun, 1L)
  }
  name_fun <- unique(name_fun)
  if (length(name_fun) != 1L) {
    stop('length(unique(name_fun)) != 1L, Does the input path contain Multiple API function?')
  }
  features <- basename(dirname(path))
  path <- normalizePath(dirname(dirname(path)))
  if (length(unique(path)) != 1L) {
    stop('length(unique(path)) != 1L, Does the input path contain Multiple SIRIUS project?')
  }
  path <- path[1L]
  tryTimes <- getOption("mcn_try_touch_sirius_times", 3L)
  if (!is.numeric(tryTimes)) {
    stop('!is.numeric(tryTimes), Invalid `mcn_try_touch_sirius_times` option.')
  }
  id <- .touch_project.sirius.v6(path)
  split_args <- FALSE
  as_data_frame_R6 <- function(x) {
    if (is.null(fun_as_data_R6)) {
      data <- dplyr::bind_rows(x$toList())
    } else {
      data <- fun_as_data_R6(x)
    }
    if (is.null(fun_filter_data)) {
      data
    } else {
      fun_filter_data(data)
    }
  }
  lst <- pbapply::pbsapply(features, simplify = FALSE,
    function(fea) {
      res <- FALSE
      n <- 0L
      while ((isFALSE(res) || inherits(res, "try-error")) && n <= tryTimes) {
        n <- n + 1L
        if (inherits(res, "try-error")) {
          id <<- .touch_project.sirius.v6(path)
        }
        if (split_args || grepl(",", fea)) {
          split_args <<- TRUE
          args <- strsplit(fea, ",")[[1]]
          args <- setNames(
            as.list(args), c("features", rep("", length(args) - 1L))
          )
          args <- c(list(name_fun, id), args)
          res <- try(do.call(.map_to_extId_run, args), TRUE)
        } else {
          res <- try(
            .map_to_extId_run(name_fun, id, features = fea), TRUE
          )
        }
      }
      if (!length(res)) {
        message(
          glue::glue("Maybe no any results of the feature, escape: {fea}")
        )
        return(NULL)
      }
      if (is(res, "R6")) {
        data <- as_data_frame_R6(res)
        return(data)
      } else if (is(res, "list")) {
        lst <- lapply(res,
          function(x){
            if (is(x, "R6")) {
              as_data_frame_R6(x)
            } else {
              message(
                glue::glue(
                  "Unexpected class while try collating candidates data, \
                  the class is: {paste0(class(x), collapse = ', ')}"
                )
              )
              message("Escape this candidate.")
              NULL
            }
          }
        )
        return(dplyr::bind_rows(lst))
      }
    })
  message(glue::glue("Successfully perform `{name_fun}` on all features."))
  if (bind) {
    data <- data.table::rbindlist(lst, idcol = ".features_id", fill = TRUE)
    if (split_args) {
      data <- dplyr::mutate(
        data, .features_id = .split_args(.features_id, 1L)
      )
    }
    return(data)
  } else {
    lapply(lst, 
      function(data) {
        if (is.null(data)) {
          data.frame()
        } else {
          data
        }
      })
  }
}

.get_methods_read_sirius.v6 <- function(){
  set <- c(
    read.f2_msms = .collate_msms_via_sirius_api,
    read.f2_formula = .collate_candidates_via_sirius_api,
    read.f2_info = .collate_features_via_sirius_api,
    read.f3_fingerid = .collate_candidates_via_sirius_api,
    read.f3_spectra = .collate_spectra_annotated_via_sirius_api,
    read.f3_canopus = .collate_canopus_via_sirius_api,
    read.canopus = .fakeFun_canopus(),
    read.canopus_neg = .fakeFun_canopus()
  )
}

.get_attribute_name_sirius.v6 <- function(){
  set <- c(
    ## .f3_fingerid
    ...sig = ".f3_fingerid",
    inchikey2d = "inchiKey",
    mol.formula = "molecularFormula",
    rank.structure = "rank",
    csi.score = "csiScore",
    synonym = "structureName",
    smiles = "smiles",
    xlogp = "xlogP",
    tani.score = "tanimotoSimilarity",
    .candidates_id = "formulaId",
    ## .f3_spectra
    ...sig = ".f3_spectra",
    mz = "mz",
    int. = "intensity",
    rel.int. = "rel.intensity",
    exactmass = "exactMass",
    formula = "molecularFormula",
    ## .f2_formula
    ...sig = ".f2_formula",
    adduct = "adduct",
    zodiac.score = "zodiacScore",
    sirius.score = "siriusScore",
    tree.score = "treeScore",
    iso.score = "isotopeScore",
    rank.formula = "rank",
    .candidates_id = "formulaId",
    ## .f2_info
    ...sig = ".f2_info",
    rt.secound = "rtApexSeconds",
    mz = "ionMass",
    .features_id = "externalFeatureId",
    ## .canopus
    ...sig = ".canopus",
    rel.index = "rel.index",
    abs.index = "index",
    chem.ont.id = "id",
    class.name = "name",
    parent.chem.ont.id = "parentId",
    description = "description",
    ## .canopus_neg
    ...sig = ".canopus_neg",
    chem.ont.id = "id",
    class.name = "name",
    ## .canopus_summary
    ...sig = ".canopus_summary",
    .id = "name",
    most.sp.class = "most specific class",
    level5 = "level 5",
    subclass = "subclass",
    class = "class",
    superclass = "superclass",
    all.class = "all classifications",
    ## .compound_identifications
    ...sig = ".compound_identifications",
    cosmic.score = "ConfidenceScore",
    .id = "id",
    ## .f3_canopus
    ...sig = ".f3_canopus",
    pp.value = "probability",
    ...sig = "END"
  )
}

.get_attribute_type_sirius.v6 <- function(){
  set <- c(
    .candidates_id = "character",
    .features_id = "character",
    rank.formula = "integer",
    rank.structure = "integer",
    csi.score = "numeric",
    xlogp = "numeric",
    tani.score = "numeric",
    mz = "numeric",
    rt.secound = "numeric",
    rt.min = "numeric",
    int. = "numeric",
    rel.int. = "numeric",
    exactmass = "numeric",
    zodiac.score = "numeric",
    sirius.score = "numeric",
    tree.score = "numeric",
    iso.score = "numeric",
    hit.num. = "integer",
    hit.int. = "numeric",
    error.frag. = "numeric",
    error.abs.frag. = "numeric",
    error.mass = "numeric",
    rel.index = "integer",
    abs.index = "integer",
    cosmic.score = "numeric",
    pp.value = "numeric"
  )
}


# FUN_get_id_sirius.v6 <- 
#   function(x){
#     if (missing(x))
#       return("^[0-9](.*)_(.*)_(.*)$")
#     stringr::str_extract(x, "(?<=_)[^_|^/]{1,}(?=/|$)")
#   }

# ==========================================================================
# todo end
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

initialize_sirius_api <- function(sirius, port = 8080L, 
  ..., version = "v6", reset = FALSE)
{
  if (!requireNamespace("RSirius", quietly = TRUE)) {
    expr <- substitute(
      remotes::install_github(
        repo = "sirius-ms/sirius-client-openAPI",
        subdir = "client-api_r/generated", 
        ref = "master", build = TRUE
      )
    )
    string <- deparse(expr)
    message(glue::glue("`RSirius` is no installed, to install it:\n\n{string}"))
  }
  if (version != "v6") {
    stop('version != "v6".')
  }
  res <- .start_sirius_rest(sirius, port = port, ...)
  if (!is.null(res)) {
    .env_api$proc <- res
    sdk <- RSirius::SiriusSDK$new()
    .env_api$v6 <- sdk$connect(glue::glue("http://localhost:{port}"))
  }
  .env_api$port <- port
  .env_api$sirius <- sirius
  invisible(.env_api$v6)
}

.map_to_extId_run <- function(name, id, ..., features, .map = TRUE) {
  if (missing(features)) {
    .env_api$v6$features_api[[ name ]](id, ...)
  } else {
    # ID <- getOption("mcn_feature_id_type", "externalFeatureId")
    if (.map) {
      if (is.null(db <- .env_api$db_ids[[ id ]])) {
        lst <- .env_api$v6$features_api$GetAlignedFeatures(id)
        aliId <- vapply(lst, function(x) x$alignedFeatureId, character(1L))
        extId <- vapply(lst, function(x) x$externalFeatureId, character(1L))
        db <- .env_api$db_ids[[ id ]] <- list(aliId = aliId, extId = extId)
      }
      mapped <- db$aliId[ match(features, db$extId) ]
      if (any(is.na(mapped))) {
        stop('any(is.na(mapped)), mapping ID failed from "externalFeatureId" to "alignedFeatureId."')
      }
      features <- mapped
    }
    .env_api$v6$features_api[[ name ]](id, features, ...)
  }
}

# .map_back_feature_as_list <- function(x) {
#   if (is(x, "R6")) {
#     x$toList()
#   } else {
#     x
#   }
# }

.touch_project.sirius.v6 <- function(path) {
  if (!.is_sirius_running(.env_api$port)) {
    message("is_sirius_running(.env_api$port) == FALSE.")
    message(
      "SIRIUS may shutdown due to unknown event, try to running again."
    )
    continue <- getOption("mcn_re_open_sirius", TRUE)
    message("If this is not the behavior you expected, please cancel the reopening behavior by setting:")
    message("\t`options(mcn_re_open_sirius = False)`")
    if (continue) {
      if (!is.null(.env_api$sirius) && !is.null(.env_api$port)) {
        initialize_sirius_api(.env_api$sirius, .env_api$port)
      } else {
        stop('No pre-set parameters herein? Parameters were saved in: `MCnebula2:::.env_api`.')
      }
    } else {
      stop('SIRIUS is not running or expired.')
    }
  }
  # message(glue::glue("Try open SIRIUS project, this take some seconds."))
  # message(
  #   "If there is no response for a long time, please try killing the SIRIUS program and then reinitialize it with:"
  # )
  # message("\t`initialize_sirius_api`")
  path <- normalizePath(path)
  id <- .env_api$projectID[[ path ]] <- digest::digest(
    path, "xxhash64", serialize = 3L
  )
  status <- try(.env_api$v6$projects_api$GetProject(id), TRUE)
  if (!is(status, "ProjectInfo")) {
    res <- try(.env_api$v6$projects_api$OpenProject(id, path), TRUE)
    if (inherits(res, "try-error")) {
      message(
        glue::glue(
          "Project Open failed, test whether another project is opened."
        )
      )
      projects <- try(
        .env_api$v6$projects_api$GetProjects(), TRUE
      )
      if (!inherits(projects, "try-error") && length(projects)) {
        otherIds <- NULL
        if (length(projects) == 1L) {
          otherIds <- projects[[1]]$projectId
          message(glue::glue("Another project detected, ID is: {}"))
        } else {
          otherIds <- vapply(projects, function(x) x$projectId, character(1))
          message(
            glue::glue(
              "Multiple projects detected, ID are {paste0(otherIds, collapse = ', ')}"
            )
          )
        }
        if (length(otherIds) && usethis::ui_yeah("Do you want to closing other project?")) {
          lapply(otherIds,
            function(id) {
              res <- try(.env_api$v6$projects_api$CloseProject(id), TRUE)
              if (inherits(res, "try-error")) {
                message(glue::glue("Close project failed, ID is {id}"))
              } else {
                message(
                  glue::glue("Close project successfully, ID is {id}")
                )
              }
            })
        } else {
          stop("Something wrong.")
        }
      }
    }
  }
  return(id)
}

list_files_top.sirius.v6 <- function(path, pattern)
{
  if (pattern == "GetAlignedFeatures") {
    id <- .touch_project.sirius.v6(path)
    features <- .map_to_extId_run("GetAlignedFeatures", id)
    ids <- vapply(features, function(x) x$externalFeatureId, character(1))
    data.frame(files = ids)
  } else {
    data.frame(files = "")
  }
}


.is_sirius_running <- function(port = 8080L)
{
  res <- try(
    httr::GET(
      paste0("http://localhost:", port, "/v3/api-docs"),
      httr::timeout(1)
    ),
    silent = TRUE
  )
  if (inherits(res, "try-error")) {
    return(FALSE)
  }
  if (is.null(res$status_code) || res$status_code != 200) {
    return(FALSE)
  }
  txt <- try(httr::content(res, "text", encoding = "UTF-8"), silent = TRUE)
  if (inherits(txt, "try-error")) {
    return(FALSE)
  }
  grepl("SIRIUS", txt, ignore.case = TRUE)
}

.is_port_open <- function(port = 8080L)
{
  con <- try(
    socketConnection(
      host = "localhost",
      port = port,
      open = "r",
      blocking = TRUE,
      timeout = 1
    ),
    silent = TRUE
  )
  if (inherits(con, "try-error")) {
    return(FALSE)
  }
  close(con)
  TRUE
}

.is_executable_available <- function(cmd)
{
  res <- try(
    processx::run(
      command = cmd,
      args = "--version",
      error_on_status = FALSE
    ),
    silent = TRUE
  )
  if (inherits(res, "try-error")) {
    return(FALSE)
  }
  TRUE
}

.start_sirius_rest <- function(path_sirius = "sirius", port = 8080L, timeout = 20L)
{
  if (.is_sirius_running(port)) {
    message(glue::glue("Sirius is already running on port: {port}!"))
    return(TRUE)
  }

  if (.is_port_open(port) && !.is_sirius_running(port)) {
    stop(glue::glue("Port {port} is occupied by a non-SIRIUS service"))
  }

  path_sirius <- path.expand(path_sirius)

  is_path <- file.exists(path_sirius)

  if (!is_path) {
    message(glue::glue("Treating '{path_sirius}' as command"))
    if (!.is_executable_available(path_sirius)) {
      stop(glue::glue("Cannot find sirius as file or command: {path_sirius}"))
    }
  }

  if (.Platform$OS.type == "windows" && is_path && !file.exists(path_sirius)) {
    vec_try <- paste0(path_sirius, c(".exe", ".bat", ".cmd"))
    vec_exist <- vec_try[file.exists(vec_try)]
    if (length(vec_exist) > 0L) {
      path_sirius <- vec_exist[[1]]
    }
  }

  if (is_path) {
    path_sirius <- normalizePath(
      path_sirius, winslash = "/", mustWork = TRUE
    )
  }

  message(glue::glue("Starting SIRIUS REST on port {port}"))

  ext <- tolower(tools::file_ext(path_sirius))

  if (.Platform$OS.type == "windows" && ext %in% c("bat", "cmd")) {
    command <- "cmd"
    vec_args <- c("/c", path_sirius, "rest", "--port", port)
  } else {
    command <- path_sirius
    vec_args <- c("rest", "--port", port)
  }

  proc <- processx::process$new(
    command = command,
    args = vec_args,
    stdout = "sirius.log",
    stderr = "sirius.err",
    supervise = TRUE
  )

  Sys.sleep(1)

  if (!proc$is_alive()) {
    message("SIRIUS failed to start")
    return(NULL)
  }

  for (i in seq_len(timeout)) {

    if (.is_sirius_running(port)) {
      message(glue::glue("SIRIUS ready (pid: {proc$get_pid()})"))
      return(proc)
    }

    if (!proc$is_alive()) {
      message("SIRIUS exited during startup")
      return(NULL)
    }

    Sys.sleep(1)
  }

  message("Timeout: SIRIUS API not ready")

  NULL
}
