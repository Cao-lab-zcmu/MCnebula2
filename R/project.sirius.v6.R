# ==========================================================================
# load Sirius data using RSirius
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

.env_api <- new.env()

initialize_sirius_api <- function(sirius, port = 8080L, ..., version = "v6")
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
    sdk <- RSirius::SiriusSDK$new()
    .env_api$v6 <- sdk$connect(glue::glue("http://localhost:{port}"))
  }
  .env_api$port <- port
  return(.env_api$v6)
}

list_files_top.sirius.v6 <- function(path, pattern){
  if (!.is_sirius_running(.env_api$port)) {
    stop('!is_sirius_running(.env_api$port), SIRIUS is not running or expired?')
  }
  message(glue::glue("Now open SIRIUS project ..."))
  path <- normalizePath(path)
  id <- .env_api$projectID[[ path ]] <- digest::digest(
    path, "xxhash64", serialize = 3L
  )
  res <- try(.env_api$v6$projects_api$OpenProject(id, path), TRUE)
  if (inherits(res, "try-error") && !usethis::ui_yeah("Re-open failed, is project already open?")) {
    stop('Something error.')
  }
  features <- .env_api$v6$features_api$GetAlignedFeatures(id)
  ids <- vapply(features, function(x) x$alignedFeatureId, character(1))
  data.frame(files = ids)
}

list_files.sirius.v6 <- function(path, upper, pattern, ...){
  # lst_file <- pbapply::pbmapply(path, upper, pattern, SIMPLIFY = F,
  #   FUN = function(path, upper, pattern){
  #     files <- list.files(paste0(path, "/", upper), pattern)
  #     if ( length(files) == 0)
  #       return( data.frame() )
  #     data.frame(upper = upper, files = files)
  #   })
  # data.table::rbindlist(lst_file)
}

.validate_sirius.v6 <- function(path){
  if (!file.exists(path)) {
    stop('!file.exists(path).')
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
    stdout = "|",
    stderr = "|",
    supervise = TRUE
  )

  Sys.sleep(1)

  if (!proc$is_alive()) {
    message("SIRIUS failed to start")
    message(proc$read_all_output())
    message(proc$read_all_error())
    return(NULL)
  }

  for (i in seq_len(timeout)) {

    proc$read_output_lines()
    proc$read_error_lines()

    if (.is_sirius_running(port)) {
      message(glue::glue("SIRIUS ready (pid: {proc$get_pid()})"))
      return(proc)
    }

    if (!proc$is_alive()) {
      message("SIRIUS exited during startup")
      message(proc$read_all_output())
      message(proc$read_all_error())
      return(NULL)
    }

    Sys.sleep(1)
  }

  message("Timeout: SIRIUS API not ready")
  message(proc$read_all_output())
  message(proc$read_all_error())

  NULL
}
