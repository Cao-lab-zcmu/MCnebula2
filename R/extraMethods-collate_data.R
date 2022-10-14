# ==========================================================================
# collate any dataset in target project without filtering or arranging,
# relative to class-project
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @importFrom dplyr mutate
#' @importFrom dplyr relocate
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr distinct
#' @importFrom dplyr filter
#' @importFrom dplyr rename
#' @importFrom tibble as_tibble
#' @importFrom tibble tibble
#' @importFrom data.table rbindlist
#' @importFrom data.table fread
#' @importFrom stringr str_extract
#' @exportMethod collate_data
setMethod("collate_data", 
          signature = setMissing("collate_data",
                                 x = "ANY", subscript = "character"),
          function(x, subscript){
            collate_data(x, subscript, .collate_data.msframe)
          })
setMethod("collate_data",
          signature = setMissing("collate_data",
                                 x = "ANY",
                                 subscript = "character"),
          function(x, subscript, ...){
            collate_data(x, subscript, .collate_data.msframe, ...)
          })
setMethod("collate_data", 
          signature = c(x = "ANY", subscript = "character",
                        fun_collate = "function"
                        ),
          function(x, subscript, fun_collate, ...){
            x <- get_metadata(x, subscript)
            msframe.lst <- extract_rawset(x, subscript, fun_collate, ...)
            project_dataset(x) <- add_dataset(project_dataset(x), msframe.lst)
            return(x)
          })
.collate_data.msframe <- 
  function(x, subscript, reference){
    project_metadata <- extract_metadata(x, subscript)
    if (!missing(reference)) {
      df <- metadata(project_metadata)[[ subscript ]]
      df <- dplyr::mutate(df, .features_id = match.features_id(x)(upper),
                          .candidates_id = match.candidates_id(x)(files))
      df <- merge(reference, df, by = c(".features_id", ".candidates_id"))
      metadata(project_metadata)[[ subscript ]] <- df
    }
    read_data(x, project_metadata = project_metadata,
              subscript = subscript)
  }
## ---------------------------------------------------------------------- 
setMethod("read_data", 
          signature = setMissing("read_data",
                                 x = "ANY",
                                 project_metadata = "project_metadata",
                                 subscript = "character"),
          function(x, project_metadata, subscript){
            path.df <- metadata(project_metadata)[[ subscript ]]
            path <- paste0(project_path(x), "/", path.df$upper, "/", path.df$files)
            fun_read <- methods_read(x)[[ paste0("read", subscript) ]]
            fun_format <- methods_format(x)
            .features_id <- match.features_id(x)(path.df$upper)
            if (length(.features_id) == 0)
              .features_id <- subscript
            .candidates_id <- match.candidates_id(x)(path.df$files)
            .message_info("collate_data", "read_data", subscript)
            msframe <- read_data(path = path, fun_read = fun_read,
                                 subscript = subscript, fun_format = fun_format,
                                 .features_id = .features_id,
                                 .candidates_id = .candidates_id
            )
          })
setMethod("read_data", 
          signature = setMissing("read_data",
                       subscript = "character", path = "character",
                       .features_id = "character", .candidates_id = "character",
                       fun_read = "function", fun_format = "function"),
          function(subscript, path,
                   .features_id, .candidates_id,
                   fun_read, fun_format){
            entity <- fun_read(path)
            if (is.data.frame(entity)) {
              ## a 'data.table' may cause error
              entity <- list(data.frame(entity))
              names(entity) <- subscript
            }
            entity <- mapply(entity, .features_id, .candidates_id,
                             SIMPLIFY = F,
                             FUN = function(df, .features_id, .candidates_id){
                               dplyr::mutate(df, .features_id = .features_id,
                                             .candidates_id = .candidates_id)
                             })
            entity <- dplyr::relocate(data.table::rbindlist(entity, fill = T),
                                      .features_id, .candidates_id)
            msframe <- new("msframe", subscript = subscript, entity = entity)
            fun_format(msframe)
          })
setMethod("extract_metadata", 
          signature = c(x = "ANY", subscript = "character"),
          function(x, subscript){
            x <- get_metadata(x, subscript = subscript)
            path.set <- metadata(project_metadata(x))[[ subscript ]]
            ## build project_metadata
            path.set <- list(path.set)
            names(path.set) <- subscript
            new("project_metadata", metadata = path.set)
          })
## ---------------------------------------------------------------------- 
setMethod("get_metadata", 
          signature = c(x = "ANY", subscript = "character"),
          function(x, subscript){
            exits_meta <- names( metadata(project_metadata(x)) )
            if (!subscript %in% exits_meta) {
              project_metadata(x) <-
                get_metadata(subscript = subscript,
                             project_metadata = project_metadata(x),
                             project_conformation = project_conformation(x),
                             path = project_path(x)
                )
            }
            return(x)
          })
setMethod("get_metadata", 
          signature = setMissing("get_metadata",
                                 subscript = "character",
                                 project_metadata = "project_metadata",
                                 project_conformation = "project_conformation",
                                 path = "character"),
          function(subscript, project_metadata, project_conformation, path){
            file_name <- file_name(project_conformation)
            file_api <- file_api(project_conformation)
            if (!subscript %in% names(file_api) )
              stop( "`subscript` not descriped in `names(file_api(project_conformation))`" )
            ## ------------------------------------- 
            api <- file_api[[ subscript ]]
            api <- strsplit(api, split = "/")[[1]]
            ## ------------------------------------- 
            for (i in 1:length(api)) {
              sub <- api[i]
              if ( any(sub == names(metadata(project_metadata))) )
                next
              if ( !sub %in% names(file_name) )
                stop( "`subscript` not descriped in `names(file_name(project_conformation))`" )
              ## get the name of file, or the function name to get file name
              target <- file_name[[sub]]
              ## get the target of filename
              if ( grepl("^FUN_", target) )
                target <- match.fun(target)()
              if ( i == 1 ) {
                df <- data.frame(files = list.files(path = path, pattern = target))
              } else {
                ## get the metadata of upper directory
                df <- metadata(project_metadata)[[ api[i - 1] ]]
                upper <- paste0(apply(df, 1, paste0, collapse = "/"))
                ## ------------------------------------- 
                .message_info("project_metadata", "get_metadata",
                          paste0(target, "(", sub, ")"))
                df <- .list_files(path, upper, target)
              }
              lst <- list( df )
              names(lst) <- sub
              project_metadata <- add_dataset(project_metadata, lst)
            }
            return(project_metadata)
          })

