# ==========================================================================
# collate any dataset in target project without filtering or arranging,
# relative to class-project
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases collate_data
#'
#' @title ...
#'
#' @description ...
#'
#' @details ...
#'
#' @name collate_data-methods
#'
#' @order 1
NULL
#> NULL

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
#' @description \code{collate_data()}: get the default parameters for the method
#' \code{collate_data}.
#' @rdname collate_data-methods
setMethod("collate_data", 
          signature = setMissing("collate_data"),
          function(){
            list(fun_collate = .collate_data.msframe)
          })
#' @exportMethod collate_data
#' @description \code{collate_data(x, ...)}: use the default parameters whatever 'missing'
#' while performing the method \code{collate_data}.
#' @rdname collate_data-methods
setMethod("collate_data", 
          signature = c(x = "ANY"),
          function(x, subscript, fun_collate, ...){
            reCallMethod("collate_data",
                         .fresh_param(collate_data()), ...)
          })
#' @exportMethod collate_data
#'
#' @aliases collate_data
#'
#' @title ...
#'
#' @description ...
#'
#' @details ...
#'
#' @param x ...
#' @param subscript ...
#' @param fun_collate ...
#' @param ... ...
#'
# @inheritParams rdname
#'
#' @return ...
#'
# @seealso ...
#'
#' @rdname collate_data-methods
#'
#' @examples
#' \dontrun{
#' collate_data(...)
#' }
setMethod("collate_data", 
          signature = c(x = "ANY", subscript = "character",
                        fun_collate = "function"),
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
#' @exportMethod read_data
#'
#' @description ...
#'
#' @param x ...
#' @param project_metadata ...
#' @param subscript ...
#' @param path ...
#' @param .features_id ...
#' @param .candidates_id ...
#' @param fun_read ...
#' @param fun_format ...
#'
#' @rdname collate_data-methods
#'
#' @examples
#' \dontrun{
#' read_data(...)
#' }
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
#' @exportMethod read_data
#' @rdname collate_data-methods
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

