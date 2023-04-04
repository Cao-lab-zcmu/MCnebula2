# ==========================================================================
# collate any dataset in target project without filtering or arranging,
# relative to class-project
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases collate_data
#'
#' @title Extract and format data from raw project directory
#'
#' @description 
#' The primary method used to extract data from the raw project directory.
#' By specifying [subscript-class], this method reads all corresponding files,
#' followed by gathering and formating the data, then stores these data in the slot
#' (\code{dataset(project_dataset(object))}).
#'
#' @note Normally, users do not need to use this method for MCnebula2 analysis.
#' [filter_formula()], [filter_structure()], [filter_ppcp()]
#' provide more understandable usage.
#'
#' @details 
#' This methods requires the name and path of the file in the raw project directory,
#' as well as the reading function; These are recorded in [project-class].
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
#' @param x [project-class] object or other class object inheriting it.
#' @param subscript character(1). See [subscript-class].
#' @param fun_collate function.
#' Used to extract and format the data from raw project directory.
#' The default is \code{MCnebula2:::.collate_data.msframe}.
#' @param ... Other parameters passed to the fun_collate.
#'
#' @rdname collate_data-methods
#'
#' @examples
#' \dontrun{
#'   ## The raw data used for the example
#'   tmp <- paste0(tempdir(), "/temp_data")
#'   dir.create(tmp)
#'   eg.path <- system.file("extdata", "raw_instance.tar.gz",
#'                          package = "MCnebula2")
#'   
#'   utils::untar(eg.path, exdir = tmp)
#'   
#'   ## initialize 'mcnebula' object
#'   test <- mcnebula()
#'   test <- initialize_mcnebula(test, "sirius.v4", tmp)
#'   
#'   ## extract candidates data in SIRIUS project directory
#'   ## chemical structure
#'   test <- collate_data(test, ".f3_fingerid")
#'   latest(project_dataset(test))
#'   
#'   ## chemical formula
#'   test <- collate_data(test, ".f2_formula")
#'   latest(project_dataset(test))
#'   
#'   ## chemical classes
#'   test <- collate_data(test, ".f3_canopus")
#'   latest(project_dataset(test))
#'   
#'   ## mz and rt
#'   test <- collate_data(test, ".f2_info")
#'   latest(project_dataset(test))
#'   
#'   ## classification description
#'   test <- collate_data(test, ".canopus")
#'   
#'   ## the extracted data in 'mcnebula'
#'   dataset(project_dataset(test))
#'   entity(dataset(project_dataset(test))$.f3_fingerid)
#'   
#'   unlink(tmp, T, T)
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
    if (!missing(reference)) {
      df <- metadata(project_metadata)[[ subscript ]]
      df <- dplyr::mutate(df, .features_id = match.features_id(x)(upper),
                          .candidates_id = match.candidates_id(x)(files))
      df <- merge(reference, df, by = c(".features_id", ".candidates_id"))
      metadata(project_metadata)[[ subscript ]] <- df
    }
    project_metadata <- extract_metadata(x, subscript)
    read_data(x, project_metadata = project_metadata,
              subscript = subscript)
  }


# @exportMethod read_data
#' @description \code{read_data}: basic methods used to extract and format
#' data from raw project directory.
#'
#' @param project_metadata [project_metadata-class] object. Specifying the files to read.
#' @param path character(1). The path of raw project directory.
#' @param .features_id character. ID for signing files in sub-directory of each 'features'.
#' @param .candidates_id character. ID for signing each candidates of 'features'.
#' @param fun_read function. Used to read files from raw project directory.
#' @param fun_format function. Used to format the data.
#'
#' @rdname collate_data-methods
#' @noRd
#'
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

# @exportMethod read_data
#' @rdname collate_data-methods
#' @noRd
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

#' @export collate_used
#' @aliases collate_used
#' @description \code{collate_used}: Use [filter_structure()] and [create_reference()]
#' to build 'specific_candidate' data, then collate all used data of MCnebula workflow
#' from Project directory, for subsequent data processing.
#' @rdname collate_data-methods
collate_used <- function(x) {
  x <- filter_structure(x)
  x <- create_reference(x)
  sub1 <- c(".f2_formula",  ".canopus", ".f2_info", ".f2_msms")
  sub2 <- c(".f3_canopus", ".f3_spectra")
  for (i in sub1) {
    x <- collate_data(x, i)
    message()
  }
  for (i in sub2) {
    x <- collate_data(x, i, reference = specific_candidate(x))
    message()
  }
  return(x)
}
