# ==========================================================================
# collate any dataset in sirius project without filtering or arranging
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("collate_data", 
          signature = c(x = "mcnebula", subscript = "character"),
          function(x, subscript){
            collate_data(x, subscript, .collate_data.msframe)
          })
setMethod("collate_data", 
          signature = c(x = "mcnebula", subscript = "character",
                        fun_collate = "function"
                        ),
          function(x, subscript, fun_collate, ...){
            x <- get_metadata(x, subscript)
            msframe.lst <- extract_rawset(x, subscript, fun_collate, ...)
            project_dataset(x) <- add_dataset(project_dataset(x), msframe.lst)
            return(x)
          })
.collate_data.msframe <- 
  function(x, subscript){
    project_metadata <- extract_metadata(x, subscript)
    msframe <- read_data(x, project_metadata = project_metadata,
                         subscript = subscript)
    return(msframe)
  }
## ---------------------------------------------------------------------- 
setMethod("read_data", 
          signature = c(x = "ANY",
                        project_metadata = "project_metadata",
                        subscript = "character"
                        ),
          function(x, project_metadata, subscript){
            path.df <- metadata(project_metadata)[[ subscript ]]
            path <-
              paste0(sirius_project(mcn_path(x)), "/", path.df$upper, "/", path.df$files)
            fun_read <-
              methods_read(project_api(x))[[ paste0("read", subscript) ]]
            fun_format <-
              methods_format(project_api(x))
            .features_id <-
              methods_match(project_api(x))[[ "match.features_id" ]](path.df$upper)
            if (length(.features_id) == 0)
              .features_id <- subscript
            .candidates_id <-
              methods_match(project_api(x))[[ "match.candidates_id" ]](path.df$files)
            .get_info("collate_data", "read_data", subscript)
            msframe <- read_data(path = path, fun_read = fun_read,
                                 subscript = subscript, fun_format = fun_format,
                                 .features_id = .features_id,
                                 .candidates_id = .candidates_id
            )
          })
setMethod("read_data", 
          signature = c(x = "missing", project_metadata = "missing",
                        subscript = "character", path = "character",
                        .features_id = "character", .candidates_id = "character",
                        fun_read = "function", fun_format = "function"
                        ),
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
