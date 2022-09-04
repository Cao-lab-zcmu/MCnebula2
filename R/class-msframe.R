# ==========================================================================
# msframe: class based on data.frame
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.msframe <- 
  setClass("msframe", 
           contains = "subscript",
           representation = 
             representation(entity = "data.frame"),
           prototype = NULL
  )
# ==========================================================================
# methods
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("show", 
          signature = c(object = "msframe"),
          function(object){
            cat( "A class of \"msframe\" of", subscript(object), "\n")
          })
setMethod("msframe", 
          signature = c(x = "ANY"),
          function(x){ x@msframe })
setReplaceMethod("msframe", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, msframe = value)
                 })
## ------------------------------------- 
setMethod("latest", 
          signature = c(x = "msframe"),
          function(x){
            tibble::as_tibble(entity(x[[1]]))
          })
## ------------------------------------- 
setMethod("entity", 
          signature = c(x = "msframe"),
          function(x){ x@entity })
setReplaceMethod("entity", 
                 signature = c(x = "msframe"),
                 function(x, value){
                   initialize(x, entity = value)
                 })
## ------------------------------------- 
setMethod("read_data", 
          signature = c(x = "ANY",
                        path = "missing",
                        project_metadata = "project_metadata",
                        fun_read = "missing",
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
          signature = c(x = "missing", path = "character",
                        project_metadata = "missing",
                        fun_read = "function", subscript = "character",
                        fun_format = "function",
                        .features_id = "character", .candidates_id = "character"
                        ),
          function(path, fun_read, subscript, fun_format,
                   .features_id, .candidates_id){
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
## ------------------------------------- 
setMethod("format_msframe", 
          signature = c(x = "msframe",
                        names = "character", fun_names = "missing",
                        types = "character", fun_types = "missing"
                        ),
          function(x, names, types){
            if( !is.character(names(names)) )
              stop( "the `names` is unformat" )
            if( !is.character(names(types)) )
              stop( "the `types` is unformat" )
            .format_msframe(x, names, types)
          })
setMethod("format_msframe", 
          signature = c(x = "msframe"),
          function(x){
            names <- .get_attribute_name_sirius.v4()
            types <- .get_attribute_type_sirius.v4()
            .format_msframe(x, names, types)
          })
setMethod("format_msframe", 
          signature = c(x = "msframe",
                        fun_names = "function", names = "missing",
                        fun_types = "function", types = "missing"
                        ),
          function(x, fun_names, fun_types){
            .format_msframe(x, fun_names(), fun_types())
          })
.format_msframe <- 
  function(x, names, types){
    ## ------------------------------------- 
    ## rename col
    if( any(names(names) == "...sig") ) {
      rs <- which( names == subscript(x) & names(names) == "...sig")
      rs <- rs + 1
      re <- length(names)
      for( i in rs:length(names) ){
        if( names(names)[i] == "...sig" ) {
          re <- i - 1
          break
        }
      }
      names <- c(names[rs:re], names)
      names <- vec_unique_by_value(names)
      names <- names[names(names) != "...sig"]
    }
    pattern <- paste0("^", names, "$")
    colnames(entity(x)) <-
      mapply_rename_col(pattern, names(names), colnames(entity(x)))
    ## ------------------------------------- 
    ## check attributes type
    names <- names[names(names) %in% colnames(entity(x))]
    for (i in names(names)) {
      if (i %in% names(types))
        target_type <- types[[i]]
      else
        target_type <- "character"
      fun <- match.fun(paste0("is.", target_type))
      if ( !fun(entity(x)[[i]]) ){
        fun <- match.fun(paste0("as.", target_type))
        entity(x)[[i]] <- fun(entity(x)[[i]])
      }
    }
    return(x)
  }
## ------------------------------------- 
setMethod("filter_msframe", 
          signature = c(x = "msframe", fun_filter = "function"),
          function(x, fun_filter, ...){
            filter_msframe(x, fun_filter = fun_filter,
                           f = ~ .features_id, ...)
          })
setMethod("filter_msframe", 
          signature = c(x = "msframe", fun_filter = "function",
                        f = "formula"),
          function(x, fun_filter, f, ...){
            .get_info("msframe", "filter_msframe",
                      paste0("group_by: ", paste0(f, collapse = " "))
            )
            entity <- lapply( split(entity(x), f = f), FUN = fun_filter, ...)
            entity(x) <- data.table::rbindlist(entity, fill = T)
            return(x)
          })

