# ==========================================================================
# msframe: class based on data.frame
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.msframe <- 
  setClass("msframe", 
           contains = character(),
           representation = 
             representation(subscript = "character",
                            entity = "data.frame"
                            ),
           prototype = NULL
  )
# ==========================================================================
# methods
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("msframe", 
          signature = c(x = "ANY"),
          function(x){ x@msframe })
setReplaceMethod("msframe", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, msframe = value)
                 })
## ------------------------------------- 
setMethod("subscript", 
          signature = c(x = "msframe"),
          function(x){ x@subscript })
setReplaceMethod("subscript", 
                 signature = c(x = "msframe"),
                 function(x, value){
                   initialize(x, subscript = value)
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
                        read_fun = "missing",
                        subscript = "character"
                        ),
          function(x, project_metadata, subscript){
            path.df <- metadata(project_metadata)[[ get_upper_dir_subscript(x, subscript) ]]
            path <-
              paste0(sirius_project(mcn_path(x)), "/", path.df$upper, "/", path.df$files)
            read_fun <-
              read_methods(project_api(x))[[ paste0("read", subscript) ]]
            format_fun <-
              format_methods(project_api(x))
            .features_id <-
              match_methods(project_api(x))[[ "match.features_id" ]](path.df$upper)
            .candidates_id <-
              match_methods(project_api(x))[[ "match.candidates_id" ]](path.df$files)
            .get_info("collate_data", "read_data", subscript)
            msframe <- read_data(path = path,
                                 read_fun = read_fun,
                                 subscript = subscript,
                                 format_fun = format_fun,
                                 .features_id = .features_id,
                                 .candidates_id = .candidates_id
            )
          })
setMethod("read_data", 
          signature = c(x = "missing",
                        path = "character",
                        project_metadata = "missing",
                        read_fun = "function",
                        subscript = "character",
                        format_fun = "function",
                        .features_id = "character",
                        .candidates_id = "character"
                        ),
          function(path, read_fun, subscript, format_fun,
                   .features_id, .candidates_id){
            entity <- read_fun(path)
            if (is.list(entity)) {
              entity <- mapply(entity, .features_id, .candidates_id,
                               SIMPLIFY = F,
                               FUN = function(df, .features_id, .candidates_id){
                                 dplyr::mutate(df, .features_id = .features_id,
                                               .candidates_id = .candidates_id)
                               })
              entity <- data.table::rbindlist(entity, fill = T)
              entity <- dplyr::relocate(entity, .features_id, .candidates_id)
            }
            msframe <- new("msframe", subscript = subscript, entity = entity)
            if (missing(format_fun))
              return(format_msframe(msframe))
            format_fun(msframe)
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

