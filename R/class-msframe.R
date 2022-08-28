# ==========================================================================
# msframe: class based on data.frame
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.msframe <- 
  setClass("msframe", 
           contains = character(),
           representation = 
             representation(file = "character",
                            file_data = "data.frame"
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
setMethod("file", 
          signature = c(x = "msframe"),
          function(x){ x@file })
setReplaceMethod("file", 
                 signature = c(x = "msframe"),
                 function(x, value){
                   initialize(x, file = value)
                 })
## ------------------------------------- 
setMethod("file_data", 
          signature = c(x = "msframe"),
          function(x){ x@file_data })
setReplaceMethod("file_data", 
                 signature = c(x = "msframe"),
                 function(x, value){
                   initialize(x, file_data = value)
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
      rs <- which( names == file(x) & names(names) == "...sig")
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
    colnames(file_data(x)) <-
      mapply_rename_col(pattern, names(names), colnames(file_data(x)))
    ## ------------------------------------- 
    ## check attributes type
    names <- names[names(names) %in% colnames(file_data(x))]
    for (i in names(names)) {
      if (i %in% names(types))
        target_type <- types[[i]]
      else
        target_type <- "character"
      fun <- match.fun(paste0("is.", target_type))
      if ( !fun(file_data(x)[[i]]) ){
        print("test")
        print(file_data(x)[[i]])
        fun <- match.fun(paste0("as.", target_type))
        file_data(x)[[i]] <- fun(file_data(x)[[i]])
      }
    }
    return(x)
  }

