# ==========================================================================
# generic for class methods
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setGeneric("get_metadata", 
           signature = c(ANY = "x",
                         "character" = "subscript",
                         project_metadata = "project_metadata",
                         project_conformation = "project_conformation",
                         "character" = "path"
                         ),
           function(x, subscript, project_metadata, project_conformation, path)
             standardGeneric("get_metadata"))
setGeneric("extract_metadata", 
           signature = c(ANY = "x", "character" = "subscript"),
           function(x, subscript) standardGeneric("extract_metadata"))
## ---------------------------------------------------------------------- 
setGeneric("extract_rawset", 
           signature = c("ANY" = "x", character = "subscript",
                         "function" = "fun_collate"),
           function(x, subscript, fun_collate, ...) standardGeneric("extract_rawset"))
setGeneric("extract_mcnset", 
           signature = c("ANY" = "x", character = "subscript"),
           function(x, subscript) standardGeneric("extract_mcnset"))
## ---------------------------------------------------------------------- 
setGeneric("get_upper_dir_subscript", 
           signature = c(ANY = "x",
                         character = "subscript",
                         project_conformation = "project_conformation"
                         ),
           function(x, subscript, project_conformation)
             standardGeneric("get_upper_dir_subscript"))
## ---------------------------------------------------------------------- 
setGeneric("latest", 
           function(x, slot, subscript) standardGeneric("latest"))
## ---------------------------------------------------------------------- 
## rename the colnames and check the values type (character or interger, etc.)
setGeneric("format_msframe", 
           signature = c("ANY" = "x",
                         character = "names", "function" = "fun_names",
                         character = "types", "function" = "fun_types",
                         "function" = "fun_format"
                         ),
           function(x, names, fun_names, types, fun_types, fun_format)
             standardGeneric("format_msframe"))
setGeneric("filter_msframe", 
           signature = c(msframe = "x", "function" = "fun_filter",
                         "formula" = "f"),
           function(x, fun_filter, f, ...) standardGeneric("filter_msframe"))
