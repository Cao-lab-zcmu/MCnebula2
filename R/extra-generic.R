# ==========================================================================
# generic for class methods
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setGeneric("collate_data", 
           signature = c(ANY = "x", "character" = "subscript",
                         "function" = "fun_collate"),
           function(x, subscript, fun_collate, ...)
             standardGeneric("collate_data"))
setGeneric("read_data", 
           signature = c(ANY = "x",
                         project_metadata = "project_metadata",
                         character = "subscript",
                         character = "path",
                         character = ".features_id",
                         character = ".candidates_id",
                         "function" = "fun_read",
                         "function" = "fun_format"
                         ),
           function(x, project_metadata, subscript,
                    path, .features_id, .candidates_id,
                    fun_read, fun_format) standardGeneric("read_data"))
setGeneric("draw_structures", 
           signature = c(ANY = "x", "character" = "nebula_name"),
           function(x, nebula_name)
             standardGeneric("draw_structures"))
setGeneric("show_structure", 
           signature = c("ANY" = "x", "character" = ".features_id"),
           function(x, .features_id)
             standardGeneric("show_structure"))
setGeneric("draw_nodes", 
           signature = c(ANY = "x", "character" = "nebula_name",
                         "character" = "nodes_color",
                         "logical" = "add_id_text",
                         "logical" = "add_structure", "logical" = "add_ppcp",
                         "logical" = "add_statistic"),
           function(x, nebula_name, nodes_color, add_id_text,
                    add_structure, add_ppcp, add_statistic)
             standardGeneric("draw_nodes"))
setGeneric("set_ppcp_data", 
           signature = c(ANY = "x", "character" = "classes"),
           function(x, classes) standardGeneric("set_ppcp_data"))
setGeneric("set_statistic_data", 
           signature = c(ANY = "x", "logical" = "mean"),
           function(x, mean) standardGeneric("set_statistic_data"))
## ------------------------------------- 
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
setGeneric("add_dataset", 
           signature = c("ANY" = "x", "list" = "list"),
           function(x, list) standardGeneric("add_dataset"))
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
## ---------------------------------------------------------------------- 
setGeneric("new_command", 
           signature = c("function" = "fun",
                         "character" = "name"),
           function(fun, ..., name)
             standardGeneric("new_command"))
setGeneric("call_command", 
           function(x) standardGeneric("call_command"))
## ------------------------------------- 
setGeneric("new_ggset",
           function(...) standardGeneric("new_ggset"))
setGeneric("show_layers", 
           function(x) standardGeneric("show_layers"))
setGeneric("mutate_layer", 
           signature = c(ggset = "x", "ANY" = "layer"),
           function(x, layer, ...)
             standardGeneric("mutate_layer"))
setGeneric("add_layers", 
           signature = c(ggset = "x"),
           function(x, ...) standardGeneric("add_layers"))
setGeneric("delete_layers", 
           signature = c(ggset = "x", "numeric" = "layers"),
           function(x, layers) standardGeneric("delete_layers"))
setGeneric("move_layers",
           signature = c(ggset = "x", "numeric" = "from", "numeric" = "to"),
           function(x, from, to)
             standardGeneric("move_layers"))

