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
           signature = c(ANY = "x", "character" = "nebula_name",
                         "character" = ".features_id",
                         "data.frame" = "data"),
           function(x, nebula_name, .features_id, data, ...)
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
                         "logical" = "add_ration"),
           function(x, nebula_name, nodes_color, add_id_text,
                    add_structure, add_ppcp, add_ration)
             standardGeneric("draw_nodes"))
setGeneric("show_node", 
           signature = c(ANY = "x", "character" = ".features_id",
                         ANY = "panel_viewport", ANY = "legend_viewport"),
           function(x, .features_id, panel_viewport, legend_viewport)
             standardGeneric("show_node"))
setGeneric("set_ppcp_data", 
           signature = c(ANY = "x", "character" = "classes"),
           function(x, classes) standardGeneric("set_ppcp_data"))
setGeneric("set_ration_data", 
           signature = c(ANY = "x", "logical" = "mean"),
           function(x, mean) standardGeneric("set_ration_data"))
setGeneric("set_nodes_color", 
           function(x, attribute, extra_data, use_tracer)
             standardGeneric("set_nodes_color"))
setGeneric("set_tracer", 
           function(x, .features_id, colors, rest)
             standardGeneric("set_tracer"))
 
setGeneric("binary_comparison", 
           signature = c(ANY = "x", "formula" = "formula",
                         "function" = "fun_norm", "ANY" = "top_coef",
                         "ANY" = "contrasts"),
           function(x, ..., formula, fun_norm, top_coef, contrasts)
             standardGeneric("binary_comparison"))
 
setGeneric("get_metadata", 
           signature = c(ANY = "x",
                         "character" = "subscript",
                         project_metadata = "project_metadata",
                         project_conformation = "project_conformation",
                         "character" = "project_version",
                         "character" = "path"
                         ),
           function(x, subscript, project_metadata, project_conformation,
             project_version, path)
             standardGeneric("get_metadata"))
setGeneric("extract_metadata", 
           signature = c(ANY = "x", "character" = "subscript"),
           function(x, subscript) standardGeneric("extract_metadata"))
 
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
 
setGeneric("get_upper_dir_subscript", 
           signature = c(ANY = "x",
                         character = "subscript",
                         project_conformation = "project_conformation"
                         ),
           function(x, subscript, project_conformation)
             standardGeneric("get_upper_dir_subscript"))
 
setGeneric("latest", 
           function(x, slot, subscript) standardGeneric("latest"))
 
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

# ==========================================================================
# for report and ggset
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

setGeneric("new_command", 
           signature = c("function" = "fun",
                         "character" = "name"),
           function(fun, ..., name)
             standardGeneric("new_command"))
setGeneric("call_command", 
           function(x) standardGeneric("call_command"))
 
setGeneric("new_code_block", 
           signature = c(character = "language", "character" = "codes",
                         "list" = "args", "logical" = "prettey",
                         "function" = "fun_prettey"),
           function(language, codes, args, prettey, fun_prettey)
             standardGeneric("new_code_block"))
setGeneric("new_code_block_table", 
           signature = c(character = "name"),
           function(name, ...)
             standardGeneric("new_code_block_table"))
setGeneric("new_code_block_figure", 
           signature = c(character = "name"),
           function(name, caption, ...)
             standardGeneric("new_code_block_figure"))
setGeneric("include_table", 
           function(data, name, caption)
             standardGeneric("include_table"))
setGeneric("include_figure", 
           function(file, name, caption)
             standardGeneric("include_figure"))
setGeneric("history_rblock", 
           function(nrow, pattern_start, pattern_end, exclude)
             standardGeneric("history_rblock"))
 
setGeneric("new_heading", 
           function(heading, level)
             standardGeneric("new_heading"))
setGeneric("new_section", 
           signature = c(character = "heading", "numeric" = "level",
                         "character" = "paragraph", "ANY" = "code_block"),
           function(heading, level, paragraph, code_block)
             standardGeneric("new_section"))
setGeneric("new_report", 
           function(..., yaml)
             standardGeneric("new_report"))
 
setGeneric("new_ggset",
           function(...) standardGeneric("new_ggset"))
setGeneric("show_layers", 
           function(x) standardGeneric("show_layers"))
setGeneric("add_layers", 
           signature = c(ANY = "x"),
           function(x, ...) standardGeneric("add_layers"))
setGeneric("delete_layers", 
           signature = c(ANY = "x", "numeric" = "layers"),
           function(x, layers) standardGeneric("delete_layers"))
setGeneric("move_layers",
           signature = c(ANY = "x", "numeric" = "from", "numeric" = "to"),
           function(x, from, to)
             standardGeneric("move_layers"))
setGeneric("insert_layers", 
           signature = c(ANY = "x", "numeric" = "to"),
           function(x, to, ...) standardGeneric("insert_layers"))
setGeneric("mutate_layer", 
           signature = c(ANY = "x", "ANY" = "layer"),
           function(x, layer, ...)
             standardGeneric("mutate_layer"))
 
setGeneric("workflow", 
  function(sections, mode, envir, sirius_version, sirius_project, ion_mode, ...)
    standardGeneric("workflow"))


