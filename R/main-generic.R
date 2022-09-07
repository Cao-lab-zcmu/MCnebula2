# ==========================================================================
# Generic for main method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setGeneric("initialize_mcnebula", 
           signature = c(ANY = "x", "character" = "sirius_version"),
           function(x, sirius_version) standardGeneric("initialize_mcnebula"))
## ------------------------------------- 
setGeneric("collate_data", 
           signature = c(mcnebula = "x", "character" = "subscript",
                         "function" = "fun_collate"),
           function(x, subscript, fun_collate, ...) standardGeneric("collate_data"))
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
## ------------------------------------- 
setGeneric("filter_formula", 
           signature = c(mcnebula = "x",
                         "function" = "fun_filter"),
           function(x, fun_filter, ...) standardGeneric("filter_formula"))
setGeneric("filter_structure", 
           signature = c(mcnebula = "x",
                         "function" = "fun_filter"),
           function(x, fun_filter, ...) standardGeneric("filter_structure"))
setGeneric("create_reference", 
           signature = c(mcnebula = "x",
                         "character" = "from",
                         "character" = "subscript",
                         "data.frame" = "data",
                         "vector" = "columns",
                         "logical" = "fill"
                         ),
           function(x, from, subscript, data, columns, fill)
             standardGeneric("create_reference"))
setGeneric("filter_ppcp", 
           signature = c(mcnebula = "x",
                         "function" = "fun_filter",
                         "logical" = "by_reference"
                         ),
           function(x, fun_filter, ..., by_reference)
             standardGeneric("filter_ppcp"))
setGeneric("get_hierarchy", 
           signature = c(mcnebula = "x", "function" = "fun_organize"),
           function(x, fun_organize) standardGeneric("get_hierarchy"))
setGeneric("summarise_stardust_classes", 
           signature = c(mcnebula = "x",
                         "numeric" = "pp.threashold",
                         "numeric" = "hierarchy_priority",
                         "logical" = "position_isomerism",
                         "logical" = "inherit_dataset"
                         ),
           function(x, pp.threashold, hierarchy_priority,
                    position_isomerism, inherit_dataset)
             standardGeneric("summarise_stardust_classes"))
setGeneric("cross_filter_stardust", 
           signature = c(mcnebula = "x"),
           function(x) standardGeneric("cross_filter_stardust"))
setGeneric("summarise_nebula_index", 
           signature = c(mcnebula = "x"),
           function(x) standardGeneric("summarise_nebula_index"))
