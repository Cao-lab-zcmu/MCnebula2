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
                         "function" = "fun_filter",
                         "logical" = "by_reference"),
           function(x, fun_filter, ..., by_reference)
             standardGeneric("filter_formula"))
setGeneric("filter_structure", 
           signature = c(mcnebula = "x",
                         "function" = "fun_filter",
                         "logical" = "by_reference"),
           function(x, fun_filter, ..., by_reference)
             standardGeneric("filter_structure"))
setGeneric("create_reference", 
           signature = c(mcnebula = "x",
                         "character" = "from",
                         "character" = "subscript",
                         "data.frame" = "data",
                         "vector" = "columns",
                         "logical" = "fill",
                         "list" = "MoreArgs"
                         ),
           function(x, from, subscript, data, columns, fill, MoreArgs)
             standardGeneric("create_reference"))
setGeneric("filter_ppcp", 
           signature = c(mcnebula = "x",
                         "function" = "fun_filter",
                         "logical" = "by_reference"
                         ),
           function(x, fun_filter, ..., by_reference)
             standardGeneric("filter_ppcp"))
setGeneric("create_hierarchy", 
           signature = c(mcnebula = "x", "function" = "fun_organize"),
           function(x, fun_organize) standardGeneric("create_hierarchy"))
setGeneric("create_features_annotation", 
           signature = c(mcnebula = "x", "data.frame" = "extra_data",
                         "ANY" = "column"),
           function(x, extra_data, column)
             standardGeneric("create_features_annotation"))
setGeneric("create_stardust_classes", 
           signature = c(mcnebula = "x",
                         "numeric" = "pp.threashold",
                         "numeric" = "hierarchy_priority",
                         "logical" = "position_isomerism",
                         "logical" = "inherit_dataset"
                         ),
           function(x, pp.threashold, hierarchy_priority,
                    position_isomerism, inherit_dataset)
             standardGeneric("create_stardust_classes"))
## ------------------------------------- 
setGeneric("cross_filter_stardust",
           signature = c(mcnebula = "x"),
           function(x, min_number, max_ratio,
                    types, cutoff, tolerance,
                    hierarchy_range, identical_factor)
             standardGeneric("cross_filter_stardust"))
setGeneric("cross_filter_quantity",
           signature = c(mcnebula = "x",
                         "numeric" = "min_number", "numeric" = "max_ratio"),
           function(x, min_number, max_ratio)
             standardGeneric("cross_filter_quantity"))
setGeneric("cross_filter_score",
           signature = c(mcnebula = "x", "character" = "types",
                         "numeric" = "cutoff", "numeric" = "tolerance"),
           function(x, types, cutoff, tolerance)
             standardGeneric("cross_filter_score"))
setGeneric("cross_filter_identical",
           signature = c(mcnebula = "x", "numeric" = "hierarchy_range",
                         "numeric" = "identical_factor"),
           function(x, hierarchy_range, identical_factor)
             standardGeneric("cross_filter_identical"))
setGeneric("backtrack_stardust", 
           signature = c(mcnebula = "x", "character" = "class.name",
                         "numeric" = "rel.index", "logical" = "remove"),
           function(x, class.name, rel.index, remove)
             standardGeneric("backtrack_stardust"))
## ------------------------------------- 
setGeneric("create_nebula_index", 
           signature = c(mcnebula = "x"),
           function(x) standardGeneric("create_nebula_index"))
