# ==========================================================================
# Generic for main method supplied by MCnebula2
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setGeneric("initialize_mcnebula", 
           signature = c(ANY = "x",
                         "character" = "sirius_version",
                         "character" = "sirius_project",
                         "character" = "output_directory"
                         ),
           function(x, sirius_version, sirius_project, output_directory)
             standardGeneric("initialize_mcnebula"))
 
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
                         "numeric" = "pp.threshold",
                         "numeric" = "hierarchy_priority",
                         "logical" = "position_isomerism",
                         "logical" = "inherit_dataset"
                         ),
           function(x, pp.threshold, hierarchy_priority,
                    position_isomerism, inherit_dataset)
             standardGeneric("create_stardust_classes"))
 
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
 
setGeneric("create_nebula_index", 
           signature = c(mcnebula = "x", "logical" = "force"),
           function(x, force)
             standardGeneric("create_nebula_index"))
 
setGeneric("compute_spectral_similarity", 
           signature = c(mcnebula = "x",
                         "logical" = "within_nebula",
                         "logical" = "recompute",
                         "ANY" = "sp1",
                         "ANY" = "sp2"),
           function(x, within_nebula, recompute, sp1, sp2)
             standardGeneric("compute_spectral_similarity"))
 
setGeneric("create_parent_nebula", 
           signature = c(mcnebula = "x",
                         "numeric" = "edge_cutoff",
                         "numeric" = "max_edge_number",
                         "logical" = "remove_isolate"),
           function(x, edge_cutoff, max_edge_number, remove_isolate)
             standardGeneric("create_parent_nebula"))
setGeneric("create_child_nebulae", 
           signature = c(mcnebula = "x",
                         "numeric" = "edge_cutoff",
                         "numeric" = "max_edge_number",
                         "logical" = "use_tracer"),
           function(x, edge_cutoff, max_edge_number, use_tracer)
             standardGeneric("create_child_nebulae"))
 
setGeneric("create_parent_layout", 
           signature = c(mcnebula = "x", "character" = "ggraph_layout",
                         "numeric" = "seed"),
           function(x, ggraph_layout, seed)
             standardGeneric("create_parent_layout"))
setGeneric("create_child_layouts", 
           signature = c(mcnebula = "x", "character" = "ggraph_layouts",
                         "numeric" = "seeds", "ANY" = "grid_layout",
                         "list" = "viewports", "ANY" = "panel_viewport",
                         "ANY" = "legend_viewport"),
           function(x, ggraph_layouts, seeds, grid_layout,
                    viewports, panel_viewport, legend_viewport)
             standardGeneric("create_child_layouts"))
 
setGeneric("activate_nebulae", 
           signature = c(mcnebula = "x",
                         "function" = "fun_default_parent",
                         "function" = "fun_default_child"),
           function(x, fun_default_parent, fun_default_child)
             standardGeneric("activate_nebulae"))
 
setGeneric("visualize", 
           function(x, item, fun_modify, annotate)
             standardGeneric("visualize"))
setGeneric("visualize_all", 
           signature = c("ANY" = "x", "logical" = "newpage",
                         "ANY" = "fun_modify",
                         "ANY" = "legend_hierarchy"),
           function(x, newpage, fun_modify, legend_hierarchy)
             standardGeneric("visualize_all"))
 
setGeneric("annotate_nebula", 
           function(x, nebula_name)
             standardGeneric("annotate_nebula"))
