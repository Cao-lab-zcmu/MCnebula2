# ==========================================================================
# Generic for base method (get or replace data in slots) of class
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## class-mcn_palette
setGeneric("mcn_palette", 
           function(x) standardGeneric("mcn_palette"))
setGeneric("mcn_palette<-", 
           function(x, value) standardGeneric("mcn_palette<-"))
## ------------------------------------- 
setGeneric("palette_set", 
           function(x) standardGeneric("palette_set"))
setGeneric("palette_set<-", 
            function(x, value) standardGeneric("palette_set<-"))
setGeneric("palette_stat", 
           function(x) standardGeneric("palette_stat"))
setGeneric("palette_stat<-", 
           function(x, value) standardGeneric("palette_stat<-"))
setGeneric("palette_ppcp", 
           function(x) standardGeneric("palette_ppcp"))
setGeneric("palette_ppcp<-", 
           function(x, value) standardGeneric("palette_ppcp<-"))
setGeneric("palette_label", 
           function(x) standardGeneric("palette_label"))
setGeneric("palette_label<-", 
           function(x, value) standardGeneric("palette_label<-"))
## ---------------------------------------------------------------------- 
## class-nebula
setGeneric("parent_nebula", 
           function(x) standardGeneric("parent_nebula"))
setGeneric("parent_nebula<-", 
           function(x, value) standardGeneric("parent_nebula<-"))
setGeneric("child_nebulae", 
           function(x) standardGeneric("child_nebulae"))
setGeneric("child_nebulae<-", 
           function(x, value) standardGeneric("child_nebulae<-"))
## ---------------------------------------------------------------------- 
## class-mcnebula
setGeneric("creation_time", 
           function(x) standardGeneric("creation_time"))
setGeneric("creation_time<-", 
           function(x, value) standardGeneric("creation_time<-"))
setGeneric("ion_mode", 
           function(x) standardGeneric("ion_mode"))
setGeneric("ion_mode<-", 
           function(x, value) standardGeneric("ion_mode<-"))
setGeneric("sirius_version", 
           function(x) standardGeneric("sirius_version"))
setGeneric("sirius_version<-", 
           function(x, value) standardGeneric("sirius_version<-"))
setGeneric("match.features_id", 
           function(x) standardGeneric("match.features_id"))
setGeneric("match.candidates_id", 
           function(x) standardGeneric("match.candidates_id"))
setGeneric("specific_candidate", 
           function(x) standardGeneric("specific_candidate"))
setGeneric("classification", 
           function(x) standardGeneric("classification"))
setGeneric("hierarchy", 
           function(x) standardGeneric("hierarchy"))
setGeneric("stardust_classes", 
           function(x) standardGeneric("stardust_classes"))
setGeneric("features_annotation", 
           function(x) standardGeneric("features_annotation"))
setGeneric("nebula_index", 
           function(x) standardGeneric("nebula_index"))
setGeneric("spectral_similarity", 
           function(x) standardGeneric("spectral_similarity"))
## ---------------------------------------------------------------------- 
## class-mcn_path
setGeneric("mcn_path", 
           function(x) standardGeneric("mcn_path"))
setGeneric("mcn_path<-", 
           function(x, value) standardGeneric("mcn_path<-"))
## ------------------------------------- 
setGeneric("sirius_project", 
           function(x) standardGeneric("sirius_project"))
setGeneric("sirius_project<-", 
           function(x, value) standardGeneric("sirius_project<-"))
setGeneric("output_directory", 
           function(x) standardGeneric("output_directory"))
setGeneric("output_directory<-", 
           function(x, value) standardGeneric("output_directory<-"))
## ---------------------------------------------------------------------- 
## class-project_conformation
setGeneric("project_conformation", 
           function(x) standardGeneric("project_conformation"))
setGeneric("project_conformation<-", 
           function(x, value) standardGeneric("project_conformation<-"))
## ------------------------------------- 
setGeneric("file_name", 
           function(x) standardGeneric("file_name"))
setGeneric("file_name<-", 
           function(x, value) standardGeneric("file_name<-"))
setGeneric("file_api", 
           function(x) standardGeneric("file_api"))
setGeneric("file_api<-", 
           function(x, value) standardGeneric("file_api<-"))
setGeneric("attribute_name", 
           function(x) standardGeneric("attribute_name"))
setGeneric("attribute_name<-", 
           function(x, value) standardGeneric("attribute_name<-"))
## ---------------------------------------------------------------------- 
## class-project_metadata
setGeneric("project_metadata", 
           function(x) standardGeneric("project_metadata"))
setGeneric("project_metadata<-", 
           function(x, value) standardGeneric("project_metadata<-"))
## ------------------------------------- 
setGeneric("metadata", 
           function(x) standardGeneric("metadata"))
setGeneric("metadata<-", 
           function(x, value) standardGeneric("metadata<-"))
## ---------------------------------------------------------------------- 
## class-project_api
setGeneric("project_api", 
           function(x) standardGeneric("project_api"))
setGeneric("project_api<-", 
           function(x, value) standardGeneric("project_api<-"))
## ------------------------------------- 
setGeneric("methods_read", 
           function(x) standardGeneric("methods_read"))
setGeneric("methods_read<-", 
           function(x, value) standardGeneric("methods_read<-"))
setGeneric("methods_format", 
           function(x) standardGeneric("methods_format"))
setGeneric("methods_format<-", 
           function(x, value) standardGeneric("methods_format<-"))
setGeneric("methods_match", 
           function(x) standardGeneric("methods_match"))
setGeneric("methods_match<-", 
           function(x, value) standardGeneric("methods_match<-"))
## ---------------------------------------------------------------------- 
## class-project_dataset
## class-mcn_dataset
setGeneric("project_dataset", 
           function(x) standardGeneric("project_dataset"))
setGeneric("project_dataset<-", 
           function(x, value) standardGeneric("project_dataset<-"))
setGeneric("mcn_dataset", 
           function(x) standardGeneric("mcn_dataset"))
setGeneric("mcn_dataset<-", 
           function(x, value) standardGeneric("mcn_dataset<-"))
## ---------------------------------------------------------------------- 
## class-msframe
setGeneric("msframe", 
           function(x) standardGeneric("msframe"))
setGeneric("msframe<-", 
           function(x, value) standardGeneric("msframe<-"))
## ------------------------------------- 
setGeneric("entity", 
           signature = c(msframe = "x"),
           function(x) standardGeneric("entity"))
setGeneric("entity<-", 
           signature = c(msframe = "x"),
           function(x, value) standardGeneric("entity<-"))
## ---------------------------------------------------------------------- 
## class-command
setGeneric("command", 
           function(x) standardGeneric("command"))
setGeneric("command<-", 
           function(x, value) standardGeneric("command<-"))
## ------------------------------------- 
setGeneric("command_name", 
           function(x) standardGeneric("command_name"))
setGeneric("command_name<-", 
           function(x, value) standardGeneric("command_name<-"))
setGeneric("command_function", 
           function(x) standardGeneric("command_function"))
setGeneric("command_function<-", 
           function(x, value) standardGeneric("command_function<-"))
setGeneric("command_args", 
           function(x) standardGeneric("command_args"))
setGeneric("command_args<-", 
           function(x, value) standardGeneric("command_args<-"))
## ---------------------------------------------------------------------- 
setGeneric("ggset", 
           function(x) standardGeneric("ggset"))
setGeneric("ggset<-", 
           function(x, value) standardGeneric("ggset<-"))
## ------------------------------------- 
setGeneric("layers", 
           function(x) standardGeneric("layers"))
setGeneric("layers<-", 
           function(x, value) standardGeneric("layers<-"))
## ---------------------------------------------------------------------- 
## class-VIRTUAL
setGeneric("subscript", 
           function(x) standardGeneric("subscript"))
setGeneric("subscript<-", 
           function(x, value) standardGeneric("subscript<-"))
## ------------------------------------- 
setGeneric("dataset", 
           function(x) standardGeneric("dataset"))
setGeneric("dataset<-", 
           function(x, value) standardGeneric("dataset<-"))
## ------------------------------------- 
setGeneric("reference", 
           function(x) standardGeneric("reference"))
setGeneric("reference<-", 
           function(x, value) standardGeneric("reference<-"))
## ------------------------------------- 
setGeneric("backtrack", 
           function(x) standardGeneric("backtrack"))
setGeneric("backtrack<-", 
           function(x, value) standardGeneric("backtrack<-"))
## ------------------------------------- 
setGeneric("igraph", 
           function(x) standardGeneric("igraph"))
setGeneric("igraph<-", 
           function(x, value) standardGeneric("igraph<-"))
setGeneric("tbl_graph", 
           function(x) standardGeneric("tbl_graph"))
setGeneric("tbl_graph<-", 
           function(x, value) standardGeneric("tbl_graph<-"))
setGeneric("layout_ggraph", 
           function(x) standardGeneric("layout_ggraph"))
setGeneric("layout_ggraph<-", 
           function(x, value) standardGeneric("layout_ggraph<-"))
setGeneric("grid_layout", 
           function(x) standardGeneric("grid_layout"))
setGeneric("grid_layout<-", 
           function(x, value) standardGeneric("grid_layout<-"))
setGeneric("viewports", 
           function(x) standardGeneric("viewports"))
setGeneric("viewports<-", 
           function(x, value) standardGeneric("viewports<-"))
setGeneric("panel_viewport", 
           function(x) standardGeneric("panel_viewport"))
setGeneric("panel_viewport<-", 
           function(x, value) standardGeneric("panel_viewport<-"))
setGeneric("legend_viewport", 
           function(x) standardGeneric("legend_viewport"))
setGeneric("legend_viewport<-", 
           function(x, value) standardGeneric("legend_viewport<-"))
## ------------------------------------- 
