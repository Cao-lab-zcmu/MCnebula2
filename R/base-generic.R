# ==========================================================================
# Generic for base method of class
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## class-mcn_palette
setGeneric("mcn_palette", 
           function(x) standardGeneric("mcn_palette"))
setGeneric("mcn_palette<-", 
           function(x, value) standardGeneric("mcn_palette<-"))
## ------------------------------------- 
setGeneric("palette_set", 
           signature = c(mcn_palette = "x"),
           function(x) standardGeneric("palette_set"))
setGeneric("palette_set<-", 
           signature = c(mcn_palette = "x"),
            function(x, value) standardGeneric("palette_set<-"))
setGeneric("palette_stat", 
           signature = c(mcn_palette = "x"),
           function(x) standardGeneric("palette_stat"))
setGeneric("palette_stat<-", 
           signature = c(mcn_palette = "x"),
           function(x, value) standardGeneric("palette_stat<-"))
setGeneric("palette_ppcp", 
           signature = c(mcn_palette = "x"),
           function(x) standardGeneric("palette_ppcp"))
setGeneric("palette_ppcp<-", 
           signature = c(mcn_palette = "x"),
           function(x, value) standardGeneric("palette_ppcp<-"))
setGeneric("palette_label", 
           signature = c(mcn_palette = "x"),
           function(x) standardGeneric("palette_label"))
setGeneric("palette_label<-", 
           signature = c(mcn_palette = "x"),
           function(x, value) standardGeneric("palette_label<-"))
## ---------------------------------------------------------------------- 
## class-mcnebula
setGeneric("creation_time", 
           function(x) standardGeneric("creation_time"))
setGeneric("creation_time<-", 
           function(x, value) standardGeneric("creation_time<-"))
setGeneric("sirius_version", 
           signature = c(mcnebula = "x"),
           function(x) standardGeneric("sirius_version"))
setGeneric("sirius_version<-", 
           signature = c(mcnebula = "x"),
           function(x, value) standardGeneric("sirius_version<-"))
## ---------------------------------------------------------------------- 
## class-mcn_path
setGeneric("mcn_path", 
           function(x) standardGeneric("mcn_path"))
setGeneric("mcn_path<-", 
           function(x, value) standardGeneric("mcn_path<-"))
## ------------------------------------- 
setGeneric("sirius_project", 
           signature = c(mcn_path = "x"),
           function(x) standardGeneric("sirius_project"))
setGeneric("sirius_project<-", 
           signature = c(mcn_path = "x"),
           function(x, value) standardGeneric("sirius_project<-"))
setGeneric("output_directory", 
           signature = c(mcn_path = "x"),
           function(x) standardGeneric("output_directory"))
setGeneric("output_directory<-", 
           signature = c(mcn_path = "x"),
           function(x, value) standardGeneric("output_directory<-"))
## ---------------------------------------------------------------------- 
## class-project_conformation
setGeneric("project_conformation", 
           function(x) standardGeneric("project_conformation"))
setGeneric("project_conformation<-", 
           function(x, value) standardGeneric("project_conformation<-"))
## ------------------------------------- 
setGeneric("file_name", 
           signature = c(project_conformation = "x"),
           function(x) standardGeneric("file_name"))
setGeneric("file_name<-", 
           signature = c(project_conformation = "x"),
           function(x, value) standardGeneric("file_name<-"))
setGeneric("file_api", 
           signature = c(project_conformation = "x"),
           function(x) standardGeneric("file_api"))
setGeneric("file_api<-", 
           signature = c(project_conformation = "x"),
           function(x, value) standardGeneric("file_api<-"))
setGeneric("attribute_name", 
           signature = c(project_conformation = "x"),
           function(x) standardGeneric("attribute_name"))
setGeneric("attribute_name<-", 
           signature = c(project_conformation = "x"),
           function(x, value) standardGeneric("attribute_name<-"))
## ------------------------------------- 
setGeneric("get_upper_dir_subscript", 
           signature = c(ANY = "x",
                         character = "subscript",
                         project_conformation = "project_conformation"
                         ),
           function(x, subscript, project_conformation)
             standardGeneric("get_upper_dir_subscript"))
## ---------------------------------------------------------------------- 
## class-project_metadata
setGeneric("project_metadata", 
           function(x) standardGeneric("project_metadata"))
setGeneric("project_metadata<-", 
           function(x, value) standardGeneric("project_metadata<-"))
## ------------------------------------- 
setGeneric("metadata", 
           signature = c(project_metadata = "x"),
           function(x) standardGeneric("metadata"))
setGeneric("metadata<-", 
           signature = c(project_metadata = "x"),
           function(x, value) standardGeneric("metadata<-"))
## ------------------------------------- 
setGeneric("add", 
           signature = c(project_metadata = "x",
                         "list" = "list"
                         ),
           function(x, list) standardGeneric("add"))
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
## ------------------------------------- 
setGeneric("extract_metadata", 
           signature = c(ANY = "x", "character" = "subscript"),
           function(x, subscript) standardGeneric("extract_metadata"))
## ---------------------------------------------------------------------- 
## class-project_api
setGeneric("project_api", 
           function(x) standardGeneric("project_api"))
setGeneric("project_api<-", 
           function(x, value) standardGeneric("project_api<-"))
## ------------------------------------- 
setGeneric("read_methods", 
           signature = c(project_api = "x"),
           function(x) standardGeneric("read_methods"))
setGeneric("read_methods<-", 
           signature = c(project_api = "x"),
           function(x, value) standardGeneric("read_methods<-"))
setGeneric("format_methods", 
           signature = c(project_api = "x"),
           function(x) standardGeneric("format_methods"))
setGeneric("format_methods<-", 
           signature = c(project_api = "x"),
           function(x, value) standardGeneric("format_methods<-"))
setGeneric("match_methods", 
           signature = c(project_api = "x"),
           function(x) standardGeneric("match_methods"))
setGeneric("match_methods<-", 
           signature = c(project_api = "x"),
           function(x, value) standardGeneric("match_methods<-"))
## ---------------------------------------------------------------------- 
## class-msframe
setGeneric("msframe", 
           function(x) standardGeneric("msframe"))
setGeneric("msframe<-", 
           function(x, value) standardGeneric("msframe<-"))
## ------------------------------------- 
setGeneric("subscript", 
           signature = c(msframe = "x"),
           function(x) standardGeneric("subscript"))
setGeneric("subscript<-", 
           signature = c(msframe = "x"),
           function(x, value) standardGeneric("subscript<-"))
setGeneric("entity", 
           signature = c(msframe = "x"),
           function(x) standardGeneric("entity"))
setGeneric("entity<-", 
           signature = c(msframe = "x"),
           function(x, value) standardGeneric("entity<-"))
## ------------------------------------- 
## read data (table format)
setGeneric("read_data", 
           signature = c(ANY = "x",
                         character = "path",
                         project_metadata = "project_metadata",
                         "function" = "read_fun",
                         character = "subscript",
                         "function" = "format_fun",
                         character = ".features_id",
                         character = ".candidates_id"
                         ),
           function(x, path, project_metadata, read_fun,
                    subscript, format_fun,
                    .features_id, .candidates_id) standardGeneric("read_data"))
## ------------------------------------- 
## rename the colnames and check the values type (character or interger, etc.)
setGeneric("format_msframe", 
           signature = c(msframe = "x",
                         character = "names", "function" = "fun_names",
                         character = "types", "function" = "fun_types"
                         ),
           function(x, names, fun_names, types, fun_types)
             standardGeneric("format_msframe"))
## ---------------------------------------------------------------------- 
