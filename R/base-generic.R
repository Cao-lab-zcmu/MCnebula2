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
## ---------------------------------------------------------------------- 
setGeneric("get_metadata", 
           signature = c("character" = "x",
                         ANY = "db",
                         project_metadata = "meta",
                         project_conformation = "api",
                         "character" = "path"
                         ),
           function(x, meta, api, path, db) standardGeneric("get_metadata"))
## ---------------------------------------------------------------------- 
## class-msframe
setGeneric("msframe", 
           function(x) standardGeneric("msframe"))
setGeneric("msframe<-", 
           function(x, value) standardGeneric("msframe<-"))
## ------------------------------------- 
setGeneric("file", 
           signature = c(msframe = "x"),
           function(x) standardGeneric("file"))
setGeneric("file<-", 
           signature = c(msframe = "x"),
           function(x, value) standardGeneric("file<-"))
setGeneric("file_data", 
           signature = c(msframe = "x"),
           function(x) standardGeneric("file_data"))
setGeneric("file_data<-", 
           signature = c(msframe = "x"),
           function(x, value) standardGeneric("file_data<-"))
## ------------------------------------- 
## rename the colnames
setGeneric("format_msframe", 
           signature = c(msframe = "x",
                         character = "names", "function" = "fun_names",
                         character = "types", "function" = "fun_types"
                         ),
           function(x, names, fun_names, types, fun_types)
             standardGeneric("format_msframe"))
## ---------------------------------------------------------------------- 
