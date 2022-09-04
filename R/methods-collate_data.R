# ==========================================================================
# collate any dataset in sirius project without filtering or arranging
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("collate_data", 
          signature = c(x = "mcnebula", subscript = "character"),
          function(x, subscript){
            collate_data(x, subscript, .collate_data.msframe)
          })
setMethod("collate_data", 
          signature = c(x = "mcnebula", subscript = "character",
                        fun_collate = "function"
                        ),
          function(x, subscript, fun_collate, ...){
            x <- get_metadata(x, subscript)
            msframe.lst <- extract_rawset(x, subscript, fun_collate, ...)
            project_dataset(x) <- add_dataset(project_dataset(x), msframe.lst)
            return(x)
          })
.collate_data.msframe <- 
  function(x, subscript){
    project_metadata <- extract_metadata(x, subscript)
    msframe <- read_data(x, project_metadata = project_metadata,
                         subscript = subscript)
    return(msframe)
  }
