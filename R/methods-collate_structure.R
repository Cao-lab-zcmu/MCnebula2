# ==========================================================================
# collate structure dataset in sirius project and do fomatting
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("collate_structure", 
          signature = c(x = "mcnebula"),
          function(x){
            subscript <- ".f3_fingerid"
            .get_info_formal("MCnebula2", "collate_structure")
            project_metadata <- extract_metadata(x, subscript)
            msframe <- read_data(x, project_metadata = project_metadata,
                                 subscript = subscript)
          })

