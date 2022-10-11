# ==========================================================================
# functions to get export setting
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.get_export_name <- 
  function(){
    set <- c(
             mz = "m/z",
             similarity = "Spectral similarity",
             tani.score = "Tanimoto similarity",
             rel.index = "Relative index",
             group = "Group"
    )
  }
