# ==========================================================================
# functions to get export setting
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.get_export_name <- 
  function(){
    set <- c(
             mz = "m/z",
             pre.mz = "Precursor m/z",
             rt.min = "RT (min)",
             similarity = "Spectral similarity",
             tani.score = "Tanimoto similarity",
             rel.index = "Relative index",
             rel.int. = "Relative intensity",
             tracer = "Tracer",
             group = "Group"
    )
  }
