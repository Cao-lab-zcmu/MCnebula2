# ==========================================================================
# algorithmic functions used in methods-*.R files
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.rank_by_csi.score <- 
  function(df){
    head( dplyr::arrange(df, desc(csi.score)), n = 1)
  }
.rank_by_default <- 
  function(df){
    head(df, n = 1)
  }
.filter_ppcp_by_threashold <- 
  function(df, pp.threashold = 0.5){
    dplyr::filter(df, pp.value > pp.threashold)
  }

