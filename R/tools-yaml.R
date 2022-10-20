# ==========================================================================
# get or modify 'yaml' for 'report'
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.yaml_default <- 
  function(style = c("default", "BiocStyle", "BiocStyle_pdf")){
    style <- match.arg(style)
    readLines(system.file("extdata", paste0(style, ".yml"),
                                 package = "MCnebula2"))
  }
