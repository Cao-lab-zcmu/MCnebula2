# ==========================================================================
# get or modify 'yaml' for 'report'
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.yaml_default <- 
  function(){
    paste0(readLines(system.file("extdata", "default.yml",
                                 package = "MCnebula2")),
           collapse = "\n")
  }
