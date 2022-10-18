# ==========================================================================
# a class for creating documentation for annotating analysis workflow
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.report <- 
  setClass("report", 
           contains = c("ggset"),
           representation = 
             representation(yaml = "character"
                            ),
           prototype = prototype(yaml = .yaml_default())
  )
# ==========================================================================
# method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("show_layers", 
          signature = c(x = "report"),
          function(x){
            message("\n", strsplit(yaml(x),
                                   split = "\n")[[1]][1], "\n")
          })
setMethod("yaml", 
          signature = c(x = "ANY"),
          function(x){ x@yaml })
setReplaceMethod("yaml", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, yaml = value)
                 })
