# ==========================================================================
# a class for creating documentation for annotating analysis workflow
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.report <- 
  setClass("report", 
           contains = c("layerSet"),
           representation = 
             representation(yaml = "character"),
           prototype = prototype(yaml = .yaml_default())
  )
# ==========================================================================
# validity
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setValidity("report", 
            function(object){
              recepts <- c("section", "heading", "code_block")
              tip <- paste0("'layer' in 'report' must either be: ",
                            paste0("'", recepts, "'", collapse = ", "))
              validate_class_in_list(layers(object), recepts, tip)
            })
# ==========================================================================
# method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @importFrom crayon silver
#' @importFrom crayon blue
#' @importFrom crayon cyan
setMethod("show_layers", 
          signature = c(x = "report"),
          function(x){
            text <- strsplit(yaml(x), split = "\n")[[1]]
            textSh(crayon::blue$bold(text[1]), exdent = 0)
            lapply(c(head(as.list(crayon::cyan(text[-1])), n = 4),
                     crayon::silver("...")),
                   function(text){
                     textSh(text, pre_trunc = T, trunc_width = 60,
                            ending = "")
                   })
            lapply(layers(x), show)
          })
setMethod("yaml", 
          signature = c(x = "ANY"),
          function(x){ x@yaml })
setReplaceMethod("yaml", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, yaml = value)
                 })
## ---------------------------------------------------------------------- 
setMethod("new_report", 
          signature = setMissing("new_report",
                                 yaml = "missing"),
          function(...){
            args <- list(yaml = .yaml_default())
            if (missing(...))
              return(args)
            else
              reCallMethod("new_report", args, ...)
          })
setMethod("new_report", 
          signature = c(yaml = "character"),
          function(..., yaml){
            .report(yaml = yaml, layers = list(...))
          })
