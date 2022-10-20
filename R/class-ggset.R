# ==========================================================================
# a class to store a series of 'command' for consisting of a plot of 'ggplot'
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.ggset <- 
  setClass("ggset", 
           contains = c("layerSet"),
           representation = representation(),
           prototype = NULL
           )
# ==========================================================================
# validity
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setValidity("ggset", 
            function(object){
              recepts <- c("command")
              tip <- paste0("'layer' in 'ggset' must be: ",
                            paste0("'", recepts, "'", collapse = ", "))
              validate_class_in_list(layers(object), recepts, tip)
            })
# ==========================================================================
# method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @importFrom crayon silver
#' @importFrom crayon yellow
setMethod("show_layers", 
          signature = c(x = "ggset"),
          function(x){
            layers <- layers(x)
            cat(crayon::silver("layers of", length(layers), "\n"))
            mapply(layers, 1:length(layers),
                   FUN = function(com, seq){
                     cat(crayon::silver("  +++ layer", seq, "+++\n"))
                     cat("  ", crayon::yellow(command_name(com)), "\n",
                         rep(" ", 4), "Args:\n", sep = "")
                     args <- vapply(command_args(com), function(v) class(v)[1], "ch")
                     if (length(args) >= 1) {
                       cat(paste0(paste0(rep(" ", 6), collapse = ""),
                                  names(args), ": ", args), sep = "\n")
                     } else {
                       cat(paste0(paste0(rep(" ", 6), collapse = ""),
                                  "list()"), "\n")
                     }
                     cat("\n")
                   })
            cat("\n")
          })
## ------------------------------------- 
setMethod("new_ggset", 
          signature = c(... = "ANY"),
          function(...){
            args <- list(...)
            names(args) <- vapply(args, command_name, "ch")
            new("ggset", layers = args)
          })
setMethod("mutate_layer", 
          signature = c(x = "ggset",
                        layer = "numeric"),
          function(x, layer, ...){
            args <- list(...)
            command <- layers(x)[[ layer ]]
            old <- command_args(command)
            if (length(old) > 0) {
              args <- list_unique_by_names(c(args, old))
            }
            layers(x)[[ layer ]] <- 
              do.call(new_command,
                      c(command_function(command), args,
                        name = command_name(command)))
            return(x)
          })
## ------------------------------------- 
setMethod("mutate_layer", 
          signature = c(x = "ANY", layer = "character"),
          function(x, layer, ...){
            seq <- which(names(layers(x)) == layer)
            if (length(seq) == 0) {
              stop( paste0("'", layer, "' not found") )
            } else if (length(seq) > 1) {
              stop(paste0("multiple layers of '", layer, "' were found"))
            } else {
              x <- mutate_layer(x, seq, ...)
            }
            return(x)
          })
## ------------------------------------- 
setMethod("call_command", 
          signature = c(x = "ggset"),
          function(x){
            layers <- layers(x)
            for (i in 1:length(layers)) {
              res <- try( call_command(layers[[i]]), silent = T )
              if (inherits(res, "try-error")) {
                stop(paste0("the 'command' named '", command_name(layers[[i]]),
                            "' (sequence:", i, ") in `layers(x)` caused error."))
              }
              if (i == 1) {
                p <- call_command(layers[[1]])
              } else {
                p <- p + res
              }
            }
            return(p)
          })
