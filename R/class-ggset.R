# ==========================================================================
# a class to store a series of 'command' for consisting of a plot of 'ggplot'
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.ggset <- 
  setClass("ggset", 
           contains = character(),
           representation = 
             representation(layers = "list"),
           prototype = NULL
           )
# ==========================================================================
# method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("layers", 
          signature = c(x = "ggset"),
          function(x){ x@layers })
setReplaceMethod("layers", 
                 signature = c(x = "ggset"),
                 function(x, value){
                   initialize(x, layers = value)
                 })
## ------------------------------------- 
setMethod("show", 
          signature = c(object = "ggset"),
          function(object){
            show_layers(object)
          })
## ------------------------------------- 
setMethod("show_layers", 
          signature = c(x = "ggset"),
          function(x){
            layers <- layers(x)
            cat("layers of", length(layers), "\n")
            mapply(layers, 1:length(layers),
                   FUN = function(com, seq){
                     cat("  +++ layer", seq, "+++\n")
                     cat("  ", command_name(com), "\n",
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
              do.call(set_command,
                      c(command_function(command), args,
                        name = command_name(command)))
            return(x)
          })
## ------------------------------------- 
setMethod("add_layers", 
          signature = c(x = "ggset"),
          function(x, ...){
            args <- list(...)
            names(args) <- vapply(args, command_name, "ch")
            layers(x) <- c(layers(x), args)
            return(x)
          })
setMethod("delete_layers", 
          signature = c(x = "ggset", layers = "numeric"),
          function(x, layers){
            layers(x)[layers] <- NULL
            return(x)
          })
setMethod("move_layers", 
          signature = c(x = "ggset", from = "numeric", to = "numeric"),
          function(x, from, to){
            layers(x)[c(from, to)] <- layers(x)[c(to, from)]
            return(x)
          })
## ------------------------------------- 
setMethod("set_ggset", 
          signature = c(... = "ANY"),
          function(...){
            args <- list(...)
            names(args) <- vapply(args, command_name, "ch")
            new("ggset", layers = args)
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
