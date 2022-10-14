# ==========================================================================
# a class to store function and its name and args
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.command <- 
  setClass("command", 
           contains = character(),
           representation = 
             representation(command_name = "character",
                            command_function = "function",
                            command_args = "list"
                            ),
           prototype = NULL
           )
# ==========================================================================
# method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("show", 
          signature = c(object = "command"),
          function(object){
            cat(command_name(object), "\n")
            args <- vapply(command_args(object), function(v) class(v)[1], "ch")
            if (length(args) >= 1) {
              cat(paste0(paste0(rep(" ", 2), collapse = ""),
                         names(args), ": ", args), sep = "\n")
            } else {
              cat(paste0(paste0(rep(" ", 2), collapse = ""),
                         "list()"), "\n")
            }
          })
setMethod("command_name", 
          signature = c(x = "command"),
          function(x){ x@command_name })
setReplaceMethod("command_name", 
                 signature = c(x = "command"),
                 function(x, value){
                   initialize(x, command_name = value)
                 })
setMethod("command_function", 
          signature = c(x = "command"),
          function(x){ x@command_function })
setReplaceMethod("command_function", 
                 signature = c(x = "command"),
                 function(x, value){
                   initialize(x, command_function = value)
                 })
setMethod("command_args", 
          signature = c(x = "command"),
          function(x){ x@command_args })
setReplaceMethod("command_args", 
                 signature = c(x = "command"),
                 function(x, value){
                   initialize(x, command_args = value)
                 })
## ---------------------------------------------------------------------- 
setMethod("new_command", 
          signature = c(fun = "function",
                        name = "character"),
          function(fun, ..., name){
            args <- list(...)
            if (length(args) != 0) {
              args_name <- formalArgs(fun)
              if (is.null(names(args))) {
                names(args) <- args_name[1:length(args)]
              } else {
                args_name <- args_name[!args_name %in% names(args)]
                no_name_arg <- which(names(args) == "")
                names(args)[no_name_arg] <- args_name[1:length(no_name_arg)]
              }
            }
            new("command", command_name = name, command_function = fun,
                command_args = args)
          })
#' @importFrom rlang as_label
setMethod("new_command", 
          signature = setMissing("new_command",
                                 fun = "function"),
          function(fun, ...){
            name <- rlang::as_label(substitute(fun))
            if (length(name) != 1) {
              name <- paste0(name[2], name[1], name[3])
            }
            new_command(fun, ..., name = name)
          })
setMethod("call_command", 
          signature = c(x = "command"),
          function(x){
            do.call(command_function(x), command_args(x))
          })
