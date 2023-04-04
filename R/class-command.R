# ==========================================================================
# a class to store function and its name and args
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportClass command
#'
#' @aliases command
#'
#' @title Preparation of an instruction to be executed
#'
#' @description Packing the funciton and the args inside this class object,
#' so that it can be performed easily at any time.
#'
#' @family call_commands
#'
#' @slot command_name character(1). Describe the command name.
#' @slot command_function function.
#' @slot command_args the parameters passed to the function.
#'
#' @rdname command-class
#'
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
#' @exportMethod show
#' @aliases show
#' @rdname command-class
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

#' @exportMethod command_name
#' @aliases command_name
#' @description \code{command_name}, \code{command_name<-}: getter and setter
#' for the \code{command_name} slot of the object.
#' @rdname command-class
setMethod("command_name", 
          signature = c(x = "command"),
          function(x){ x@command_name })

#' @exportMethod command_name<-
#' @aliases command_name<-
#' @param value The value for the slot.
#' @rdname command-class
#'
#' @examples
#' \dontrun{
#'   ## example 1
#'   com <- new_command(plot, x = 1:10)
#'   com
#'   call_command(com)
#'   
#'   ## example 2
#'   com <- new_command(data.frame, x = 1:10, y = 1:10, z = 1:10)
#'   call_command(com)
#'   
#'   ## example 3
#'   data <- data.frame(x = 1:10, y = 1:10)
#'   com1 <- new_command(ggplot, data)
#'   com2 <- new_command(geom_point, aes(x = x, y = y))
#'   call_command(com1) + call_command(com2)
#'   
#'   ## slots
#'   command_name(com)
#'   command_args(com)
#'   command_function(com)
#' }
setReplaceMethod("command_name", 
                 signature = c(x = "command"),
                 function(x, value){
                   initialize(x, command_name = value)
                 })

#' @exportMethod command_function
#' @aliases command_function
#' @description \code{command_function}, \code{command_function<-}: getter and setter
#' for the \code{command_function} slot of the object.
#' @rdname command-class
setMethod("command_function", 
          signature = c(x = "command"),
          function(x){ x@command_function })

#' @exportMethod command_function<-
#' @aliases command_function<-
#' @param value The value for the slot.
#' @rdname command-class
setReplaceMethod("command_function", 
                 signature = c(x = "command"),
                 function(x, value){
                   initialize(x, command_function = value)
                 })

#' @exportMethod command_args
#' @aliases command_args
#' @description \code{command_args}, \code{command_args<-}: getter and setter
#' for the \code{command_args} slot of the object.
#' @rdname command-class
setMethod("command_args", 
          signature = c(x = "command"),
          function(x){ x@command_args })

#' @exportMethod command_args<-
#' @aliases command_args<-
#' @param value The value for the slot.
#' @rdname command-class
setReplaceMethod("command_args", 
                 signature = c(x = "command"),
                 function(x, value){
                   initialize(x, command_args = value)
                 })


#' @exportMethod new_command
#' @aliases new_command
#' @description \code{new_command}: create an object of [command-class].
#' @param fun function.
#' @param ... parameters (with names or without names) passed to the function.
#' @param name character(1). Name to slot \code{command_name}.
#' @rdname command-class
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
#' @exportMethod new_command
#' @aliases new_command
#' @rdname command-class
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

#' @exportMethod call_command
#' @aliases call_command
#' @description \code{call_command}: Execute the function (slot \code{command_function})
#' with the parameters (slot \code{command_args}).
#' @family call_commands
#' @rdname command-class
setMethod("call_command", 
          signature = c(x = "command"),
          function(x){
            do.call(command_function(x), command_args(x))
          })
