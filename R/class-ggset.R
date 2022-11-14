# ==========================================================================
# a class to store a series of 'command' for consisting of a plot of 'ggplot'
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportClass ggset
#'
#' @aliases ggset
#'
#' @title Management for 'ggplot' visualzation
#'
#' @description
#' Let each packed "ggplot2" function (packed as [command-class] object)
#' into layers in sequence, allowing post modifications programmatically
#' and visualizing as "ggplot2" plot at any time.
#'
#' @family layerSets
#'
#' @slot layers list with names. Each element of list must be a [command-class] object
#' packed 'ggplot2' function and its args.
#'
#' @rdname ggset-class
#' @order 1
#'
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
#' @exportMethod show_layers
#' @aliases show_layers
#' @description \code{show_layers}: show functions and parameters in layers
#' with a pretty and readable form.
#' @rdname ggset-class
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
#' @exportMethod new_ggset
#' @aliases new_ggset
#' @description \code{new_ggset}: Simplified creation of [ggset-class] object.
#' @param ... An arbitrary number of [command-class] object.
#' @rdname ggset-class
#'
#' @examples
#' \dontrun{
#'   data <- data.frame(x = 1:10, y = 1:10)
#'   layer1 <- new_command(ggplot, data)
#'   layer2 <- new_command(geom_point, aes(x = x, y = y))
#'   layer3 <- new_command(labs, x = "x label", y = "y label")
#'   layer4 <- new_command(theme, text = element_text(family = "Times"))
#'   
#'   ## gather
#'   ggset <- new_ggset(layer1, layer2, layer3, layer4)
#'   ggset
#'   ## visualize
#'   p <- call_command(ggset)
#'   p
#'   
#'   ## add layers
#'   layer5 <- new_command(
#'     geom_text,
#'     aes(x = x, y = y, label = paste0("label_", x))
#'   )
#'   layer6 <- new_command(ggtitle, "this is title")
#'   ggset <- add_layers(ggset, layer5, layer6)
#'   call_command(ggset)
#'   
#'   ## delete layers
#'   ggset <- delete_layers(ggset, 5:6)
#'   call_command(ggset)
#'   
#'   ## mutate layer
#'   ggset <- mutate_layer(ggset, "theme",
#'     legend.position = "none",
#'     plot.background = element_rect(fill = "red")
#'   )
#'   ggset <- mutate_layer(ggset, "geom_point",
#'     mapping = aes(x = x, y = y, color = x)
#'   )
#'   call_command(ggset)
#' }
setMethod("new_ggset", 
          signature = c(... = "ANY"),
          function(...){
            args <- list(...)
            names(args) <- vapply(args, command_name, "ch")
            new("ggset", layers = args)
          })

#' @exportMethod mutate_layer
#' @aliases mutate_layer
#' @description \code{mutate_layer}:
#' Pass new parameters or modify pre-existing parameters to the packed function.
#' @param x [ggset-class] object
#' @param layer numeric(1) or character(1). If "character", the name must be unique
#' in slot \code{layers}.
#' @param ... parameters passed to the layer.
#' @rdname ggset-class
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
#' @exportMethod mutate_layer
#' @aliases mutate_layer
#' @rdname ggset-class
setMethod("mutate_layer", 
          signature = c(x = "ggset", layer = "character"),
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
#' @exportMethod call_command
#' @aliases call_command
#' @description \code{call_command}: plot as 'ggplot' object.
#' @family call_commands
#' @rdname ggset-class
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
