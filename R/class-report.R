# ==========================================================================
# a class for creating documentation for annotating analysis workflow
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportClass report
#'
#' @aliases report
#'
#' @title ...
#'
#' @description ...
#'
#' @family layerSets
#' @seealso [fun()]
#'
#' @slot yaml ...
#'
#' @rdname report-class
#'
#' @examples
#' \dontrun{
#' new('report', ...)
#' }
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
#' @exportMethod show_layers
#' @aliases show_layers
#' @description \code{show_layers}: show \code{layers} slots in a pretty
#' and readable style.
#' @rdname report-class
#' @examples
#' \dontrun{
#' show_layers(...)
#' }
setMethod("show_layers", 
          signature = c(x = "report"),
          function(x){
            text <- yaml(x)
            textSh(crayon::blue$bold(text[1]), exdent = 0)
            lapply(c(head(as.list(crayon::cyan(text[-1])), n = 4),
                     crayon::silver("...")),
                   function(text){
                     textSh(text, pre_trunc = T, trunc_width = 60,
                            ending = "")
                   })
            cat(crayon::silver("layers of", length(layers(x)), "\n\n"))
            lapply(1:length(layers(x)),
                   function(seq) {
                     cat(crayon::silver("+++ layer", seq, "+++\n"))
                     show(layers(x)[[ seq ]])
                   })
          })
## ------------------------------------- 
#' @exportMethod yaml
#' @aliases yaml
#' @description \code{yaml}, \code{yaml<-}: getter and setter
#' for the \code{yaml} slot of the object.
#' @rdname report-class
setMethod("yaml", 
          signature = c(x = "ANY"),
          function(x){ x@yaml })
#' @exportMethod yaml<-
#' @aliases yaml<-
#' @param value The value for the slot.
#' @rdname report-class
setReplaceMethod("yaml", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, yaml = value)
                 })
## ---------------------------------------------------------------------- 
#' @exportMethod new_report
#' @description \code{new_report()}: get the default parameters for the method \code{new_report}.
#' @description \code{new_report(x, ...)}: use the default parameters whatever 'missing'
#' while performing the method \code{new_report}.
#' @rdname report-class
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
#' @exportMethod new_report
#' @aliases new_report
#' @description \code{new_report}: ...
#' @param ... ...
#' @param yaml ...
#' @rdname report-class
#' @examples
#' \dontrun{
#' new_report(...)
#' }
setMethod("new_report", 
          signature = c(yaml = "character"),
          function(..., yaml){
            .report(yaml = yaml, layers = list(...))
          })
#' @exportMethod call_command
#' @aliases call_command
#' @description \code{call_command}: Format 'report' object as character, which can be output
#' by \code{writeLines()} function as '.Rmd' file and than use \code{rmarkdown::render} as
#' pdf, html, or other format files.
#' @family call_commands
#' @seealso [writeLines()], [rmarkdown::render()]...
#' @rdname report-class
#' @examples
#' \dontrun{
#' call_command(...)
#' }
setMethod("call_command", 
          signature = c(x = "report"),
          function(x){
            yaml <- c("---", yaml(x), "---")
            layers <- unlist(lapply(layers(x), call_command))
            c(yaml, "", layers)
          })
