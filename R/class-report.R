# ==========================================================================
# a class for creating documentation for annotating analysis workflow
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportClass report
#'
#' @aliases report
#'
#' @title Creating a formatted report
#'
#' @description
#' The report module can create output report quickly for
#' and not just for the mcnebula2 workflow.
#' The report system is primarily a class object that manages text and code blocks.
#' Heading or paragraphs or code blocks were treated as individual report units
#' and deposited sequentially in "layers".
#' The report system provides methods to exhibit, modify these layers.
#' Reports can be exported as ".Rmd" text files, and subsequently,
#' users can call [rmarkdown::render()] for output formatted documents.
#'
#' @family layerSets
#'
#' @slot yaml character. Metadata passed to .Rmd for setting format of documentation.
#' See \url{https://bookdown.org/yihui/rmarkdown/compile.html} for details.
#' @slot layers list. Element in list must be [section-class],
#' [heading-class] or [code_block-class].
#'
#' @rdname report-class
#' @order 1
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
#' @aliases new_report
#' @description \code{new_report}: Create a [report-class] object.
#' @param ... An arbitrary number of [heading-class],
#' [section-class] or [code_block-class] in sequence.
#' @param yaml character. Passed to .Rmd for setting format of documentation.
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

#' @exportMethod call_command
#' @aliases call_command
#' @description \code{call_command}: Format 'report' object as character, which can be output
#' by \code{writeLines()} function as '.Rmd' file and than use \code{rmarkdown::render} output
#' as pdf, html, or other format files.
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
