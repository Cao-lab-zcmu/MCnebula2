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
#' @param x [report-class] object.
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
              recepts <- c("section", "heading", "code_block", "character")
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
                     layer <- layers(x)[[ seq ]]
                     if (is.character(layer) & !is(layer, "heading"))
                       textSh(layer, pre_collapse = T, pre_trunc = T,
                              pre_wrap = T)
                     else
                       show(layer)
                   })
          })

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
#'
#' @examples
#' \dontrun{
#'   h1 <- new_heading("heading, level 1", 1)
#'   
#'   sec1 <- new_section(
#'     "sub-heading", 2,
#'     "This is a description.",
#'     new_code_block(codes = "seq <- lapply(1:10, cat)")
#'   )
#'   
#'   h2 <- new_heading("heading 2, level 1", 1)
#'   
#'   fig_block <- new_code_block_figure("plot", "this is a caption",
#'     codes = "df <- data.frame(x = 1:10, y = 1:10)
#'       p <- ggplot(df) +
#'         geom_point(aes(x = x, y = y))
#'       p"
#'   )
#'   sec2 <- new_section(
#'     "sub-heading2", 2,
#'     paste0(
#'       "This is a description. ",
#'       "See Figure ", get_ref(fig_block), "."
#'     ),
#'     fig_block
#'   )
#'   
#'   a_data <- dplyr::storms[1:15, 1:10]
#'   table_block <- include_table(a_data, "table1", "This is a caption")
#'   
#'   sec3 <- new_section(
#'     NULL, ,
#'     paste0("See Table ", get_ref(table_block, "tab"), "."),
#'     NULL
#'   )
#'   
#'   tmp_p <- paste0(tempdir(), "/test.pdf")
#'   pdf(tmp_p)
#'   plot(1:10)
#'   dev.off()
#'   fig_block_2 <- include_figure(tmp_p, "plot2", "this is a caption")
#'   sec4 <- history_rblock(, "^tmp_p <- ", "^fig_block_2")
#'   sec4
#'   
#'   ## gather
#'   yaml <- "title: 'title'\noutput:\n  bookdown::pdf_document2"
#'   report <- new_report(
#'     h1, sec1, h2, sec2,
#'     table_block, sec3,
#'     fig_block_2, sec4,
#'     yaml = yaml
#'   )
#'   report
#'   
#'   ## output
#'   tmp <- paste0(tempdir(), "/tmp_output.Rmd")
#'   writeLines(call_command(report), tmp)
#'   rmarkdown::render(tmp)
#'   file.exists(sub("Rmd$", "pdf", tmp))
#' }
setReplaceMethod("yaml", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, yaml = value)
                 })

#' @exportMethod new_report
#' @aliases new_report
#' @description \code{new_report}: Create a [report-class] object.
#' @param ... An arbitrary number of [heading-class],
#' [section-class] or [code_block-class] in sequence.
#' Specially, \code{NULL} can be passed herein, but would be ignored.
#' @param yaml character. Passed to .Rmd for setting format of documentation.
#' @rdname report-class
setMethod("new_report", 
          signature = c(yaml = "character"),
          function(..., yaml){
            layers <- list(...)
            layers <- layers[!vapply(layers, is.null, logical(1))]
            .report(yaml = yaml, layers = layers)
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
setMethod("call_command", 
          signature = c(x = "report"),
          function(x){
            yaml <- c("---", yaml(x), "---")
            layers <- unlist(lapply(layers(x), call_command))
            c(yaml, "", layers)
          })

setMethod("call_command", 
          signature = c(x = "character"),
          function(x){ 
            if (tail(x, n = 1) != "")
              c(x, "")
            else
              x
          })

# ==========================================================================
# function
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#' @export search_heading
#' @aliases search_heading
#' @description \code{search_heading}: Regex match for [heading-class] object
#' in slot \code{layers}.
#' @rdname report-class
search_heading <- 
  function(x, pattern) {
    log <- vapply(layers(x), FUN.VALUE = logical(1),
                  function(s) {
                    if (is(s, "heading"))
                      grepl(pattern, s, perl = T)
                    else F
                  })
    (1:length(layers(x)))[ log ]
  }

