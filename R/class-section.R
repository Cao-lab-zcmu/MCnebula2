# ==========================================================================
# a class contains slots for building a text section with code block, 
# figure, and table
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportClass code_block
#'
#' @aliases code_block
#'
#' @title Sequestrate code and setting run parameters.
#'
#' @description Mainly desiged for R code block.
#' The job of this class object is to record the codes and the running parameters
#' of its source language or program;
#' These information can then be output as formatted code block text (use [call_command()]).
#'
#' @slot codes character. Codes.
#' @slot command_name character(1). Program or language. e.g., "r".
#' @slot command_function function. Used for gather the codes and args as code block.
#' @slot command_args list. Args passed to program.
#'
#' @seealso \code{\link{command-class}}.
#' \url{https://bookdown.org/yihui/rmarkdown-cookbook/cross-ref.html#cross-ref}.
#' \url{https://bookdown.org/yihui/rmarkdown/compile.html}.
#'
#' @rdname code_block-class
#' @order 1
#'
#' @examples
#' \dontrun{
#' new('code_block', ...)
#' }
.code_block <- 
  setClass("code_block", 
           contains = c("command"),
           representation = 
             representation(codes = "character"),
           prototype =
             prototype(command_name = "r",
                       command_function = .write_block,
                       command_args = .args_r_block(),
                       codes = "## codes"
           )
  )
#' @exportClass code_block_table
#'
#' @aliases code_block_table
#'
#' @description \code{code_block_table}: class inherit from \code{code_block}, with
#' default values for slot \code{command_args} facilitate showing table in document.
#'
#' @rdname code_block-class
#'
#' @examples
#' \dontrun{
#' new('code_block_table', ...)
#' }
.code_block_table <- 
  setClass("code_block_table", 
           contains = c("code_block"),
           representation = 
             representation(),
           prototype = prototype(command_args = .args_r_block_table())
           )
#' @exportClass code_block_figure
#'
#' @aliases code_block_figure
#'
#' @description \code{code_block_figure}: class inherit from \code{code_block}, with
#' default values for slot \code{command_args} facilitate showing figure in document.
#'
#' @rdname code_block-class
#'
#' @examples
#' \dontrun{
#' new('code_block_figure', ...)
#' }
.code_block_figure <- 
  setClass("code_block_figure", 
           contains = c("code_block"),
           representation = 
             representation(),
           prototype = prototype(command_args = .args_r_block_figure())
           )
#' @exportClass heading
#'
#' @aliases heading
#'
#' @description This is a class object used to clarify the heading and its hierarchy.
#'
#' @slot .Data character(1). Text of heading.
#' @slot level numeric. Level of heading.
#'
#' @rdname section-class
#'
#' @examples
#' \dontrun{
#' new('heading', ...)
#' }
.heading <- 
  setClass("heading", 
           contains = "character",
           representation = 
             representation(level = "numeric"),
           prototype = prototype(level = 2)
           )
#' @exportClass section
#'
#' @aliases section
#'
#' @title Basic cells in the report
#'
#' @description A class object consist of [heading-class], paragraph (character),
#' and [code_block-class]. These [section-class] belong to basic cells of report.
#'
#' @slot heading [heading-class] object.
#' @slot paragraph character. Text for description.
#' @slot code_block [code_block-class] object.
#'
#' @rdname section-class
#' @order 1
#'
#' @examples
#' \dontrun{
#' new('section', ...)
#' }
.section <- 
  setClass("section", 
           contains = character(),
           representation = 
             representation(heading = "ANY",
                            paragraph = "character",
                            code_block = "ANY"
                            ),
           prototype = prototype(heading = .heading("An analysis step"),
                                 paragraph = "Description",
                                 code_block = .code_block())
           )
setClassUnion("maybe_code_block", c("code_block", "NULL"))
# ==========================================================================
# method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @importFrom crayon silver
#' @exportMethod show
#' @aliases show
#' @rdname code_block-class
setMethod("show", 
          signature = c(object = "code_block"),
          function(object){
            content <- call_command(object)
            content <- lapply(content,
                            function(text){
                              if (grepl("#\\s*#", text))
                                crayon::silver(text)
                              else
                                text
                            })
            content[1] <- .text_fold(content[1], width = 60)
            if (length(content) > 4) {
              content <- c(content[1:3], crayon::silver(" + <codes-fold> + "),
                         tail(content, n = 1))
            }
            textSh(content, pre_collapse = T)
          })
#' @exportMethod show
#' @aliases show
#' @rdname code_block-class
setMethod("show", 
          signature = c(object = "code_block_table"),
          function(object){
            textSh(crayon::silver("use for cross-referencing:",
                                  get_ref(object, "tab")), ending = NULL)
            selectMethod("show", "code_block")@.Data(object)
            if (!grepl("kable\\([^\\(]*caption", codes(object))) {
              textSh(crayon::silver("make sure the codes contain:",
                                    "`knitr::kable(..., caption = '...')`"))
            }
          })
#' @exportMethod show
#' @aliases show
#' @rdname code_block-class
setMethod("show", 
          signature = c(object = "code_block_figure"),
          function(object){
            textSh(crayon::silver("use for cross-referencing:",
                                  get_ref(object, "fig")), ending = NULL)
            selectMethod("show", "code_block")@.Data(object)
          })
#' @importFrom crayon green
#' @exportMethod show
#' @aliases show
#' @rdname code_block-class
setMethod("show", 
          signature = c(object = "heading"),
          function(object){
            textSh(crayon::green$bold(call_command(object)), exdent = 0)
          })
#' @exportMethod show
#' @aliases show
#' @rdname code_block-class
setMethod("show", 
          signature = c(object = "section"),
          function(object){
            nshow(heading(object))
            textSh(paragraph(object),
                   pre_collapse = T, pre_trunc = T, pre_wrap = T)
            nshow(code_block(object))
          })
## ---------------------------------------------------------------------- 
## ---------------------------------------------------------------------- 
#' @exportMethod code_block
#' @aliases code_block
#' @description \code{code_block}, \code{code_block<-}: getter and setter
#' for the \code{code_block} slot of the object.
#' @rdname code_block-class
setMethod("code_block", 
          signature = c(x = "ANY"),
          function(x){ x@code_block })
#' @exportMethod code_block<-
#' @aliases code_block<-
#' @param value The value for the slot.
#' @rdname code_block-class
setReplaceMethod("code_block", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, code_block = value)
                 })
## ------------------------------------- 
#' @exportMethod codes
#' @aliases codes
#' @description \code{codes}, \code{codes<-}: getter and setter
#' for the \code{codes} slot of the object.
#' @rdname code_block-class
setMethod("codes", 
          signature = c(x = "code_block"),
          function(x){ x@codes })
#' @exportMethod codes<-
#' @aliases codes<-
#' @param value The value for the slot.
#' @rdname code_block-class
setReplaceMethod("codes", 
                 signature = c(x = "code_block"),
                 function(x, value){
                   initialize(x, codes = value)
                 })
## ------------------------------------- 
#' @exportMethod new_code_block
#' @aliases new_code_block
#' @description \code{new_code_block}: create a [code_block-class] object.
#' @param language character(1). For slot \code{command_name}.
#' @param codes character. For slot \code{codes}.
#' @param args list. For slot \code{command_args}.
#' @param prettey logical. If ture, use [styler::style_text()] to pretty the codes.
#' @param fun_prettey function. Default is \code{styler::style_text}.
#' @rdname code_block-class
#' @examples
#' \dontrun{
#' new_code_block(...)
#' }
setMethod("new_code_block", 
          signature = c(language = "character", codes = "character",
                        args = "list", prettey = "logical",
                        fun_prettey = "function"),
          function(language, codes, args, prettey, fun_prettey){
            if (length(codes) > 1)
              codes <- paste0(codes, collapse = "\n")
            if (prettey) {
              codes <- paste0(fun_prettey(codes), collapse = "\n")
            }
            .code_block(command_name = language, codes = codes,
                        command_args = args)
          })
## ------------------------------------- 
#' @exportMethod new_code_block
#' @description \code{new_code_block()}: get the default parameters
#' for the method \code{new_code_block}.
#' @rdname code_block-class
setMethod("new_code_block", 
          signature = setMissing("new_code_block",
                                 x = "missing"),
          function(){
            list(language = "r",
                 codes = "## codes",
                 args = .args_r_block(),
                 prettey = T,
                 fun_prettey = styler::style_text
            )
          })
#' @exportMethod new_code_block
#' @description \code{new_code_block(x, ...)}: use the default parameters whatever 'missing'
#' while performing the method \code{new_code_block}.
#' @rdname code_block-class
setMethod("new_code_block", 
          signature = c(language = "ANY"),
          function(language, codes, args, prettey, fun_prettey){
            reCallMethod("new_code_block", .fresh_param(new_code_block()))
          })
## ------------------------------------- 
#' @exportMethod new_code_block_figure
#' @aliases new_code_block_figure
#' @description \code{new_code_block_figure}: create [code_block_figure-class] object.
#' This methods simplified parameter settings for displaying figures in documents.
#' @param name character(1). For cross-reference in document.
#' See \url{https://bookdown.org/yihui/rmarkdown-cookbook/cross-ref.html#cross-ref}.
#' @param caption character(1). Caption of figure display in document.
#' @param ... Other parameters passed to [new_code_block()].
#' @rdname code_block-class
#' @examples
#' \dontrun{
#' new_code_block_figure(...)
#' }
setMethod("new_code_block_figure", 
          signature = c(name = "character"),
          function(name, caption, ...){
            args <- .fresh_param(new_code_block(), list(...))
            args$args$fig.cap <- caption
            args$language <- paste0("r ", name)
            as(do.call(new_code_block, args), "code_block_figure")
          })
#' @exportMethod new_code_block_table
#' @aliases new_code_block_table
#' @description \code{new_code_block_table}: create [code_block_table-class] object.
#' This methods simplified parameter settings for displaying table in documents.
#' @rdname code_block-class
#' @examples
#' \dontrun{
#' new_code_block_table(...)
#' }
setMethod("new_code_block_table", 
          signature = c(name = "character"),
          function(name, ...){
            args <- .fresh_param(new_code_block(), list(...))
            args$language <- paste0("r ", name)
            as(do.call(new_code_block, args), "code_block_table")
          })
## ------------------------------------- 
#' @exportMethod call_command
#' @aliases call_command
#' @description \code{call_command}: Format 'code_block' object as character.
#' @family call_commands
#' @rdname code_block-class
#' @examples
#' \dontrun{
#' call_command(...)
#' }
setMethod("call_command", 
          signature = c(x = "code_block"),
          function(x){
            do.call(command_function(x),
                    c(command_name = command_name(x),
                      command_args(x), codes = codes(x)))
          })
## ---------------------------------------------------------------------- 
## ---------------------------------------------------------------------- 
#' @exportMethod heading
#' @aliases heading
#' @description \code{heading}, \code{heading<-}: getter and setter
#' for the \code{heading} slot of the object.
#' @rdname section-class
setMethod("heading", 
          signature = c(x = "ANY"),
          function(x){ x@heading })
#' @exportMethod heading<-
#' @aliases heading<-
#' @param value The value for the slot.
#' @rdname section-class
setReplaceMethod("heading", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, heading = value)
                 })
#' @exportMethod level
#' @aliases level
#' @description \code{level}, \code{level<-}: getter and setter
#' for the \code{level} slot of the object.
#' @rdname section-class
setMethod("level", 
          signature = c(x = "heading"),
          function(x){ x@level })
#' @exportMethod level<-
#' @aliases level<-
#' @param value The value for the slot.
#' @rdname section-class
setReplaceMethod("level", 
                 signature = c(x = "heading"),
                 function(x, value){
                   initialize(x, level = value)
                 })
#' @exportMethod new_heading
#' @aliases new_heading
#' @description \code{new_heading}: create [heading-class] object.
#' @param heading character(1). For slot \code{.Data}.
#' @param level numeric(1). For slot \code{level}.
#' @rdname section-class
#' @examples
#' \dontrun{
#' new_heading(...)
#' }
setMethod("new_heading", 
          signature = c(heading = "character",
                        level = "numeric"),
          function(heading, level){
            .heading(heading, level = level)
          })
#' @exportMethod call_command
#' @aliases call_command
#' @description \code{call_command}: Format 'heading' object as character.
#' @family call_commands
#' @rdname section-class
#' @examples
#' \dontrun{
#' call_command(...)
#' }
setMethod("call_command", 
          signature = c(x = "heading"),
          function(x){
            paste0(paste0(rep("#", level(x)), collapse = ""),
                   " ", x)
          })
## ---------------------------------------------------------------------- 
## ---------------------------------------------------------------------- 
#' @exportMethod paragraph
#' @aliases paragraph
#' @description \code{paragraph}, \code{paragraph<-}: getter and setter
#' for the \code{paragraph} slot of the object.
#' @rdname section-class
setMethod("paragraph", 
          signature = c(x = "section"),
          function(x){ x@paragraph })
#' @exportMethod paragraph<-
#' @aliases paragraph<-
#' @param value The value for the slot.
#' @rdname section-class
setReplaceMethod("paragraph", 
                 signature = c(x = "section"),
                 function(x, value){
                   initialize(x, paragraph = value)
                 })
#' @exportMethod new_section
#' @description \code{new_section()}: get the default parameters for
#' the method \code{new_section}.
#' @rdname section-class
setMethod("new_section", 
          signature = setMissing("new_section",
                                 heading = "missing"),
          function(heading, level, paragraph, code_block){
            list(heading = "heading",
                 level = 2,
                 paragraph = "Description",
                 code_block = .code_block()
            )
          })
#' @exportMethod new_section
#' @description \code{new_section(x, ...)}: use the default parameters whatever 'missing'
#' while performing the method \code{new_section}.
#' @rdname section-class
setMethod("new_section", 
          signature = c(heading = "ANY"),
          function(heading, level, paragraph, code_block){
            args <- .fresh_param(new_section())
            reCallMethod("new_section", args)
          })
#' @exportMethod new_section
#' @aliases new_section
#' @description \code{new_section}: create [section-class] object.
#' @param paragraph character. Text for description.
#' @param code_block [code_block-class] object.
#' @rdname section-class
#' @examples
#' \dontrun{
#' new_section(...)
#' }
setMethod("new_section", 
          signature = c(heading = "character", level = "numeric",
                        paragraph = "character", code_block = "maybe_code_block"),
          function(heading, level, paragraph, code_block){
            .section(heading = .heading(heading, level = level),
                     paragraph = paragraph, code_block = code_block)
          })
#' @exportMethod new_section
#' @aliases new_section
#' @rdname section-class
setMethod("new_section", 
          signature = c(heading = "NULL", level = "numeric",
                        paragraph = "character", code_block = "maybe_code_block"),
          function(heading, level, paragraph, code_block){
            .section(heading = NULL, paragraph = paragraph, code_block = code_block)
          })
#' @exportMethod call_command
#' @aliases call_command
#' @description \code{call_command}: Format 'section' object as character.
#' @rdname section-class
#' @examples
#' \dontrun{
#' call_command(...)
#' }
setMethod("call_command", 
          signature = c(x = "section"),
          function(x){
            .part(call_command(heading(x)),
                  paragraph(x),
                  call_command(code_block(x))
            )
          })
## ---------------------------------------------------------------------- 
## ---------------------------------------------------------------------- 
#' @exportMethod call_command
#' @aliases call_command
#' @rdname section-class
setMethod("call_command", 
          signature = c(x = "NULL"),
          function(x){
            return()
          })
