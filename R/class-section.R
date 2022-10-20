# ==========================================================================
# a class contains slots for building a text section with code block, 
# figure, and table
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
.code_block_table <- 
  setClass("code_block_table", 
           contains = c("code_block"),
           representation = 
             representation(),
           prototype = prototype(command_args = .args_r_block_table())
           )
.code_block_figure <- 
  setClass("code_block_figure", 
           contains = c("code_block"),
           representation = 
             representation(),
           prototype = prototype(command_args = .args_r_block_figure())
           )
.heading <- 
  setClass("heading", 
           contains = "character",
           representation = 
             representation(level = "numeric"),
           prototype = prototype(level = 2)
           )
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
# ==========================================================================
# method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @importFrom crayon silver
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
setMethod("show", 
          signature = c(object = "code_block_figure"),
          function(object){
            textSh(crayon::silver("use for cross-referencing:",
                                  get_ref(object, "fig")), ending = NULL)
            selectMethod("show", "code_block")@.Data(object)
          })
#' @importFrom crayon green
setMethod("show", 
          signature = c(object = "heading"),
          function(object){
            textSh(crayon::green$bold(call_command(object)), exdent = 0)
          })
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
setMethod("code_block", 
          signature = c(x = "ANY"),
          function(x){ x@code_block })
setReplaceMethod("code_block", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, code_block = value)
                 })
## ------------------------------------- 
setMethod("codes", 
          signature = c(x = "code_block"),
          function(x){ x@codes })
setReplaceMethod("codes", 
                 signature = c(x = "code_block"),
                 function(x, value){
                   initialize(x, codes = value)
                 })
## ------------------------------------- 
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
setMethod("new_code_block", 
          signature = c(language = "ANY"),
          function(language, codes, args, prettey, fun_prettey){
            reCallMethod("new_code_block", .fresh_param(new_code_block()))
          })
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
setMethod("new_code_block_figure", 
          signature = c(name = "character"),
          function(name, caption, ...){
            args <- .fresh_param(new_code_block(), list(...))
            args$args$fig.cap <- caption
            args$language <- paste0("r ", name)
            as(do.call(new_code_block, args), "code_block_figure")
          })
setMethod("new_code_block_table", 
          signature = c(name = "character"),
          function(name, ...){
            args <- .fresh_param(new_code_block(), list(...))
            args$language <- paste0("r ", name)
            as(do.call(new_code_block, args), "code_block_table")
          })
## ------------------------------------- 
setMethod("call_command", 
          signature = c(x = "code_block"),
          function(x){
            do.call(command_function(x),
                    c(command_name = command_name(x),
                      command_args(x), codes = codes(x)))
          })
## ---------------------------------------------------------------------- 
## ---------------------------------------------------------------------- 
setMethod("heading", 
          signature = c(x = "ANY"),
          function(x){ x@heading })
setReplaceMethod("heading", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, heading = value)
                 })
setMethod("level", 
          signature = c(x = "heading"),
          function(x){ x@level })
setReplaceMethod("level", 
                 signature = c(x = "heading"),
                 function(x, value){
                   initialize(x, level = value)
                 })
setMethod("new_heading", 
          signature = c(heading = "character",
                        level = "numeric"),
          function(heading, level){
            .heading(heading, level = level)
          })
setMethod("call_command", 
          signature = c(x = "heading"),
          function(x){
            paste0(paste0(rep("#", level(x)), collapse = ""),
                   " ", x)
          })
## ---------------------------------------------------------------------- 
## ---------------------------------------------------------------------- 
setMethod("paragraph", 
          signature = c(x = "section"),
          function(x){ x@paragraph })
setReplaceMethod("paragraph", 
                 signature = c(x = "section"),
                 function(x, value){
                   initialize(x, paragraph = value)
                 })
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
setMethod("new_section", 
          signature = c(heading = "ANY"),
          function(heading, level, paragraph, code_block){
            args <- .fresh_param(new_section())
            reCallMethod("new_section", args)
          })
setMethod("new_section", 
          signature = c(heading = "character", level = "numeric",
                        paragraph = "character"),
          function(heading, level, paragraph, code_block){
            .section(heading = .heading(heading, level = level),
                     paragraph = paragraph, code_block = code_block)
          })
setMethod("new_section", 
          signature = c(heading = "NULL", level = "numeric",
                        paragraph = "character"),
          function(heading, level, paragraph, code_block){
            .section(heading = NULL, paragraph = paragraph, code_block = code_block)
          })
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
setMethod("call_command", 
          signature = c(x = "NULL"),
          function(x){
            return()
          })
