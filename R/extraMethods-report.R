# ==========================================================================
# some methods for class 'report', to fast generate layer of 'section' or
# 'code_block' etc.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases include_figure
#'
#' @title Easily embed figure into document
#'
#' @description
#' Creates a pre-defined [code_block_figure-class] object containing the codes of
#' [knitr::include_graphics()] for formatting display the figure in document.
#'
#' @seealso [code_block_figure-class], [report-class], [knitr::include_graphics()]...
#' 
#' @name include_figure-methods
#'
#' @order 1
NULL
#> NULL

#' @exportMethod include_figure
#'
#' @aliases include_figure
#'
#' @param file character(1). The path of file. See [knitr::include_graphics()] for
#' the supported image formats.
#' @param name character(1). For cross-reference in document.
#' See \url{https://bookdown.org/yihui/rmarkdown-cookbook/cross-ref.html#cross-ref}.
#' @param caption character(1). Caption of figure display in document.
#'
#' @rdname include_figure-methods
#'
#' @examples
#' \dontrun{
#'   tmp <- paste0(tempdir(), "/test.pdf")
#'   pdf(tmp)
#'   plot(1:10)
#'   dev.off()
#'   
#'   fig_block <- include_figure(
#'     tmp, "plot", "This is caption"
#'   )
#'   fig_block
#' }
setMethod("include_figure", 
          signature = c(file = "character",
                        name = "character",
                        caption = "character"),
          function(file, name, caption){
            .check_file(file)
            codes <- paste0("knitr::include_graphics('", file, "')")
            args <- list(echo = F, eval = T, message = F)
            new_code_block_figure(name = name,
                                  caption = caption,
                                  codes = codes,
                                  args = args)
          })

#' @aliases include_table
#'
#' @title Easily embed table into document
#'
#' @description
#' Creates a pre-defined [code_block_table-class] object containing the codes of
#' [knitr::kable()] for formatting display the table in document.
#'
#' @name include_table-methods
#'
#' @order 1
NULL
#> NULL

#' @exportMethod include_table
#'
#' @aliases include_table
#'
#' @param data 'data.frame' object. The data of table to display in document.
#' @param name character(1). For cross-reference in document.
#' See \url{https://bookdown.org/yihui/rmarkdown-cookbook/cross-ref.html#cross-ref}.
#'
#' @param caption character(1). Caption of figure display in document.
#'
#' @rdname include_table-methods
#'
#' @examples
#' \dontrun{
#'   data <- data.frame(x = 1:10, y = 1:10)
#'   tab_block <- include_table(
#'     data, "table1",
#'     "This is caption"
#'   )
#'   tab_block
#' }
setMethod("include_table", 
          signature = c(data = "data.frame",
                        name = "character",
                        caption = "character"),
          function(data, name, caption){
            var <- deparse(substitute(data))
            codes <- paste0("knitr::kable(", var, ", ",
                            "format = 'markdown', ",
                            "caption = '", caption, "')")
            args <- list(echo = F, eval = T, message = F)
            new_code_block_table(name = name,
                                 codes = codes,
                                 args = args)
          })

#' @aliases history_rblock
#'
#' @title Create 'code_block' object from history codes
#'
#' @description
#' Get codes from R history, then formatted as [code_block-class] object.
#' 
#' @seealso
#' [code_block-class], [history()]...
#'
#' @name history_rblock-methods
#'
#' @order 1
NULL
#> NULL

#' @exportMethod history_rblock
#' @description \code{history_rblock()}: get the default parameters for the method
#' \code{history_rblock}.
#' @rdname history_rblock-methods
setMethod("history_rblock", 
          signature = setMissing("history_rblock"),
          function(){
            list(exclude = 1)
          })

#' @exportMethod history_rblock
#' @description \code{history_rblock(x, ...)}: use the default parameters whatever 'missing'
#' while performing the method \code{history_rblock}.
#' @rdname history_rblock-methods
setMethod("history_rblock", 
          signature = c(nrow  = "numeric"),
          function(nrow, pattern_start, pattern_end, exclude){
            reCallMethod("history_rblock",
                         .fresh_param(history_rblock()))
          })

#' @exportMethod history_rblock
#'
#' @aliases history_rblock
#'
#' @param nrow numeric(1). The number of lines of code to fetch.
#' @param pattern_start character(1).
#' The pattern string used to match the starting line of codes in R history.
#' 
#' @param pattern_end character(1).
#' The pattern string used to match the ending line of codes in R history.
#' 
#' @param exclude numeric(1). Used to exclude the last lines of code.
#'
#' @rdname history_rblock-methods
#'
#' @examples
#' \dontrun{
#'   test1 <- 1
#'   test2 <- 2
#'   test3 <- 3
#'   
#'   block <- history_rblock(, "^test1", "^test3")
#'   block
#' }
setMethod("history_rblock", 
          signature = setMissing("history_rblock",
                                 nrow = "numeric",
                                 exclude = "numeric"),
          function(nrow, exclude){
            his <- tail(get_history(exclude), n = nrow)
            args <- list(echo = T, eval = F)
            new_code_block(codes = his, args = args)
          })

#' @exportMethod history_rblock
#' @rdname history_rblock-methods
setMethod("history_rblock", 
          signature = setMissing("history_rblock",
                                 pattern_start = "character",
                                 pattern_end = "character",
                                 exclude = "ANY"),
          function(pattern_start, pattern_end, exclude){
            if (missing(exclude))
              exclude <- 1
            his <- get_history(exclude)
            end <- tail(grep(pattern_end, his, perl = T), n = 1)
            start <- tail(grep(pattern_start, his[1:end]), n = 1)
            args <- list(echo = T, eval = F)
            new_code_block(codes = his[start:end], args = args)
          })

#' @export rblock
#' @aliases rblock
#' @title Eval the code as well create 'code_block' object
#' 
#' @description \code{rblock}: Run or not run the code with formatting as [code_block-class]
#' object.
#' @param code The code to run or document. Braces (‘{}’) must be used.
#' @param eval logical. Whether to eval the code.
#' @param envir environment. The ‘environment’ in which the code is to be evaluated.
#' @param ... Other parameters passed to [new_code_block()].
#' @rdname rblock
#'
#' @examples
#' \dontrun{
#'   rblock({
#'     test1 <- 1
#'     test2 <- 2
#'     test3 <- 3
#'   })
#'   
#'   rblock({
#'     test <- mcn_5features
#'     ## this annotation line would be ignored
#'     test1 <- filter_structure(test)
#'     test1 <- create_reference(test1)
#'     test1 <- filter_formula(test1, by_reference=T)
#'     test1 <- create_stardust_classes(test1)
#'   })
#' }
rblock <- function(code, eval = T, envir = parent.frame(), ...){
  code <- substitute(code)
  if (!rlang::is_call(code, "{")) {
    info <- paste0(c("The `code` argument must be a braced expression,",
                     "use this such as:",
                     styler::style_text("rblock({\ntest <- 1\ntest <- 2\n})")),
                   collapse = "\n")
    stop(info)
  }
  if (eval) {
    eval(code, envir = envir)
  }
  code <- unlist(lapply(2:length(code), function(n) deparse(code[[n]])))
  args <- list(...)
  block.args <- list(echo = T, eval = F)
  if (!is.null(args$args)) {
    block.args <- .fresh_param(block.args, args$args)
    args$args <- NULL
  }
  do.call(new_code_block, c(list(codes = code, args = block.args), args))
}
