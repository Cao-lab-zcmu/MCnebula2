# ==========================================================================
# some methods for class 'report', to fast generate layer of 'section' or
# 'code_block' etc.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases include_figure
#'
#' @title ...
#'
#' @description ...
#'
#' @details ...
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
#' @title ...
#'
#' @description ...
#'
#' @details ...
#'
#' @param file ...
#' @param name ...
#' @param caption ...
#'
#' @return ...
#'
#' @seealso \code{\link{report-class}}, \code{\link{code_block-class}}...
#'
#' @rdname include_figure-methods
#'
#' @examples
#' \dontrun{
#' include_figure(...)
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
#' @title ...
#'
#' @description ...
#'
#' @details ...
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
#' @title ...
#'
#' @description ...
#'
#' @details ...
#'
#' @param data ...
#' @param name ...
#' @param caption ...
#'
#' @return ...
#'
# @seealso ...
#'
#' @rdname include_table-methods
#'
#' @examples
#' \dontrun{
#' include_table(...)
#' }
setMethod("include_table", 
          signature = c(data = "data.frame",
                        name = "character",
                        caption = "character"),
          function(data, name, caption){
            var <- rlang::as_label(substitute(data))
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
#' @title ...
#'
#' @description ...
#'
#' @details ...
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
#' @title ...
#'
#' @description ...
#'
#' @details ...
#'
#' @param nrow ...
#' @param pattern_start ...
#' @param pattern_end ...
#' @param exclude ...
#'
# @inheritParams rdname
#'
#' @return ...
#'
#' @seealso \code{\link{report-class}}, \code{\link{code_block-class}}...
#'
#' @rdname history_rblock-methods
#'
#' @examples
#' \dontrun{
#' history_rblock(...)
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

