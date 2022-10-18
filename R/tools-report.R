# ==========================================================================
# functions used in 'report' or 'section' class 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.write_block <- 
  function(command_name, ..., codes){
    args <- list(...)
    if (length(args) > 0) {
      lapply(names(args),
             function(name) {
               if (nchar(name) == 0)
                 stop("the args for r block must contain parameter names, ",
                      "e.g., 'eval = FALSE', 'echo = TRUE'")
             })
      args <- paste0(paste0(names(args), " = ", args),
                     collapse = ", ")
      leader <- paste0("```{", command_name, ", ", args, "}")
    } else {
      leader <- paste0("```{", command_name, "}")
    }
    end <- "```"
    if (length(codes) > 1)
      codes <- paste0(codes, collapse = "\n")
    paste(leader, codes, end, sep = "\n")
  }
.args_r_block <-
  function(){
    list(echo = T,
         eval = T,
         message = F
    )
  }
.args_r_block_table <-
  function(){
    list(echo = T,
         eval = T,
         message = F
    )
  }
.args_r_block_figure <-
  function(){
    list(echo = T,
         eval = T,
         message = F,
         fig.cap = "The figure"
    )
  }
nshow <- function(object){
  if (!is.null(object)) {
    show(object)
  }
}
textSh <- 
  function(..., sep = "", exdent = 4, ending = "\n",
           pre_collapse = F, collapse = "\n",
           pre_trunc = F, trunc_width = 200,
           pre_wrap = F, wrap_width = 60){
    text <- list(...)
    if (pre_collapse) {
      text <- vapply(text, paste, "ch", collapse = collapse)
    }
    text <- paste(text, sep = sep)
    if (pre_trunc) {
      text <- .text_fold(text, trunc_width)
    }
    if (pre_wrap) {
      text <- paste0(strwrap(text, width = wrap_width), collapse = "\n")
    }
    exdent <- paste0(rep(" ", exdent), collapse = "")
    writeLines(gsub("(?<=\n)|(?<=^)", exdent, text, perl = T))
    if (!is.null(ending))
      cat(ending)
  }
get_ref <- 
  function(object, type = c("fig", "tab")){
    type <- match.arg(type)
    name <- sub("^r ", "", command_name(object))
    paste0("\\@ref(", type, ":", name, ")")
  }
#' @importFrom stringr str_trunc
.text_fold <- 
  function(text, width = 200, ellipsis = crayon::silver("...(fold)")){
    stringr::str_trunc(text, width = width, ellipsis = ellipsis)
  }
