# ==========================================================================
# Help for rendering report
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @export get_ref
#' @description \code{get_ref}: get the string for cross-reference.
#' @param object [code_block_figure-class] or [code_block_table-class] object.
#' @param type character. "fig" or "tab".
#' @rdname code_block-class
#'
#' @examples
#' \dontrun{
#'   ## general
#'   codes <- "df <- data.frame(x = 1:10)
#'     df<-dplyr::mutate(df,y=x*1.5)%>%
#'     dplyr::filter(x >= 5)
#'     p <- ggplot(df)+
#'     geom_point(aes(x=x,y=y))
#'     p"
#'   block <- new_code_block("r", codes, list(eval = T, echo = T, message = T))
#'   ## see results
#'   block
#'   call_command(block)
#'   writeLines(call_command(block))
#'   
#'   ## figure
#'   fig_block <- new_code_block_figure(
#'     "plot1",
#'     "this is a caption",
#'     codes = codes
#'   )
#'   ## see results
#'   fig_block
#'   writeLines(call_command(fig_block))
#'   command_args(fig_block)
#'   cat(get_ref(fig_block), "\n")
#'   
#'   ## table
#'   codes <- "df <- data.frame(x = 1:10) %>% 
#'     dplyr::mutate(y = x, z = x * y)
#'     knitr::kable(df, format = 'markdown', caption = 'this is a caption') "
#'   tab_block <- new_code_block_table("table1", codes = codes)
#'   ## see results
#'   tab_block
#'   cat(get_ref(tab_block), "\n")
#'   
#'   ## default parameters
#'   new_code_block()
#'   
#' }
get_ref <- 
  function(object, type = c("fig", "tab")){
    type <- match.arg(type)
    name <- sub("^r ", "", command_name(object))
    paste0("\\@ref(", type, ":", name, ")")
  }

#' @export render_report
#' @aliases render_report
#' @title Convert 'report' as .Rmd file
#' @description \code{render_report}: Write down [report-class] onject
#' as .Rmd file, then [rmarkdown::render()] can be used to output any
#' available formation.
#' 
#' @param x [report-class] object.
#' @param file character(1). File name to save as.
#' @param set_all_eval logical(1). If \code{TRUE}, all [code_block-class] object
#' or [section-class] object related with "r" would be set with \code{eval = T}.
#' 
#' @rdname render_report
render_report <- function(x, file, set_all_eval = F) {
  if (set_all_eval) {
    layers(x) <- lapply(layers(x),
      function(obj) {
        if (is(obj, "code_block")) {
          command_args(obj)$eval <- T
        } else if (is(obj, "section")) {
          if (!is.null(code_block(obj))) {
            command_args(code_block(obj))$eval <- T
          }
        }
        return(obj)
      })
  }
  writeLines(call_command(x), file)
}

#' @export gather_sections
#' @aliases gather_sections
#' @title Quickly gather all the 'sections' in environment
#' @description \code{gather_sections}:
#' Gathers all eligible variable names in an environment by means of Regex
#' matches.  These variables must: have a uniform character prefix, and the first
#' character that follows must be a number. e.g., "s1", "s2", "s12.2",
#' "s15.5.figure"...
#' 
#' @param prefix character(1). The character prefix of the variable name.
#' @param envir environment. The environment to get the variables.
#' @param sort logical(1). If \code{TRUE}, sort the variable names
#' according to the first number string that accompanies the prefix.
#' @param get logical(1). If \code{TRUE}, return with a list of the value of
#' the variables. If \code{FALSE}, return with the variable names.
#' 
#' @rdname gather_sections
gather_sections <- function(prefix = "s", envir = parent.frame(),
  sort = T, get = T)
{
  objs <- ls(envir = envir)
  sections <- objs[ grepl(paste0("^", prefix, "[0-9]"), objs) ]
  if (sort) {
    num <- stringr::str_extract(
      sections, paste0("(?<=", prefix, ")[0-9]{1,}[.]{0,}[0-9]{0,}")
    )
    sections <- sections[order(as.numeric(num))]
  }
  if (get) {
    sections <- sapply(sections, get, envir = envir, simplify = F)
  }
  sections
}

