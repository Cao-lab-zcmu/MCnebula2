## default font for visualization
# @importFrom grDevices pdfFonts
# .setFont <- function(pattern){
  # font <- names(pdfFonts())
  # n <- grep(pattern, font)
  # if (length(n) >= 1) {
  #   font <- font[n[1]]
  # } else {
  #   font[1]
  # }
# }

# .font <- if (.Platform$OS.type == 'unix') "Times" else "Times New Roman"

#' @export setFont
#' @title Set font for visualization of MCnebula2
#' @description \bold{Note that} your R harbours the font you set.
#' @param font character(1). Such as 'Times'. If you output the
#' visualization for pdf, use \code{grDevices::pdfFonts()} to checkout
#' the available fonts; else, you might need help with package \code{extrafont}.
#' @rdname setFont
setFont <- function(font = "Times") {
  assign(".font", font, env = topenv(environment()))
  options(mcnebulaFont = font)
}
.font <- "Times"
setFont("Times")

