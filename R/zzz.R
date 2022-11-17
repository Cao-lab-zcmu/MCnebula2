## default font for visualization
#' @importFrom grDevices pdfFonts
.setFont <- function(pattern){
  font <- names(pdfFonts())
  n <- grep(pattern, font)
  if (length(n) >= 1) {
    font <- font[n[1]]
  } else {
    font[1]
  }
}
.font <- .setFont("^Times")
