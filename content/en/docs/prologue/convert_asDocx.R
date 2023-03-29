# ==========================================================================
# convert quick-start
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

inclu.ht2docx <- function(x, img, title) {
  img <- get_filename(img)
  dir.create("thesis_fig", F)
  if (grepl("\\.svg$", img)) {
    new_img <- paste0("thesis_fig/", gsub("\\.svg", ".png", img))
    if (!file.exists(new_img))
      rsvg::rsvg_png(img, new_img, 3000)
  } else {
    new_img <- img
  }
  knitr::opts_current$set(fig.cap = title)
  inclu.fig(new_img)
}

lines <- readLines("./quick_start.Rmd")
lines <- gsub(", results = \"asis\"", "", lines)
report <- as_report.rough(gsub("^#", "##", lines))
write_thesisDocx(report, "test.Rmd", "Quick start",
  change_include_fun = "inclu.ht2docx",
  origin_include_fun = "inclu.fig.ht"
)

custom_render("test.Rmd")
file.remove("test.Rmd", "./_temp_test.Rmd")

file.copy("./_temp_test.docx",
  "/mnt/data/wizard/Documents/article/MCnebula2/others/quick_start.docx", T)
