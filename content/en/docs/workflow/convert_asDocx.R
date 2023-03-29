# ==========================================================================
# convert Rmd to docx
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

lines <- readLines("./basic_workflow.Rmd")
report <- as_report.rough(gsub("^#", "##", lines))
write_thesisDocx(report, "test.Rmd", "Basic workflow")

custom_render("test.Rmd")
file.remove("test.Rmd", "./_temp_test.Rmd")

file.copy("./_temp_test.docx",
  "/mnt/data/wizard/Documents/article/MCnebula2/others/basic_workflow.docx", T)
