
library(MCnebula2)
devtools::load_all("~/utils.tool")

source("~/outline/mc.test/mcn_structure/a_project.R")

spArr <- polygonGrob(
  c(0, .6, 1, .6, 0), c(0, 0, .5, 1, 1),
  gp = gpar(lwd = u(2, line), col = "grey30")
)
spArrScale <- ggather(spArr, vp = viewport(, , .2, .3))

objs <- c(namel(grob.sampleCir, grob.lcms, grob.convert, grob.rdetect,
    grob.sirius, spArrScale, omit),
  dataset = list(grobs.project[[1]]),
  sapply(c("samples", "LC-MS/MS", "convert_raw_data",
      "feature_detection", "run_SIRIUS", "MCnebula2"),
    simplify = F, function(name) { gtext(name) })
)

ws <-  c(grob.sampleCir = .5, samples = .1, spArrScale = .1,
  grob.lcms = 1, `LC-MS/MS` = .1, spArrScale = .1,
  grob.convert = .6, convert_raw_data = .1, spArrScale = .1,
  grob.rdetect = .7, feature_detection = .1, spArrScale = .1,
  grob.sirius = .4, run_SIRIUS = .1, spArrScale = .1,
  MCnebula2 = .2, omit = .1, dataset = 1)

mframe_row <- function(ws, objs) {
  ws <- split(ws, rep(1:6, each = 3))[1:5]
  grobs <- lapply(ws,
    function(w) {
      frame_row(w[1], objs)
    })
}

jobs <- mframe_row(ws, objs)
names(jobs) <- n(g, 5)

frame_colEx <- function(ws2, jobs) {
  w.text <- ws2
  ws <- split(ws, rep(1:6, each = 3))
  names(w.text) <- unlist(lapply(ws, function(w) c(names(w)[2], "null"))) %>% .[-length(.)]
  print(w.text)
  names(w.text) %<>% vapply(.,
    function(ch) if (ch == "omit") "MCnebula2" else ch, character(1))
  jobs <- c(list(null = nullGrob()), jobs)
  project <- frame_col(ws2, jobs)
  text <- frame_col(w.text, jobs)
  frame_row(c(project = 1, text = .3), namel(project, text))
}
.project <- frame_colEx(c(g1 = .5, spArrScale = .2,
    g2 = .6, spArrScale = .2,
    g3 = .6, spArrScale = .2,
    g4 = .8, spArrScale = .2,
    g5 = .8, spArrScale = .2,
    dataset = .8),
  c(jobs, objs))

svg("./procedure_quick_start.svg", 13, 2)
draw(.project)
dev.off()
