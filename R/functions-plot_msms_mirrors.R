# ==========================================================================
# draw mirror bar plot of MS/MS spectra
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @importFrom dplyr starts_with
#' @importFrom grid grid.draw
#' @importFrom grid upViewport
#' @importFrom grid downViewport
#' @aliases plot_msms_mirrors
#'
#' @title Draw MS/MS mirror bar plots
#'
#' @description
#' Draw MS/MS spectra as mirror bar plots using 'ggplot2' with [facet_wrap()].
#' The sub-panel of each 'features' would be found by [grid::grid.grep()]...
#' Then chemical structures would be drawn into sub-panel.
#'
#' @param x [mcnebula-class] object.
#' @param .features_id character. The ID of 'features'.
#' @param fun_modify function. Used to post modify the [ggset-class] object before
#' visualization. See [fun_modify].
#' @param structure_vp 'viewport' object. Created by [grid::viewport()]. The 'viewport'
#' to draw chemical structures in sub-panel.
#'
#' @seealso [facet_wrap()], [grid::grid.force], [grid::grid.grep()]...
#' 
#' @name plot_msms_mirrors
NULL
#> NULL

#' @export plot_msms_mirrors
#' @aliases plot_msms_mirrors
#' @rdname plot_msms_mirrors
plot_msms_mirrors <- 
  function(x, .features_id, fun_modify = modify_set_labs_xy,
           structure_vp = grid::viewport(0.7, 0.3, 0.3, 0.3)
           ){
    .check_data(x, list(features_annotation = "create_features_annotation"))
    x <- collate_data(x, ".f2_msms")
    x <- collate_data(x, ".f3_spectra")

    ## raw peak
    raw_msms <- latest(x, "project_dataset", ".f2_msms")
    raw_msms <- dplyr::filter(raw_msms, .features_id %in% !!.features_id)
    raw_msms <- dplyr::mutate(raw_msms, rel.int. = int. / max(int.) * 100)
    raw_msms <- dplyr::select(raw_msms, .features_id,
                              raw_mz = mz, raw_rel.int. = rel.int.)
    raw_msms <- split(raw_msms, ~ .features_id)

    ## non-noise peak
    sig_msms <- latest(x, "project_dataset", ".f3_spectra")
    sig_msms <- dplyr::filter(sig_msms, .features_id %in% !!.features_id)
    sig_msms <- dplyr::select(sig_msms, .features_id,
                              sig_mz = mz, sig_rel.int. = rel.int.)
    sig_msms <- split(sig_msms, ~ .features_id)

    ## merge
    set <- sapply(.features_id, simplify = F,
                  function(id){
                    df <- tol_merge(raw_msms[[id]], sig_msms[[id]],
                                    "raw_mz", "sig_mz")
                    df <- dplyr::select(df, -dplyr::starts_with(".features_id"))
                    dplyr::mutate(df, sig_rel.int. = -sig_rel.int.)
                  })

    set <- data.table::rbindlist(set, idcol = T)
    set <- dplyr::rename(set, .features_id = .id)
    matched_set <- dplyr::filter(set, !is.na(sig_mz))

    ## precursor mz and rt
    anno <- dplyr::select(features_annotation(x),
                          tani.score, .features_id, pre.mz = mz, rt.secound)
    anno <- dplyr::filter(anno, .features_id %in% !!.features_id)
    export_name <- export_name(x)

    anno <-
      dplyr::mutate(anno,
                    tani.score = round(tani.score, 2),
                    pre.mz = round(pre.mz, 4),
                    rt.min = round(rt.secound / 60, 2),
                    x = 0, y = 65,
                    label = paste0(!!export_name[[ "pre.mz" ]],
                                   ": ", pre.mz, "\n",
                                   !!export_name[[ "rt.min" ]],
                                   ": ", rt.min, "\n",
                                   "TS: ", ifelse(is.na(tani.score), "-",
                                                  tani.score)
                                   ))

    ggset <-
      new_ggset(new_command(ggplot),
                .command_msms_rawPeak(set),
                .command_msms_sigPeak(matched_set),
                .command_msms_rawDot(matched_set),
                .command_msms_sigDot(matched_set),
                .command_msms_text(anno),
                .command_msms_y(),
                new_command(theme_minimal),
                .command_msms_theme(),
                .command_msms_facet()
      )
    p <- call_command(fun_modify(ggset))
    
    print(p, newpage = T)
    df.vp <- get_facet.wrap.vp(.features_id)

    .message_info_viewport()
    apply(df.vp, 1,
          function(lst){
            grob <- structures_grob(child_nebulae(x))[[ lst[[ "strip" ]] ]]
            if (!is.null(grob)) {
              grid::downViewport(paste0(lst[[ "vp" ]]))
              grid::pushViewport(structure_vp)
              grid.draw(grob)
              grid::upViewport(0)
            }
          })
    .message_info_viewport()

  }

.command_msms_rawPeak <- 
  function(df, color = "black", size = 0.8, alpha = 0.8){
    df <- dplyr::mutate(df, mz = raw_mz, rel.int. = 0)
    new_command(geom_segment, data = df,
                aes(x = mz, xend = raw_mz, y = rel.int., yend = raw_rel.int.),
                color = color, size = size, alpha = alpha
    )
  }

.command_msms_sigPeak <- 
  function(df, color = "#E6550DFF", size = 0.8, alpha = 1){
    new_command(geom_segment, data = df,
                aes(x = sig_mz, xend = sig_mz, y = 0, yend = sig_rel.int.),
                color = color, size = size, alpha = alpha
    )
  }

.command_msms_sigDot <- 
  function(df, color = "#E6550DFF", size = 0.8, alpha = 1){
    new_command(geom_point, data = df,
                aes(x = sig_mz, y = sig_rel.int.),
                color = color, size = size, alpha = alpha
    )
  }

.command_msms_rawDot <- 
  function(df, color = "black", size = 0.8, alpha = 0.8){
    new_command(geom_point, data = df,
                aes(x = raw_mz, y = raw_rel.int.),
                color = color, size = size, alpha = alpha
    )
  }

.command_msms_text <- 
  function(df, hjust = 0, fontface = "bold", family = .font){
    new_command(geom_text, data = df,
                aes(x = x, y = y, label = label),
                hjust = hjust, fontface = fontface, family = family
    )
  }

.command_msms_y <- 
  function(){
    new_command(scale_y_continuous, limits = c(-100, 100))
  }

.command_msms_theme <- 
  function(){
    new_command(theme, 
                text = element_text(family = .font),
                strip.text = element_text(size = 12),
                panel.grid = element_line(color = "grey85"),
                plot.background = element_rect(
                  fill = "white", color = "transparent", size = 0
                )
    )
  }

.command_msms_facet <- 
  function(){
    new_command(facet_wrap, ~ paste("ID:", .features_id), scales = "free")
  }

#' @importFrom dplyr bind_rows
tol_merge <- 
  function(main,
           sub,
           main_col = "mz",
           sub_col = "mz",
           tol = 0.002,
           bin_size = 1
           ){
    if (main_col == sub_col) {
      new_name <- paste0(sub_col, ".sub")
      colnames(sub)[colnames(sub) == sub_col] <- new_name
      sub_col <- new_name
    }
    main$...seq <- 1:nrow(main)
    backup <- main
    ## to reduce computation, round numeric for limitation
    ## main
    main$...id <- round(main[[ main_col ]], bin_size)
    ## sub
    sub.x <- sub.y <- sub
    sub.x$...id <- round(sub.x[[ sub_col ]], bin_size)
    sub.y$...id <- sub.x$...id + ( 1 * 10^-bin_size )
    sub <- rbind(sub.x, sub.y)
    ## expand merge
    df <- merge(main, sub, by = "...id", all.x = T, allow.cartesian = T)
    df$...diff <- abs(df[[ main_col ]] - df[[ sub_col ]])
    df <- dplyr::filter(df, ...diff <= !!tol)
    ## get the non-merged
    backup <- backup[!backup$...seq %in% df$...seq, ]
    df <- dplyr::bind_rows(df, backup)
    ## remove the assist col
    dplyr::select(df, -...id, -...diff, -...seq)
  }

#' @importFrom grid grid.grep
#' @importFrom grid grid.force
get_facet.wrap.vp <-
  function(
           strip,
           grid.force = T
           ){
    if(grid.force){
      grid::grid.force()
    }
    ## grep vp of panel
    panel <- grid::grid.grep("panel", grep = T, global= T, viewports = T, grobs = F)
    ## vp name
    panel <- sapply(panel, paste)
    ## the specific seq number of vp
    panel.seq <- stringr::str_extract(panel, "(?<=panel-)[0-9]{1,}(?=-)")
    panel.seq <- max(as.integer(panel.seq))
    ## number stat
    len <- length(strip)
    len.p <- length(panel)
    ## the number of blank panel
    na <- len.p - (len %% len.p)
    if(na == len.p)
      na <- 0
    ## as matrix
    mat <- matrix(c(sort(strip), rep(NA, na)), ncol = panel.seq, byrow = T)
    vec <- as.vector(mat)
    ## as data.frame
    df <- data.table::data.table(vp = panel, strip = vec)
    ## filter out the NA
    df <- dplyr::filter(df, !is.na(strip))
    return(df)
  }
