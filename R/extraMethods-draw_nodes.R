# ==========================================================================
# draw all nodes (with annotation) for a specified child-nebula
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("draw_nodes", 
          signature = setMissing("draw_nodes",
                                 x = "missing"),
          function(){
            list(nodes_color = "#D9D9D9",
                 add_id_text = T,
                 add_structure = T,
                 add_ppcp = T,
                 add_statistic = T
            )
          })
setMethod("draw_nodes", 
          signature = c(x = "mcnebula", nebula_name = "character"),
          function(x, nebula_name){
            args <- as.list(environment())
            args <- args[ !vapply(args, is.name, T) ]
            new_args <- draw_nodes()
            for (i in names(args)) {
              new_args[[i]] <- args[[i]]
            }
            do.call(draw_nodes, new_args)
          })
#' @importFrom svglite svglite
#' @importFrom grid pushViewport
#' @importFrom grid viewport
#' @importFrom grid popViewport
#' @importFrom tibble as_tibble
setMethod("draw_nodes", 
          signature = c(x = "mcnebula", nebula_name = "character",
                        nodes_color = "character",
                        add_id_text = "logical",
                        add_structure = "logical",
                        add_ppcp = "logical",
                        add_statistic = "logical"),
          function(x, nebula_name, nodes_color, add_id_text, 
                   add_structure, add_ppcp, add_statistic){
            if (add_ppcp) {
              if (length(ppcp_data(child_nebulae(x))) == 0)
                x <- set_ppcp_data(x)
            }
            if (add_statistic) {
              if (length(statistic_data(child_nebulae(x))) == 0)
                x <- set_statistic_data(x)
            }
            if (add_structure) {
              if (length(ppcp_data(child_nebulae(x))) == 0)
                x <- draw_structures(x, nebula_name)
            }
            .features_id <-
              `[[`(tibble::as_tibble(tbl_graph(child_nebulae(x))[[nebula_name]]),
                   "name")
            if (is.null(.features_id)) {
              return(message("No features found for `nebula_name`"))
            }
            ggsets <- ggset_vis_nodes(x, .features_id, nodes_color,
                                     add_ppcp, add_statistic)
            path <- paste0(export_path(x), "/tmp/nodes")
            .check_path(path)
            lapply(names(ggsets),
                   function(id){
                     svglite::svglite(paste0(path, "/", id, ".svg"),
                                      bg = "transparent")
                     print(call_command(modify_rm_legend(ggsets[[ id ]])))
                     if (add_structure) {
                       grid::pushViewport(grid::viewport(width = 0.7, height = 0.7))
                       show_structure(x, id)
                       grid::popViewport()
                     }
                     dev.off()
                   })
          })
ggset_vis_nodes <- 
  function(x, .features_id, nodes_color = "#D9D9D9",
           add_ppcp = T, add_statistic = T){
    if (add_ppcp) {
      .check_data(child_nebulae(x), list(ppcp_data = "set_ppcp_data"))
    }
    nodes_color <- .as_dic(nodes_color, .features_id, "#D9D9D9")
    set <- .prepare_data_for_nodes(x, .features_id, add_ppcp)
    ggsets <-
      sapply(.features_id, simplify = F,
             function(id) {
               names <- paste0(set[[id]]$rel.index)
               pal <- .as_dic(palette_col(x), names,
                              fill = F, as.list = F, na.rm = T)
               labels <- .as_dic(paste0("Bar: ", names, ": ", set[[id]]$class.name),
                                 names, fill = F, as.list = F)
               new_ggset(new_command(ggplot, set[[id]]),
                         .default_node_nuclear(nodes_color[[id]]),
                         .default_node_border(),
                         .default_nodes_radial_bar(),
                         .default_node_ylim(),
                         .default_node_polar(),
                         .default_node_fill(pal, labels),
                         new_command(theme_void),
                         .default_node_theme()
               )
             })
    if (add_statistic) {
      .check_data(child_nebulae(x), list(statistic_data = "statistic_data"))
      axis.len <- vapply(set, function(df) tail(df$seq, n = 1), 1) + 1
      set <- .prepare_data_for_statistic(x, .features_id, axis.len)
      group <- unique(sample_metadata(x)$group)
      pal.ex <- .as_dic(palette_stat(x), group,
                        fill = F, as.list = F, na.rm = T)
      labels.ex <- .as_dic(paste0("Ring: group: ", group), group,
                           fill = F, as.list = F)
      ggsets <-
        sapply(.features_id, simplify = F,
               function(id) {
                 if (is.null(set[[id]]))
                   return(ggsets[[id]])
                 ggset <- add_layers(ggsets[[id]],
                                     .default_node_statistic(set[[id]]),
                                     new_command(labs, fill = "Classes / Groups"))
                 scale <- command_args(layers(ggset)$scale_fill_manual)
                 command_args(layers(ggset)$scale_fill_manual)$values <- 
                   c(pal.ex, c(" " = "white"), scale$values)
                 command_args(layers(ggset)$scale_fill_manual)$labels <- 
                   c(scale$labels, labels.ex)
                 ggset
               })
    }
    ggsets
  }
#' @importFrom dplyr mutate
.prepare_data_for_nodes <- 
  function(x, .features_id, add_ppcp = T){
    df <- data.frame(rel.index = -1L, pp.value = 0L, seq = 1:3)
    if (add_ppcp) {
      set <- ppcp_data(child_nebulae(x))
      set <- sapply(.features_id, simplify = F,
                    function(id) {
                      if (is.null(set[[id]]))
                        df
                      else
                        set[[id]]
                    })
    } else {
      set <- sapply(.features_id, function(id) df, simplify = F)
    }
    set
  }
.prepare_data_for_statistic <- 
  function(x, .features_id, axis.len){
    set <- statistic_data(child_nebulae(x))
    sapply(.features_id, simplify = F,
           function(id) {
             if (is.null(set[[id]]))
               return()
             df <- set[[id]]
             max <- cumsum(df$value)
             min <- c(0, max[-length(max)])
             factor <- axis.len[[id]] / max(max)
             df$x <- (min + df$value / 2) * factor
             df$width <- df$value * factor
             df
           })
  }
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
setMethod("set_ppcp_data", 
          signature = setMissing("set_ppcp_data",
                                 x = "mcnebula"),
          function(x){
            set_ppcp_data(x, classes = names(tbl_graph(child_nebulae(x))))
          })
setMethod("set_ppcp_data", 
          signature = c(x = "mcnebula", classes = "character"),
          function(x, classes){
            ppcp_data <-
              suppressMessages(latest(filter_ppcp(x, dplyr::filter,
                                                  class.name %in% classes)))
            ppcp_data <- dplyr::select(ppcp_data, rel.index, class.name,
                                       pp.value, .features_id)
            ppcp_data(child_nebulae(x)) <-
              lapply(split(ppcp_data, ~ .features_id),
                     function(df) {
                       dplyr::mutate(df, seq = 1:nrow(df))
                     })
            return(x)
          })
#' @importFrom tidyr gather
#' @importFrom tibble as_tibble
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
setMethod("set_statistic_data", 
          signature = setMissing("set_statistic_data",
                                 x = "mcnebula"),
          function(x){
            set_statistic_data(x, mean = T)
          })
setMethod("set_statistic_data", 
          signature = c(x = "mcnebula", mean = "logical"),
          function(x, mean){
            .check_data(x, list(features_quantification = "features_quantification",
                                sample_metadata = "sample_metadata"), "(x) <-")
            statistic_data <-
              tidyr::gather(features_quantification(x),
                            key = "sample", value = "value", -.features_id)
            statistic_data <-
              tibble::as_tibble(merge(statistic_data, sample_metadata(x),
                                      by = "sample", all.x = T))
            if (mean) {
              statistic_data <-
                dplyr::summarise(dplyr::group_by(statistic_data, .features_id, group),
                                 value = mean(value, na.rm = T))
              statistic_data <- dplyr::ungroup(statistic_data)
            }
            statistic_data(child_nebulae(x)) <- 
              split(statistic_data, ~.features_id)
            return(x)
          })
