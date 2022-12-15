# ==========================================================================
# draw all nodes (with annotation) for a specified child-nebula
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases draw_nodes
#'
#' @title Draw and visualize chemcial structures for Child-Nebulae
#'
#' @description
#' Methods used for drawing and visualizing nodes of 'features'
#' in Child-Nebulae (networks). The methods used to visualize 'features'
#' with annotations of:
#' - chemical structures
#' - chemical classification
#' - quantification data (peak area)
#' - ID of 'feature' (.features_id)
#' 
#' @details
#' Those annotated visualizations are drawn in steps and then are put together.
#' In order to render the text as a graphical path (otherwise, the graphics
#' would not be compatible with too small fonts and would result in misplaced text),
#' the 'ggplot' object or 'grob' object is first exported as an SVG file,
#' which is subsequently read by [grImport2::readPicture()], followed by
#' [grImport2::grobify()] as 'grob' object, and then combined into
#' the final 'grob'. In general, this process is time consuming,
#' especially when there are a lot of 'features' for visualization.
#'
#' @seealso [grid::grid.draw()], [grid::grob()], [grImport2::readPicture()],
#' [grImport2::grobify()]...
#'
#' @name draw_nodes-methods
#'
#' @order 1
NULL
#> NULL

#' @exportMethod draw_nodes
#' @description \code{draw_nodes()}: get the function for generating
#' default parameters for the method
#' \code{draw_nodes}.
#' @rdname draw_nodes-methods
setMethod("draw_nodes", 
          signature = setMissing("draw_nodes",
                                 x = "missing"),
          function(){
            function(x) {
              if (!is.null(nebula_index(x)[[ "tracer_color" ]])) {
                nodes_color <- nebula_index(x)[[ "tracer_color" ]]
              } else {
                nodes_color <- "#FFF9F2"
              }
              list(nodes_color = nodes_color,
                   add_id_text = T,
                   add_structure = T,
                   add_ppcp = T,
                   add_ration = T
              )
            }
          })

#' @exportMethod draw_nodes
#' @description \code{draw_nodes(x, ...)}: use the default parameters whatever 'missing'
#' while performing the method \code{draw_nodes}.
#' @rdname draw_nodes-methods
setMethod("draw_nodes", 
          signature = c(x = "mcnebula", nebula_name = "character"),
          function(x, nebula_name, nodes_color, add_id_text, 
                   add_structure, add_ppcp, add_ration){
            reCallMethod("draw_nodes",
                         .fresh_param(draw_nodes()(x)))
          })

#' @importFrom grDevices colorRampPalette dev.off
#' @importFrom svglite svglite
#' @importFrom grid pushViewport
#' @importFrom grid viewport
#' @importFrom grid popViewport
#' @importFrom pbapply pblapply
#' @importFrom tibble as_tibble
#' @importFrom rsvg rsvg_svg
#' @exportMethod draw_nodes
#'
#' @aliases draw_nodes
#'
#' @param x [mcnebula-class] object.
#' @param nebula_name character(1). Chemical classes in 'nebula_index' data.
#' Specified to draw nodes (of network) of all the 'features' of that.
#'
#' @param nodes_color character with names or not. The Value is Hex color.
#' Specified colors for 'features' to draw nodes. If the number of the colors
#' were not enough, the rest 'features' would be fill with default color.
#' If [set_tracer()] has been run, the colors specified in 'nebula_index'
#' would be used preferentially.
#'
#' @param add_id_text logical. If \code{TRUE}, add ID (.features_id) for
#' 'features' inside the nodes.
#' 
#' @param add_structure logical. If \code{TRUE}, draw chemical structures inside
#' the nodes. See [draw_structures()].
#' 
#' @param add_ppcp logical. If \code{TRUE}, draw radical bar plot inside the nodes
#' for annotation of PPCP data. See [set_ppcp_data()] for custom modify the annotated
#' PPCP data. Hex colors in \code{palette_col(object)} would be used for fill the bar
#' plot (Used by [ggplot2::scale_fill_manual()]).
#'
#' @param add_ration logical. If \code{TRUE}, draw ring plot inside the nodes
#' for annotation of features quantification data. See [set_ration_data()] for custom
#' modify the annotated quantification data. Hex colors in \code{palette_stat(object)}
#' would be used for fill be ring plot.
#'
#' @rdname draw_nodes-methods
#'
#' @examples
#' \dontrun{
#'   test <- mcn_5features
#'   
#'   ## the previous steps
#'   test1 <- filter_structure(test)
#'   test1 <- create_reference(test1)
#'   test1 <- filter_formula(test1, by_reference = T)
#'   test1 <- create_stardust_classes(test1)
#'   test1 <- create_features_annotation(test1)
#'   test1 <- cross_filter_stardust(test1, 2, 1)
#'   test1 <- create_nebula_index(test1)
#'   test1 <- compute_spectral_similarity(test1)
#'   test1 <- create_child_nebulae(test1, 0.01)
#'   test1 <- create_child_layouts(test1)
#'   test1 <- activate_nebulae(test1)
#'   
#'   ## set features quantification data
#'   ids <- features_annotation(test1)$.features_id
#'   quant. <- data.frame(
#'     .features_id = ids,
#'     sample_1 = rnorm(length(ids), 1000, 200),
#'     sample_2 = rnorm(length(ids), 2000, 500)
#'   )
#'   metadata <- data.frame(
#'     sample = paste0("sample_", 1:2),
#'     group = c("control", "model")
#'   )
#'   features_quantification(test1) <- quant.
#'   sample_metadata(test1) <- metadata
#'   
#'   ## optional 'nebula_name'
#'   visualize(test1)
#'   ## a class for example
#'   class <- visualize(test1)$class.name[1]
#'   tmp <- export_path(test1)
#'   test1 <- draw_structures(test1, class)
#'   test1 <- draw_nodes(test1, class)
#'   
#'   ## see results
#'   grobs <- nodes_grob(child_nebulae(test1))
#'   grobs
#'   grid::grid.draw(grobs[[1]])
#'   ## visualize with ID of 'feature' (.features_id)
#'   ## with legend
#'   ids <- names(grobs)
#'   x11(width = 9, height = 5)
#'   show_node(test1, ids[1])
#'   
#'   ## default parameters
#'   draw_nodes()
#'   
#'   unlink(tmp, T, T)
#' }
setMethod("draw_nodes", 
          signature = c(x = "mcnebula", nebula_name = "character",
                        nodes_color = "character",
                        add_id_text = "logical",
                        add_structure = "logical",
                        add_ppcp = "logical",
                        add_ration = "logical"),
          function(x, nebula_name, nodes_color, add_id_text, 
                   add_structure, add_ppcp, add_ration){
            if (add_ppcp) {
              if (length(ppcp_data(child_nebulae(x))) == 0)
                x <- set_ppcp_data(x)
            }
            if (add_ration) {
              if (length(ration_data(child_nebulae(x))) == 0)
                x <- set_ration_data(x)
            }
            if (add_structure) {
              if (length(ppcp_data(child_nebulae(x))) == 0)
                x <- draw_structures(x, nebula_name)
            }
            .features_id <-
              `[[`(tibble::as_tibble(tbl_graph(child_nebulae(x))[[nebula_name]]),
                   "name")
            .features_id <-
              unlist(lapply(.features_id,
                            function(id){
                              if (is.null(nodes_ggset(child_nebulae(x))[[id]]))
                                id
                            }))
            if (is.null(.features_id)) {
              return(x)
            }
            ggsets <- ggset_activate_nodes(x, .features_id, nodes_color,
                                     add_ppcp, add_ration)
            nodes_ggset(child_nebulae(x)) <-
              c(nodes_ggset(child_nebulae(x)), ggsets)
            path <- paste0(export_path(x), "/tmp/nodes")
            .check_path(path)
            .message_info("draw_nodes", "ggplot -> svg -> grob")
            grImport2:::setPrefix("")
            nodes_grob <- 
              pbapply::pbsapply(names(ggsets), simplify = F,
                                function(id){
                                  file <- paste0(path, "/", id, ".svg")
                                  svglite::svglite(file, bg = "transparent")
                                  ggset <- modify_rm_legend(ggsets[[ id ]])
                                  ggset <- modify_set_margin(ggset)
                                  print(call_command(ggset))
                                  if (add_structure) {
                                    vp <- grid::viewport(width = 0.8, height = 0.8)
                                    grid::pushViewport(vp)
                                    show_structure(x, id)
                                    grid::popViewport()
                                  }
                                  if (add_id_text) {
                                    label <- paste0("ID: ", id)
                                    grid::grid.draw(.grob_node_text(label))
                                  }
                                  dev.off()
                                  rsvg::rsvg_svg(file, file)
                                  .cairosvg_to_grob(file)
                                })
            nodes_grob(child_nebulae(x)) <- 
              c(nodes_grob(child_nebulae(x)), nodes_grob)
            return(x)
          })

#' @exportMethod show_node
#' @description \code{show_node()}: get the default parameters for the method
#' \code{show_node}.
#' @rdname draw_nodes-methods
setMethod("show_node", 
          signature = setMissing("show_node",
                                 x = "missing"),
          function(){
            list(panel_viewport =
                 grid::viewport(0, 0.5, 0.4, 1, just = c("left", "centre")),
               legend_viewport =
                 grid::viewport(0.4, 0.5, 0.6, 1, just = c("left", "centre"))
            )
          })

#' @exportMethod show_node
#'
#' @aliases show_node
#'
#' @description Visualize the node of 'feature' which has been drawn
#' by methods [draw_nodes()] (or drawn by methods [annotate_nebula()]).
#' @description \code{show_node(x, ...)}: use the default parameters whatever 'missing'
#' while performing the method \code{show_node}.
#'
#' @param x [mcnebula-class] object.
#' @param .features_id character(1). ID of 'feature' to show node.
#' @param panel_viewport 'viewport' object. Create by [grid::viewport()].
#' @param legend_viewport 'viewport' object.
#'
#' @rdname draw_nodes-methods
#'
setMethod("show_node", 
          signature = c(x = "ANY", .features_id = "character"),
          function(x, .features_id, panel_viewport, legend_viewport){
            args <- .fresh_param(show_node())
            args$.features_id <- .features_id
            do.call(.show_node, args)
          })

.show_node <-
  function(x, .features_id, panel_viewport, legend_viewport){
    grob <- nodes_grob(child_nebulae(x))[[.features_id]]
    if (is.null(grob))
      stop("the node of `.features_id` has not been drawn")
    .message_info_viewport("BEGIN")
    if (!is.null(panel_viewport)) {
      .check_class(panel_viewport, "viewport", "grid::viewport")
      grid::pushViewport(panel_viewport)
      upper <- T
    } else {
      upper <- F
    }
    .message_info_viewport()
    grid::grid.draw(grob)
    if (upper) {
      grid::upViewport()
    } else {
      return(message(""))
    }
    if (!is.null(legend_viewport)) {
      .check_class(legend_viewport, "viewport", "grid::viewport")
      .message_info_viewport()
      grid::pushViewport(legend_viewport)
      p <- call_command(nodes_ggset(child_nebulae(x))[[.features_id]])
      grid::grid.draw(.get_legend(p))
    }
    .message_info_viewport("END")
  }

#' @description \code{ggset_activate_nodes}:
#' create the [ggset-class] object of node of specified 'feature'.
#' @rdname draw_nodes-methods
#' @export 
ggset_activate_nodes <- 
  function(x, .features_id, nodes_color = "#FFF9F2",
           add_ppcp = T, add_ration = T){
    if (add_ppcp) {
      .check_data(child_nebulae(x), list(ppcp_data = "set_ppcp_data"))
    }
    nodes_color <- .as_dic(nodes_color, .features_id, "#FFF9F2")
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
                         .command_node_nuclear(nodes_color[[id]]),
                         .command_node_border(),
                         .command_node_radial_bar(),
                         .command_node_ylim(),
                         .command_node_polar(),
                         .command_node_fill(pal, labels),
                         new_command(theme_void),
                         .command_node_theme()
               )
             })
    if (add_ration) {
      .check_data(child_nebulae(x), list(ration_data = "set_ration_data"))
      axis.len <- vapply(set, function(df) tail(df$seq, n = 1), 1) + 1
      set <- .prepare_data_for_ration(x, .features_id, axis.len)
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
                                     .command_node_ration(set[[id]]),
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

.prepare_data_for_ration <- 
  function(x, .features_id, axis.len){
    set <- ration_data(child_nebulae(x))
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

#' @aliases set_ppcp_data
#'
#' @title Custom specify PPCP data for visualization in nodes
#'
#' @description
#' Run before [annotate_nebula()] or [draw_nodes()].
#' Custom specify PPCP data for visualization in nodes.
#' All chemical classes exists in PPCP data could be specified.
#' 
#' @seealso [annotate_nebula()], [draw_nodes()].
#'
#' @name set_ppcp_data-methods
#'
#' @order 1
NULL
#> NULL

#' @exportMethod set_ppcp_data
#' @description \code{set_ppcp_data()}: get the function for generating
#' default parameters for the method
#' \code{set_ppcp_data}.
#' @rdname set_ppcp_data-methods
setMethod("set_ppcp_data", 
          signature = setMissing("set_ppcp_data"),
          function(){
            function(x){
              list(classes = names(tbl_graph(child_nebulae(x))))
            }
          })

#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @exportMethod set_ppcp_data
#' @description \code{set_ppcp_data(x, ...)}:
#' use the default parameters whatever 'missing'
#' while performing the method \code{set_ppcp_data}.
#' @rdname set_ppcp_data-methods
setMethod("set_ppcp_data", 
          signature = c(x = "mcnebula"),
          function(x, classes){
            reCallMethod("set_ppcp_data",
                         .fresh_param(set_ppcp_data()(x)))
          })

#' @exportMethod set_ppcp_data
#'
#' @param x [mcnebula-class] object.
#' @param classes character. The names of chemical classes.
#' Use \code{classification(object)} to get optional candidates.
#'
#' @rdname set_ppcp_data-methods
#'
#' @examples
#' \dontrun{
#'   test <- mcn_5features
#'   
#'   ## the previous steps
#'   test1 <- filter_structure(test)
#'   test1 <- create_reference(test1)
#'   test1 <- filter_formula(test1, by_reference = T)
#'   test1 <- create_stardust_classes(test1)
#'   test1 <- create_features_annotation(test1)
#'   test1 <- cross_filter_stardust(test1, 2, 1)
#'   test1 <- create_nebula_index(test1)
#'   test1 <- compute_spectral_similarity(test1)
#'   test1 <- create_child_nebulae(test1, 0.01)
#'   test1 <- create_child_layouts(test1)
#'   test1 <- activate_nebulae(test1)
#'   
#'   ## optional 'nebula_name'
#'   visualize(test1)
#'   ## a class for example
#'   class <- visualize(test1)$class.name[1]
#'   tmp <- export_path(test1)
#'   ## customize the chemical classes displayed
#'   ## in the radial bar plot in node.
#'   classes <- classification(test1)
#'   ## get some random classes
#'   set.seed(10)
#'   classes <- sample(classes$class.name, 50)
#'   classes
#'   test1 <- set_ppcp_data(test1, classes)
#'   test1 <- draw_nodes(test1, class,
#'     add_structure = F,
#'     add_ration = F
#'   )
#'   
#'   ## visualize with ID of 'feature' (.features_id)
#'   ## with legend
#'   ids <- names(nodes_grob(child_nebulae(test1)))
#'   x11(width = 15, height = 5)
#'   show_node(test1, ids[1])
#'   
#'   ## get a function to generate default parameters
#'   set_ppcp_data()
#'   ## the default parameters
#'   set_ppcp_data()(test1)
#'   
#'   unlink(tmp, T, T)
#' }
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

#' @aliases set_ration_data
#'
#' @title Custom specify the quantification data for visualization in nodes
#'
#' @description
#' Run before [annotate_nebula()] or [draw_nodes()].
#' Set whether to use the group average value to annotate the 'features'
#' quantification in nodes.
#' Before this methods, user should use \code{features_quantification<-} and
#' \code{sample_metadata<-} to set quantification data and metadata in
#' [mcnebula-class] object.
#' 
#' @seealso [annotate_nebula()], [draw_nodes()].
#'
#' @name set_ration_data-methods
#'
#' @order 1
NULL
#> NULL

#' @importFrom tidyr gather
#' @importFrom tibble as_tibble
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @exportMethod set_ration_data
#' @description \code{set_ration_data()}: get the default parameters for the method
#' \code{set_ration_data}.
#' @rdname set_ration_data-methods
setMethod("set_ration_data", 
          signature = setMissing("set_ration_data"),
          function(){
            list(mean = T)
          })

#' @exportMethod set_ration_data
#' @description \code{set_ration_data(x, ...)}: use the default parameters whatever 'missing'
#' while performing the method \code{set_ration_data}.
#' @rdname set_ration_data-methods
setMethod("set_ration_data", 
          signature = c(x = "mcnebula"),
          function(x, mean){
            reCallMethod("set_ration_data",
                         .fresh_param(set_ration_data()))
          })

#' @exportMethod set_ration_data
#'
#' @param x [mcnebula-class] object.
#' @param mean logical. If \code{TRUE}, calculate mean value for
#' all group of the samples.
#'
#' @rdname set_ration_data-methods
#'
#' @examples
#' \dontrun{
#'   test <- mcn_5features
#'   
#'   ## the previous steps
#'   test1 <- filter_structure(test)
#'   test1 <- create_reference(test1)
#'   test1 <- filter_formula(test1, by_reference = T)
#'   test1 <- create_stardust_classes(test1)
#'   test1 <- create_features_annotation(test1)
#'   test1 <- cross_filter_stardust(test1, 2, 1)
#'   test1 <- create_nebula_index(test1)
#'   test1 <- compute_spectral_similarity(test1)
#'   test1 <- create_child_nebulae(test1, 0.01)
#'   test1 <- create_child_layouts(test1)
#'   test1 <- activate_nebulae(test1)
#'   
#'   ## set features quantification data
#'   ids <- features_annotation(test1)$.features_id
#'   quant. <- data.frame(
#'     .features_id = ids,
#'     sample_1 = rnorm(length(ids), 1000, 200),
#'     sample_2 = rnorm(length(ids), 2000, 500)
#'   )
#'   quant. <- dplyr::mutate(quant.,
#'     sample_3 = sample_1 * 1.5,
#'     sample_4 = sample_2 * 5
#'   )
#'   metadata <- data.frame(
#'     sample = paste0("sample_", 1:4),
#'     group = rep(c("control", "model"), c(2, 2))
#'   )
#'   features_quantification(test1) <- quant.
#'   sample_metadata(test1) <- metadata
#'   
#'   ## a more convenient way to obtain simulation data
#'   # test1 <- MCnebula2:::.simulate_quant_set(test1)
#'   
#'   ## optional 'nebula_name'
#'   visualize(test1)
#'   ## a class for example
#'   class <- visualize(test1)$class.name[1]
#'   tmp <- export_path(test1)
#'   
#'   test1 <- set_ration_data(test1, mean = F)
#'   test1 <- draw_nodes(test1, class,
#'     add_structure = F,
#'     add_ppcp = F
#'   )
#'   
#'   ## visualize with ID of 'feature' (.features_id)
#'   ## with legend
#'   ids <- names(nodes_grob(child_nebulae(test1)))
#'   x11(width = 15, height = 5)
#'   show_node(test1, ids[1])
#'   
#'   ## the default parameters
#'   set_ration_data()
#'   
#'   unlink(tmp, T, T)
#' }
setMethod("set_ration_data", 
          signature = c(x = "mcnebula", mean = "logical"),
          function(x, mean){
            .check_data(x, list(features_quantification = "features_quantification",
                                sample_metadata = "sample_metadata"), "(x) <-")
            ration_data <-
              tidyr::gather(features_quantification(x),
                            key = "sample", value = "value", -.features_id)
            ration_data <-
              tibble::as_tibble(merge(ration_data, sample_metadata(x),
                                      by = "sample", all.x = T))
            if (mean) {
              ration_data <-
                dplyr::summarise(dplyr::group_by(ration_data, .features_id, group),
                                 value = mean(value, na.rm = T))
              ration_data <- dplyr::ungroup(ration_data)
            }
            ration_data(child_nebulae(x)) <- 
              split(ration_data, ~.features_id)
            return(x)
          })
