# ==========================================================================
# draw all chemical structures for a specified child-nebula
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases draw_structures
#'
#' @title Draw and visualize chemcial structure
#'
#' @description
#' Methods used for drawing and visualizing chemical structures of 'features'
#' in Child-Nebulae.
#' [ChemmineOB::convertToImage()] is the core function used for drawing chemical
#' structures.Since 'ChemmineOB' has not been tested on Mac ARM64, this system 
#' needs to use obabel directly.
#' 
#' @seealso [ChemmineOB::convertToImage()].
#'
#' @name draw_structures-methods
#'
#' @order 1
NULL
#> NULL

#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom tibble as_tibble
#' @importFrom tibble as_tibble
#' @exportMethod draw_structures
#'
#' @aliases draw_structures
#'
#' @param x [mcnebula-class] object.
#' @param nebula_name character(1). Chemical classes in 'nebula_index' data.
#' Specified to draw chemical structures of all the 'features' of that.
#' @param .features_id character(1). The ID of 'features'.
#' @param data data.frame. A 'data.frame' contains columns of '.features_id' and
#' 'smiles'.
#' @param ... ...
#'
#' @rdname draw_structures-methods
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
#'   test1 <- draw_structures(test1, class)
#'   
#'   ## see results
#'   grobs <- structures_grob(child_nebulae(test1))
#'   grobs
#'   grid::grid.draw(grobs[[1]])
#'   ## visualize with ID of 'feature' (.features_id)
#'   ids <- names(grobs)
#'   show_structure(test1, ids[1])
#'   
#'   unlink(tmp, T, T)
#' }
setMethod("draw_structures", 
          signature = setMissing("draw_structures",
                                 x = "mcnebula",
                                 nebula_name = "character"),
          function(x, nebula_name, ...){
            .check_data(child_nebulae(x), list(tbl_graph = "create_child_layouts"))
            tidy <- tbl_graph(child_nebulae(x))[[nebula_name]]
            if (is.null(tidy))
              stop( "`nebula_name` not found in `tbl_graph(child_nebulae(x))`" )
            df <- dplyr::select(tibble::as_tibble(tidy), .features_id = name, smiles)
            draw_structures(x, data = df, ...)
          })

#' @exportMethod draw_structures
#' @rdname draw_structures-methods
setMethod("draw_structures", 
          signature = setMissing("draw_structures",
                                 x = "mcnebula",
                                 .features_id = "character"),
          function(x, .features_id, ...){
            .check_data(x, list(features_annotation = "create_features_annotation"))
            df <- dplyr::select(features_annotation(x), .features_id, smiles)
            df <- dplyr::filter(df, .features_id %in% !!.features_id)
            draw_structures(x, data = df, ...)
          })

#' @exportMethod draw_structures
#' @rdname draw_structures-methods
setMethod("draw_structures", 
          signature = setMissing("draw_structures",
                                 x = "mcnebula",
                                 data = "data.frame"),
          function(x, data, ...){
            sets <- structures_grob(child_nebulae(x))
            if (!is.null(sets)) {
              data <- dplyr::filter(data, !.features_id %in% names(sets))
            }
            if (!nrow(data) == 0) {
              structures_grob(child_nebulae(x)) <- 
                c(sets, .draw_structures(data, paste0(export_path(x),
                      "/tmp/structure"), T, ...))
            }
            return(x)
          })

#' @importFrom grid grid.draw
#' @exportMethod show_structure
#'
#' @description
#' \code{show_structure}: visualize the chemical structure of 'feature'
#' which has been drawn.
#'
#' @rdname draw_structures-methods
#'
setMethod("show_structure", 
          signature = c(x = "ANY", .features_id = "character"),
          function(x, .features_id){
            .check_data(child_nebulae(x), list(structures_grob = "draw_structures"))
            grid::grid.draw(structures_grob(child_nebulae(x))[[.features_id]])
          })

#' @importFrom dplyr mutate
#' @importFrom pbapply pbapply
.draw_structures <-
  function(df, path, rm_background = F, fun_draw = .smiles_to_cairosvg){
    .check_columns(df, c(".features_id", "smiles"), "data.frame")
    .check_path(path)
    df <- dplyr::mutate(df, path = paste0(!!path, "/", .features_id, ".svg"))
    df <- dplyr::filter(df, !is.na(smiles))
    if (nrow(df) == 0)
      return(NULL)
    .message_info("draw_structures", "smiles -> svg -> grob")
    grImport2:::setPrefix("")
    lst <- pbapply::pbapply(dplyr::select(df, smiles, path), 1,
                            function(vec){
                              fun_draw(vec[["smiles"]], vec[["path"]])
                              .cairosvg_to_grob(vec[["path"]])
                            })
    names(lst) <- df$.features_id
    if (rm_background) 
      lst <- lapply(lst, .rm_background)
    return(lst)
  }

.rm_background <- 
  function(grob){
    if (is(grob$children[[1]]$children[[1]], "picRect")) {
      grob$children[[1]]$children[[1]] <- NULL
    }
    return(grob)
  }

# @importFrom ChemmineOB convertToImage
#' @importFrom rsvg rsvg_svg
.smiles_to_cairosvg <- 
  function(smile, path){
    if (requireNamespace("ChemmineOB", quietly = TRUE)) {
      ChemmineOB::convertToImage("SMI", "SVG", source = smile, toFile = path)
    } else if (Sys.info()["sysname"] == "Darwin" && Sys.which("obabel") != "") {
      system(paste0("obabel -:", "'", smile, "'", " -O ", path))
    } else stop("Neither ChemmineOb nor open-babel(on Mac ARM64) is available.")
    if (packageVersion("rsvg") > "2.4.0") {
      raw <- charToRaw(sub("<rect[^>]*/>", "", paste0(readLines(path), collapse = "\n")))
      rsvg::rsvg_svg(raw, path)
    } else {
      rsvg::rsvg_svg(path, path)
    }
  }
