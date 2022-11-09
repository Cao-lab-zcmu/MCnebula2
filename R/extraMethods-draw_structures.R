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
#' structures.
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
#'
#' @rdname draw_structures-methods
#'
#' @examples
#' \dontrun{
#' draw_structures(...)
#' }
setMethod("draw_structures", 
          signature = c(x = "mcnebula", nebula_name = "character"),
          function(x, nebula_name){
            .check_data(child_nebulae(x), list(tbl_graph = "create_child_layouts"))
            tidy <- tbl_graph(child_nebulae(x))[[nebula_name]]
            if (is.null(tidy))
              stop( "`nebula_name` not found in `tbl_graph(child_nebulae(x))`" )
            df <- dplyr::select(tibble::as_tibble(tidy), .features_id = name, smiles)
            sets <- structures_grob(child_nebulae(x))
            if (!is.null(sets)) {
              df <- dplyr::filter(df, !.features_id %in% names(sets))
            }
            if (!nrow(df) == 0) {
              structures_grob(child_nebulae(x)) <- 
                c(sets, .draw_structures(df, paste0(export_path(x), "/tmp/structure"), T))
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
#' @param .features_id character(1). The ID of 'features' to show the
#' chemical structure.
#'
#' @rdname draw_structures-methods
#'
#' @examples
#' \dontrun{
#' show_structure(...)
#' }
setMethod("show_structure", 
          signature = c(x = "ANY", .features_id = "character"),
          function(x, .features_id){
            .check_data(child_nebulae(x), list(structures_grob = "draw_structures"))
            grid::grid.draw(structures_grob(child_nebulae(x))[[.features_id]])
          })

#' @importFrom dplyr mutate
#' @importFrom pbapply pbapply
.draw_structures <-
  function(df, path, rm_backgroud = F){
    .check_columns(df, c(".features_id", "smiles"), "data.frame")
    .check_path(path)
    df <- dplyr::mutate(df, path = paste0(!!path, "/", .features_id, ".svg"))
    df <- dplyr::filter(df, !is.na(smiles))
    .message_info("draw_structures", "smiles -> svg -> grob")
    grImport2:::setPrefix("")
    lst <- pbapply::pbapply(dplyr::select(df, smiles, path), 1,
                            function(vec){
                              .smiles_to_cairosvg(vec[["smiles"]], vec[["path"]])
                              .cairosvg_to_grob(vec[["path"]])
                            })
    names(lst) <- df$.features_id
    if (rm_backgroud) 
      lst <- lapply(lst, .rm_backgroud)
    return(lst)
  }

.rm_backgroud <- 
  function(grob){
    if (!is(grob$children[[1]]$children[[1]], "picRect")) {
      stop("'picRect' not matched.")
    }
    grob$children[[1]]$children[[1]] <- NULL
    return(grob)
  }

#' @importFrom ChemmineOB convertToImage
#' @importFrom rsvg rsvg_svg
.smiles_to_cairosvg <- 
  function(smile, path){
    ChemmineOB::convertToImage("SMI", "SVG", source = smile, toFile = path)
    rsvg::rsvg_svg(path, path)
  }
