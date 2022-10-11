# ==========================================================================
# draw all chemical structures for a specified child-nebula
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom tibble as_tibble
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
    .print_info("draw_structures", "convert smiles")
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
#' @importFrom ChemmineOB convertToImage
#' @importFrom rsvg rsvg_svg
.smiles_to_cairosvg <- 
  function(smile, path){
    ChemmineOB::convertToImage("SMI", "SVG", source = smile, toFile = path)
    rsvg::rsvg_svg(path, path)
  }
#' @importFrom grImport2 readPicture
#' @importFrom grImport2 grobify
.cairosvg_to_grob <- 
  function(path){
    grImport2::grobify(grImport2::readPicture(path))
  }
.rm_backgroud <- 
  function(grob){
    if (!is(grob$children[[1]]$children[[1]], "picRect")) {
      stop("'picRect' not matched.")
    }
    grob$children[[1]]$children[[1]] <- NULL
    return(grob)
  }
