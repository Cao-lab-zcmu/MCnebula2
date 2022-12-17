# ==========================================================================
# create nebula index from filtered stardust classes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases create_nebula_index
#'
#' @title Set down the chemical classes for visualization
#'
#' @description
#' Arrange the filtered 'stardust_classes' data as 'nebula_index' data.
#' The chemical classes in 'nebula_index' data would be visualized as Child-Nebulae.
#' Run after [cross_filter_stardust()].
#'
#' @name create_nebula_index-methods
#'
#' @order 1
NULL
#> NULL

#' @exportMethod create_nebula_index
#' @description \code{create_nebula_index()}: get the default parameters for the method
#' \code{create_nebula_index}.
#' @rdname create_nebula_index-methods
setMethod("create_nebula_index", 
          signature = setMissing("create_nebula_index"),
          function(){
            list(force = F)
          })

#' @exportMethod create_nebula_index
#' @description \code{create_nebula_index(x, ...)}: use the default parameters whatever 'missing'
#' while performing the method \code{create_nebula_index}.
#' @rdname create_nebula_index-methods
setMethod("create_nebula_index", 
          signature = c(x = "mcnebula"),
          function(x, force){
            reCallMethod("create_nebula_index",
                         .fresh_param(create_nebula_index()))
          })

#' @exportMethod create_nebula_index
#'
#' @aliases create_nebula_index
#'
#' @param x [mcnebula-class] object.
#' @param force logical. The number of chemical classes in 'stardust_classes' data
#' would be checked. The maximum is 120. If there were too many classes, return
#' with error. Set to \code{FALSE}, escape from maximum check.
#'
#' @rdname create_nebula_index-methods
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
#'   
#'   test1 <- create_nebula_index(test1)
#'   ## see results
#'   nebula_index(test1)
#'   ## or
#'   reference(test1)$nebula_index
#'   ## or
#'   reference(mcn_dataset(test1))$nebula_index
#' }
setMethod("create_nebula_index", 
          signature = c(x = "mcnebula", force = "logical"),
          function(x, force){
            .message_info_formal("MCnebula2", "create_nebula_index")
            .check_data(x, list(stardust_classes = "create_stardust_classes"))
            if (!force) {
              class_num <- length(unique(stardust_classes(x)[[ "rel.index" ]]))
              if (class_num > 120)
                stop("too many classes; ",
                     "length(unique(stardust_classes(x)$rel.index)) > 120")
            }
            reference(mcn_dataset(x))[[ "nebula_index" ]] <- 
              dplyr::select(stardust_classes(x), rel.index, class.name,
                            hierarchy, .features_id)
            return(x)
          })

#' @aliases set_tracer
#'
#' @title Mark top 'features' in 'nebula_index' data
#'
#' @description
#' Custom defined the specific 'features' in 'nebula_index' data.
#' Mark these 'features' for subsequent visualization with eye-catching highlighting
#' ([set_nodes_color()]).
#' Run after [create_nebula_index()].
#'
#' @name set_tracer-methods
#'
#' @order 1
NULL
#> NULL

#' @exportMethod set_tracer
#' @description \code{set_tracer()}: get the function for generating
#' default parameters for the method
#' \code{set_tracer}.
#' @rdname set_tracer-methods
setMethod("set_tracer", 
          signature = setMissing("set_tracer"),
          function(){
            function(x, .features_id){
              if (length(.features_id) > length(palette_set(x)))
                stop("too much specified features; ",
                     "use 'palette_set<-' to set more colors")
              colors <- .as_dic(palette_set(x), .features_id,
                                as.list = F, na.rm = T)
              list(.features_id = .features_id,
                   colors = unname(colors),
                   rest = "#D9D9D9"
              )
            }
          })

#' @exportMethod set_tracer
#' @description \code{set_tracer(x, ...)}: use the default parameters whatever 'missing'
#' while performing the method \code{set_tracer}.
#' @rdname set_tracer-methods
setMethod("set_tracer", 
          signature = c(x = "mcnebula"),
          function(x, .features_id, colors, rest){
            args <- as.list(environment())
            args$.features_id <- .features_id
            reCallMethod("set_tracer",
                         .fresh_param(set_tracer()(x, .features_id), args))
          })

#' @exportMethod set_tracer
#'
#' @aliases set_tracer
#'
#' @param x [mcnebula-class] object.
#' @param .features_id character. The ID of 'features' to mark.
#' @param colors character. Hex color.
#' @param rest character(1). Hex color.
#'
#' @seealso [create_nebula_index()], [set_nodes_color()].
#'
#' @rdname set_tracer-methods
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
#'   
#'   ids <- features_annotation(test1)$.features_id
#'   test1 <- set_tracer(test1, ids[1:2])
#'   ## see results
#'   nebula_index(test1)
#'   
#'   ## see examples in 'set_nodes_color()'
#' }
setMethod("set_tracer", 
          signature = c(x = "mcnebula", .features_id = "character",
                        colors = "character", rest = "character"),
          function(x, .features_id, colors, rest){
            .check_data(x, list(nebula_index = "create_nebula_index"))
            if (length(.features_id) != length(colors))
              stop("length(.features_id) != length(colors))")
            tracer_color <- data.frame(.features_id = .features_id,
                                       tracer_color = colors)
            nebula_index <- nebula_index(x)
            nebula_index$tracer <- NULL
            nebula_index$tracer_color <- NULL
            nebula_index <- merge(nebula_index, tracer_color,
                                  by = ".features_id", all.x = T)
            nebula_index <-
              dplyr::mutate(nebula_index,
                            tracer = ifelse(is.na(tracer_color), F, T),
                            tracer_color = ifelse(tracer, tracer_color, rest))
            reference(mcn_dataset(x))[[ "nebula_index" ]] <-
              dplyr::arrange(dplyr::relocate(tibble::as_tibble(nebula_index),
                                             .features_id,
                                             .after = hierarchy), rel.index)
            return(x)
          })

