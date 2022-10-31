# ==========================================================================
# create nebula index from filtered stardust classes
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
#' @title ...
#'
#' @description ...
#'
#' @details ...
#'
#' @param x ...
#' @param force ...
#'
# @inheritParams rdname
#'
#' @return ...
#'
# @seealso ...
#'
#' @rdname create_nebula_index-methods
#'
#' @order 1
#'
#' @examples
#' \dontrun{
#' create_nebula_index(...)
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
              colors <-
                MCnebula2:::.as_dic(palette_set(x), .features_id,
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
#' @title ...
#'
#' @description ...
#'
#' @details ...
#'
#' @param x ...
#' @param .features_id ...
#' @param colors ...
#' @param rest ...
#'
# @inheritParams rdname
#'
#' @return ...
#'
#' @seealso [create_nebula_index()]
#'
#' @rdname set_tracer-methods
#'
#' @order 1
#'
#' @examples
#' \dontrun{
#' set_tracer(...)
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
            nebula_index <- merge(nebula_index(x), tracer_color,
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

