# ==========================================================================
# set default value for project of MCnebula
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @aliases initialize_mcnebula
#'
#' @title Initialize mcnebula object
#'
#' @description
#' Set SIRIUS project path and its version to initialize [mcnebula-class] object.
#' In addition, the methods can be used for some related object to given
#' default value.
#'
#' @name initialize_mcnebula-methods
#'
#' @order 1
NULL
#> NULL

#' @exportMethod initialize_mcnebula
#'
#' @aliases initialize_mcnebula
#'
#' @param x [mcnebula-class] object, [melody-class] object,
#' [project_conformation-class] or [project_api-class] object.
#' @param sirius_version character. e.g., "sirius.v4".
#' @param sirius_project character. The path of SIRIUS project space.
#' @param output_directory character. The path for output.
#'
#' @rdname initialize_mcnebula-methods
#'
#' @examples
#' \dontrun{
#' initialize_mcnebula(...)
#' }
setMethod("initialize_mcnebula", 
          signature = c(x = "mcnebula",
                        sirius_version = "ANY",
                        sirius_project = "ANY",
                        output_directory = "ANY"),
          function(x, sirius_version, sirius_project, output_directory){
            if (missing(sirius_version))
              sirius_version <- project_version(x)
            else
              project_version(x) <- sirius_version
            if (missing(sirius_project))
              sirius_project <- project_path(x)
            else
              project_path(x) <- sirius_project
            if (missing(output_directory)) {
              if (length(export_path(x)) == 0) {
                export_path(x) <- paste0(sirius_project, "/mcnebula_results")
              }
            } else {
              export_path(x) <- output_directory
            }
            match.fun(paste0(".validate_", sirius_version))(sirius_project)
            item <- methods(initialize_mcnebula)
            item <- stringr::str_extract(item, "(?<=,).*(?=-method)")
            item <- gsub(",.*$", "", item)
            item <- item[item != "mcnebula"]
            for(i in item){
              express <- paste0(i, "(x)",
                                "<- initialize_mcnebula(",
                                ## initialize slot
                                i, "(x)", ", ",
                                ## other args
                                "sirius_version = sirius_version,",
                                "sirius_project = sirius_project",
                                ")")
              eval( parse(text = express) )
            }
            export_name(x) <- .get_export_name()
            return(x)
          })

#' @exportMethod initialize_mcnebula
#'
#' @aliases initialize_mcnebula
#'
#' @seealso [ggsci::pal_simpsons()], [ggsci::pal_igv()], [ggsci::pal_ucscgb()],
#' [ggsci::pal_d3()]...
#'
#' @rdname initialize_mcnebula-methods
#'
#' @examples
#' \dontrun{
#' initialize_mcnebula(...)
#' }
setMethod("initialize_mcnebula", 
          signature = c(x = "melody"),
          function(x){
            ## set color palette
            palette_set(x) <- .get_color_set()
            palette_gradient(x) <- .get_color_gradient()
            palette_stat(x) <- .get_color_stat()
            palette_col(x) <- .get_color_col()
            palette_label(x) <- .get_label_color()
            return(x)
          })

#' @exportMethod initialize_mcnebula
#' @rdname initialize_mcnebula-methods
setMethod("initialize_mcnebula", 
          signature = c(x = "project_conformation",
                        sirius_version = "character"),
          function(x, sirius_version){
            slots <- names(attributes(x))
            slots <- slots[-length(slots)]
            for (i in slots) {
              express <- 
                paste0( i, "(x)", "<-", ".get_", i, "_", sirius_version, "()")
              eval( parse(text = express) )
            }
            return(x)
          })

#' @exportMethod initialize_mcnebula
#' @rdname initialize_mcnebula-methods
setMethod("initialize_mcnebula", 
          signature = c(x = "project_api",
                        sirius_version = "character"),
          function(x, sirius_version){
            express <- paste0("function(x) format_msframe(",
                              "x,",
                              "fun_names = .get_attribute_name_", sirius_version, ",",
                              "fun_types = .get_attribute_type_", sirius_version, "",
                              ")")
            methods_format(x) <- eval( parse(text = express) )
            express <- paste0(".get_methods_read_", sirius_version, "()")
            methods_read(x) <- eval( parse(text = express) )
            express <- paste0(".get_methods_match_", sirius_version, "()")
            methods_match(x) <- eval( parse(text = express) )
            return(x)
          })
