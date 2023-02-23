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

#' @importFrom methods getFunction
#' @exportMethod initialize_mcnebula
#'
#' @aliases initialize_mcnebula
#'
#' @param x [mcnebula-class] object, [melody-class] object,
#' [project_conformation-class] or [project_api-class] object.
#' @param sirius_version character. e.g., "sirius.v4", "sirius.v5"
#' @param sirius_project character. The path of SIRIUS project space.
#' @param output_directory character. The path for output.
#'
#' @rdname initialize_mcnebula-methods
#'
#' @examples
#' \dontrun{
#'   ## The raw data used for the example
#'   tmp <- paste0(tempdir(), "/temp_data")
#'   dir.create(tmp)
#'   eg.path <- system.file("extdata", "raw_instance.tar.gz",
#'                          package = "MCnebula2")
#'   
#'   utils::untar(eg.path, exdir = tmp)
#'   
#'   ## initialize 'mcnebula' object
#'   test <- mcnebula()
#'   test <- initialize_mcnebula(test, "sirius.v4", tmp)
#'   ## check the setting
#'   export_path(test)
#'   palette_set(test)
#'   ion_mode(test)
#'   project_version(test)
#'   
#'   ## initialize 'melody' object
#'   test <- new("melody")
#'   test <- initialize_mcnebula(test)
#'   ## check...
#'   palette_stat(test)
#'   
#'   ## initialize 'project_conformation' object
#'   test <- new("project_conformation")
#'   test <- initialize_mcnebula(test, "sirius.v4")
#'   ## check
#'   file_name(test)
#'   
#'   ## initialize 'project_api' object
#'   test <- new("project_api")
#'   test <- initialize_mcnebula(test, "sirius.v4")
#'   ## check
#'   methods_format(test)
#'   
#'   unlink(tmp, T, T)
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
              if (length(x@export_path) == 0) {
                export_path(x) <- paste0(sirius_project, "/mcnebula_results")
              }
            } else {
              export_path(x) <- output_directory
            }
            getFunction(paste0(".validate_", sirius_version),
                        where = parent.env(environment()))(sirius_project)
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
