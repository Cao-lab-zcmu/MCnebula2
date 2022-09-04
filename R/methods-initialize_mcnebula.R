# ==========================================================================
# set default value for project of MCnebula
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("initialize_mcnebula", 
          signature = c(x = "mcnebula"),
          function(x){
            item <- methods(initialize_mcnebula)
            item <- stringr::str_extract(item, "(?<=,).*(?=-method)")
            item <- gsub(",.*$", "", item)
            item <- item[item != "mcnebula"]
            for(i in item){
              express <- paste0(i, "(x)",
                                "<- initialize_mcnebula(",
                                ## initialize slot
                                i, "(x)",
                                ", ",
                                ## other args
                                "sirius_version = sirius_version(x)",
                                ")")
              eval( parse(text = express) )
            }
            return(x)
          })
setMethod("initialize_mcnebula", 
          signature = c(x = "mcn_path"),
          function(x){
            ## ------------------------------------- 
            ## set path
            sirius_project(x) <- "."
            output_directory(x) <- paste0(sirius_project(x), "/mcnebula_results")
            return(x)
          })
setMethod("initialize_mcnebula", 
          signature = c(x = "mcn_palette"),
          function(x){
            ## set color palette
            colors <- .get_color_set()
            palette_set(x) <- colors
            palette_stat(x) <- colors
            palette_ppcp(x) <- colors
            palette_label(x) <- .get_label_color()
            return(x)
          })
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
setMethod("initialize_mcnebula", 
          signature = c(x = "project_api",
                        sirius_version = "character"),
          function(x, sirius_version){
            express <- paste0("function(x) format_msframe(",
                              "x,",
                              "fun_names = .get_attribute_name_", sirius_version, "(),",
                              "fun_types = .get_attribute_type_", sirius_version, "()",
                              ")")
            methods_format(x) <- eval( parse(text = express) )
            express <- paste0(".get_methods_read_", sirius_version, "()")
            methods_read(x) <- eval( parse(text = express) )
            express <- paste0(".get_methods_match_", sirius_version, "()")
            methods_match(x) <- eval( parse(text = express) )
            return(x)
          })
