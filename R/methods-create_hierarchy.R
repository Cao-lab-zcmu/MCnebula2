# ==========================================================================
# collate and build classification hierarchy annotation data
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("create_hierarchy", 
          signature = c(x = "mcnebula"),
          function(x){
            create_hierarchy(x, .build_hierarchy)
          })
setMethod("create_hierarchy", 
          signature = c(x = "mcnebula", fun_organize = "function"),
          function(x, fun_organize){
            class <- classification(x)
            if (is.null(class)) {
              x <- collate_data(x, ".canopus")
              class <- classification(x)
            }
            reference(mcn_dataset(x))[[ "hierarchy" ]] <- 
              fun_organize(class)
            return(x)
          })
.build_hierarchy <- 
  function(data){
    data <- dplyr::select(data, rel.index, chem.ont.id,
                           class.name, parent.chem.ont.id)
    root <- data[data$parent.chem.ont.id == "", ]
    list <- list()
    length(list) <- 12
    n <- 1
    list[[n]] <- root
    df <- data[data$parent.chem.ont.id %in% root$chem.ont.id, ]
    ## ---------------------------------------------------------------------- 
    while(nrow(df) > 0){
      n <- n + 1
      list[[n]] <- df
      df <- data[data$parent.chem.ont.id %in% df$chem.ont.id, ]
    }
    data <- data.table::rbindlist(list, idcol = T)
    data$.id <- data$.id - 1
    dplyr::rename(dplyr::as_tibble(data), hierarchy = .id)
  }
