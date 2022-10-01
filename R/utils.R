# ==========================================================================
# additional function
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMissing <- 
  function(generic, ..., .SIG = "missing"){
    args <- list(...)
    sig <- getGeneric(generic)@signature
    res <- vapply(sig, FUN.VALUE = "character",
                  function(name){
                    if (is.null(args[[ name ]]))
                      .SIG
                    else
                      args[[ name ]]
                  })
    names(res) <- sig
    return(res)
  }
match_methods <- 
  function(name, classes){
    methods <- showMethods(classes = classes, printTo = FALSE)
    methods <- methods[ grep(paste0("^Function: ", name), methods, perl = T) ]
    vapply(strsplit(methods, " "), `[`, "character", 2)
  }
list_unique_by_names <- 
  function(lst){
    unique <- data.frame(names = names(lst),
                         order = 1:length(lst))
    unique <- unique[!duplicated(unique$names), ]
    lst[unique$order]
  }
vec_unique_by_value <- 
  function(vec){
    unique <- data.frame(value = vec,
                         order = 1:length(vec))
    unique <- unique[!duplicated(unique$value), ]
    vec[unique$order]
  }
## ------------------------------------- 
slots_mapply <- 
  function(x, fun, ...){
    slots <- attributes(x)
    slots <- slots[-length(slots)]
    res <- mapply(fun, slot = slots, name = names(slots), ...)
    return(res)
  }
## ------------------------------------- 
mapply_rename_col <- 
  function(
           mutate_set,
           replace_set,
           names,
           fixed = F
           ){
    envir <- environment()
    mapply(mutate_set, replace_set,
           MoreArgs = list(envir = envir, fixed = fixed),
           FUN = function(mutate, replace, envir,
                          fixed = F, names = get("names", envir = envir)){
             names <- gsub(mutate, replace, names, perl = ifelse(fixed, F, T), fixed = fixed)
             assign("names", names, envir = envir)
           })
    return(names)
  }
## ------------------------------------- 
.show <- 
  function(object){
    cat(class(object), "\n")
    slots_mapply(object, function(names, slots){
              cat(names, ":\n", sep = "")
              cat(str(slots))
              cat("\n\n")
           })
  }
## ------------------------------------- 
.print_info <- 
  function(main, sub, arg = NULL, sig = "##"){
    cat(sig, " ", main, ": ", sub, " ", arg, "\n", sep = "")
  }
.print_info_formal <- 
  function(main, sub, arg = NULL, sig = "[INFO]"){
    cat(sig, " ", main, ": ", sub, " ", arg, "\n", sep = "")
  }
.print_info_viewport <- 
  function(info = "info"){
    .print_info(info, "current.viewport:",
                paste0("\n\t", paste0(current.viewport())))
  }
.get_missing_x <- 
  function(x, class, n = 2, envir = parent.frame(n)){
    if (missing(x)) {
      x <- get("x", envir = envir)
      if (!is(x, class)) {
        stop( paste0("there must be an `x` of '", class, 
                     "' in `parent.frame(", n - 1, ")`" ) )
      }
    }
    return(x)
  }
## ------------------------------------- 
.check_data <- 
  function(object, lst){
    mapply(lst, names(lst), FUN = function(value, name){
             if (is.null(match.fun(name)(object))) {
               stop(paste0("is.null(", name, "(x)) == T. ",
                           "use `", value, "(...)` previously."))
             }
           })
  }
.check_names <- 
  function(param, formal, tip1, tip2){
    if (!is.null(names(param))) {
      if ( any(!names(formal) %in% names(param)) ) {
        stop(paste0("the names of `", tip1, "` must contain all names of ",
                    tip2, "; or without names."
                    ))
      }
    }
  }
.check_class <- 
  function(object, class = "layout", tip = "grid::grid.layout"){
    if (!is(object, class)) {
      stop(paste0("`", paste0(substitute(object), collapse = ""),
                  "` should be a '", class, "' object created by ",
                  "`", tip, "`." ))
    }
  }
## ------------------------------------- 
.list_files <- function(path, upper, pattern){
  lst_file <- pbapply::pbmapply(path, upper, pattern, SIMPLIFY = F,
                     FUN = function(path, upper, pattern){
                       files <- list.files(paste0(path, "/", upper), pattern)
                       if ( length(files) == 0)
                         return( data.frame() )
                       data.frame(upper = upper, files = files)
                     })
  data.table::rbindlist(lst_file)
}
## ------------------------------------- 
read_tsv <- function(path){
  file <- data.table::fread(input=path, sep="\t", header=T, quote="", check.names=F)
  return(file)
}
pbsapply_read_tsv <- function(path){
  data <- pbapply::pbsapply(path, read_tsv, simplify = F)
  return(data)
}
write_tsv <-
  function(x, filename, col.names = T, row.names = F){
    write.table(x, file = filename, sep = "\t",
                col.names = col.names, row.names = row.names, quote = F)
  }
## ------------------------------------- 
#' @importFrom grid unit
.element_textbox <- 
  function(family = NULL, face = NULL, size = NULL,
           colour = "white", fill = "lightblue",
           box.colour = "white", linetype = 1, linewidth = NULL,
           hjust = NULL, vjust = NULL,
           halign = 0.5, valign = NULL, lineheight = NULL,
           margin = match.fun("margin")(3, 3, 3, 3),
           padding = match.fun("margin")(2, 0, 1, 0),
           width = grid::unit(1, "npc"),
           height = NULL, minwidth = NULL,
           maxwidth = NULL, minheight = NULL, maxheight = NULL,
           r = grid::unit(5, "pt"), orientation = NULL,
           debug = FALSE, inherit.blank = FALSE
           ){
    structure(as.list(environment()),
              class = c("element_textbox", "element_text", "element"))
  }
## ------------------------------------- 
.get_legend <- 
  function(p){
    p <- ggplot2:::ggplot_build.ggplot(p)$plot
    theme <- ggplot2:::plot_theme(p)
    position <- theme$legend.position
    ggplot2:::build_guides(p$scales, p$layers, p$mapping,
                           position, theme, p$guides, p$labels)
  }
