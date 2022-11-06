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
reCallMethod <- 
  function(funName, args, ...){
    arg.order <- unname(getGeneric(funName)@signature)
    args.missing <- !arg.order %in% names(args)
    if (any(args.missing)) {
      args.missing <- arg.order[args.missing]
      args.missing <- sapply(args.missing, simplify = F,
                             function(x) structure(0L, class = "missing"))
      args <- c(args, args.missing)
    }
    args <- lapply(arg.order, function(i) args[[i]])
    sig <- get_signature(args)
    method <- selectMethod(funName, sig)
    last_fun <- sys.function(sys.parent())
    n <- 0
    while (identical(last_fun, method@.Data, ignore.environment = T)) {
      if (n == 0) {
        mlist <- getMethodsForDispatch(getGeneric(funName))
      }
      n <- n + 1
      rm(list = paste0(method@defined, collapse = "#"), envir = mlist)
      method <- selectMethod(funName, sig, mlist = mlist)
    }
    expr <- paste0("method@.Data(",
                   paste0(paste0(arg.order, " = args[[",
                                 1:length(arg.order), "]]"),
                          collapse = ", "),
                   ", ...)")
    eval(parse(text = expr))
  }
get_signature <- 
  function(args){
    vapply(args, function(arg) class(arg)[1], FUN.VALUE = "ch")
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
# # ------------------------------------- 
.message_info <- 
  function(main, sub, arg = NULL, sig = "##"){
    message(sig, " ", main, ": ", sub, " ", arg)
  }
.message_info_formal <- 
  function(main, sub, arg = NULL, sig = "[INFO]"){
    message(sig, " ", main, ": ", sub, " ", arg)
  }
#' @importFrom grid current.viewport
.message_info_viewport <- 
  function(info = "info"){
    .message_info(info, "current.viewport:",
                  paste0("\n\t", paste0(grid::current.viewport())))
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
#' @importFrom rlang as_label
.check_data <- 
  function(object, lst, tip = "(...)"){
    target <- rlang::as_label(substitute(object))
    mapply(lst, names(lst), FUN = function(value, name){
             obj <- match.fun(name)(object)
             if (is.null(obj)) {
               stop(paste0("is.null(", name, "(", target, ")) == T. ",
                           "use `", value, tip, "` previously."))
             }
             if (is.list(obj)) {
               if (length(obj) == 0) {
                 stop(paste0("length(", name, "(", target, ")) == 0. ",
                             "use `", value, tip, "` previously."))
               }
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
#' @importFrom rlang as_label
.check_class <- 
  function(object, class = "layout", tip = "grid::grid.layout"){
    if (!is(object, class)) {
      stop(paste0("`", rlang::as_label(substitute(object)),
                  "` should be a '", class, "' object created by ",
                  "`", tip, "`." ))
    }
  }
.check_columns <- 
  function(obj, lst, tip){
    if (!is.data.frame(obj))
      stop(paste0("'", tip, "' must be a 'data.frame'."))
    lapply(lst, function(col){
             if (is.null(obj[[ col ]]))
               stop(paste0("'", tip, "' must contains a column of '", col, "'."))
           })
  }
.check_type <- 
  function(obj, type, tip){
    fun <- match.fun(paste0("is.", type))
    apply(obj, 2, function(col){
            if (!fun(col))
              stop(paste0("data columns in '", tip, "' must all be '", type, "'."))
           })
  }
.check_path <- 
  function(path){
    if (!file.exists(path)) {
      dir.create(path, recursive = T)
    }
  }
.check_file <- 
  function(file){
    if (!file.exists(file)) {
      stop("file.exists(file) == F, `file` not exists.")
    }
  }
validate_class_in_list <- 
  function(lst, recepts, tip){
    check <- 
      lapply(lst, function(layer) {
               check <- lapply(recepts, function(class) {
                                 if (is(layer, class)) T })
               if (any(unlist(check))) T else F
           })
    if (any(!unlist(check)))
      stop(tip)
    else T
  }
.suggest_bio_package <- 
  function(pkg){
    if (!requireNamespace(pkg, quietly = T))
      stop("package '", pkg, "' not installed. use folloing to install:\n",
           '\nif (!require("BiocManager", quietly = TRUE))',
           '\n\tinstall.packages("BiocManager")',
           '\nBiocManager::install("', pkg, '")\n\n')
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
#' @importFrom ggtext element_textbox
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
.depigment_col <- 
  function(col, n = 10, level = 5){
    colorRampPalette(c("white", col))(n)[level]
  }
## ------------------------------------- 
.simulate_quant_set <- 
  function(x){
    quant <- .simulate_quant(features_annotation(x)$.features_id)
    meta <- group_strings(colnames(quant),
                          c(control = "^control", model = "^model",
                            treat = "^treat", pos = "^pos"), "sample")
    features_quantification(x) <- quant
    sample_metadata(x) <- meta
    return(x)
  }
#' @importFrom tibble as_tibble
.simulate_quant <- 
  function(.features_id, mean = 50, sd = 20, seed = 555,
           group = c("control", "model", "treat", "pos"), rep = 5){
    quant <- data.frame(.features_id = .features_id)
    set.seed(seed)
    lst <- lapply(1:(length(group) * rep), function(x){
                    rnorm(nrow(quant), mean, sd)
           })
    df <- apply(do.call(data.frame, lst), 2, abs)
    df <- df[, hclust(dist(t(df)))$order]
    colnames(df) <- unlist(lapply(group, paste0, "_", 1:rep))
    tibble::as_tibble(cbind(quant, df))
  }
group_strings <- 
  function(strings, patterns, target = NA){
    if (is.null(names(patterns)))
      stop("`patterns` must be characters with names.")
    lst <- .find_and_sort_strings(strings, patterns)
    lst <- lapply(names(lst), function(name){
                    data.frame(target = lst[[name]], group = name)
           })
    df <- do.call(rbind, lst)
    if (!is.na(target)) {
      colnames(df)[1] <- target
    }
    tibble::as_tibble(df)
  }
.find_and_sort_strings <- 
  function(strings, patterns){
    lapply(patterns,
           function(pattern){
             strings[grepl(pattern, strings, perl = T)]
           })
  }
.as_dic <- 
  function(vec, names, default,
           fill = T, as.list = T, na.rm = F){
    if (is.null(names(vec)))
      names(vec) <- names[1:length(vec)]
    if (fill) {
      if (any(!names %in% names(vec))) {
        ex.names <- names[!names %in% names(vec)]
        ex <- rep(default, length(ex.names))
        names(ex) <- ex.names
        vec <- c(vec, ex)
      }
    }
    if (as.list) {
      if (!is.list(vec))
        vec <- as.list(vec)
    }
    if (na.rm) {
      vec <- vec[!is.na(names(vec))]
    }
    vec
  }
.fresh_param <- 
  function(default, args){
    if (missing(args))
      args <- as.list(parent.frame())
    args <- args[ !vapply(args, is.name, T) ]
    sapply(unique(c(names(default), names(args))),
           simplify = F,
           function(name){
             if (any(name == names(args)))
               args[[ name ]]
             else
               default[[ name ]]
           })
  }
## ---------------------------------------------------------------------- 
#' @importFrom grImport2 readPicture
#' @importFrom grImport2 grobify
.cairosvg_to_grob <- 
  function(path){
    grImport2::grobify(grImport2::readPicture(path))
  }
#' @importFrom ChemmineOB convertToImage
#' @importFrom rsvg rsvg_svg
.smiles_to_cairosvg <- 
  function(smile, path){
    ChemmineOB::convertToImage("SMI", "SVG", source = smile, toFile = path)
    rsvg::rsvg_svg(path, path)
  }
