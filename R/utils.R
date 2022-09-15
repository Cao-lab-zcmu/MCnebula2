# ==========================================================================
# additional function
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMissing <- 
  function(generic, ...){
    args <- list(...)
    sig <- getGeneric(generic)@signature
    res <- vapply(sig, FUN.VALUE = "character",
                  function(name){
                    if (is.null(args[[ name ]]))
                      "missing"
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
    res <- mapply(fun, slots = slots, names = names(slots), ...)
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
.get_info <- 
  function(main, sub, arg = NULL, sig = "##"){
    cat(sig, " ", main, ": ", sub, " ", arg, "\n", sep = "")
  }
.get_info_formal <- 
  function(main, sub, arg = NULL, sig = "[INFO]"){
    cat(sig, " ", main, ": ", sub, " ", arg, "\n", sep = "")
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
`%>%` <- magrittr::`%>%`
