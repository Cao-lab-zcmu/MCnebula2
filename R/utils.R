# ==========================================================================
# additional function
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
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
           })
  }
## ------------------------------------- 
.get_info <- 
  function(main, sub, arg = NULL, sig = "##"){
    cat(sig, " ", main, ": ", sub, ": ", arg, "\n", sep = "")
  }
## ------------------------------------- 
.list_files <- function(path, upper){
  lst_file <- pbapply::pbsapply(paste0(path, "/", upper), list.files)
  lst_file <- mapply(upper, lst_file, SIMPLIFY = F, FUN = function(upper, files){
                       if (length(files) == 0)
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
write_tsv <-
  function(x, filename, col.names = T, row.names = F){
    write.table(x, file = filename, sep = "\t",
                col.names = col.names, row.names = row.names, quote = F)
  }
