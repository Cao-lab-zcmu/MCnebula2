# ==========================================================================
# class-mcn_dataset
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.mcn_dataset <- 
  setClass("mcn_dataset", 
           contains = c("dataset", "reference", "backtrack"),
           prototype = NULL
           )
# ==========================================================================
# method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
setMethod("mcn_dataset", 
          signature = c(x = "ANY"),
          function(x){ x@mcn_dataset })
setReplaceMethod("mcn_dataset", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, mcn_dataset = value)
                 })
## ------------------------------------- 
setMethod("latest", 
          signature = c(x = "mcn_dataset"),
          function(x){
            tibble::as_tibble(entity(dataset(x)[[1]]))
          })
## ------------------------------------- 
setMethod("extract_mcnset", 
          signature = c(x = "ANY", subscript = "character"),
          function(x, subscript){
            if ( any( subscript == names(dataset(mcn_dataset(x))) ) )
              msframe <- dataset(mcn_dataset(x))[[ subscript ]]
            else
              stop("`subscript` not found in `dataset(mcn_dataset(x))`")
            lst <- list(msframe)
            names(lst) <- subscript
            return(lst)
          })
## ------------------------------------- 

