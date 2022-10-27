# ==========================================================================
# a class to store the filtered dataset from 'project_dataset'
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportClass mcn_dataset
#'
#' @aliases mcn_dataset
#'
#' @title ...
#'
#' @description ...
#'
# @family mcn_datasets
#'
# @seealso \code{\link{<class>-class}}
#'
#' @slot dataset ...
#' @slot reference ...
#' @slot backtrack ...
#'
#' @rdname mcn_dataset-class
#'
#' @examples
#' \dontrun{
#' new('mcn_dataset', ...)
#' }
.mcn_dataset <- 
  setClass("mcn_dataset", 
           contains = c("dataset", "reference", "backtrack"),
           prototype = NULL
           )
# ==========================================================================
# method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportMethod mcn_dataset
#' @aliases mcn_dataset
#' @description \code{mcn_dataset}, \code{mcn_dataset<-}: getter and setter
#' for the \code{mcn_dataset} slot of the object.
#' @rdname mcn_dataset-class
setMethod("mcn_dataset", 
          signature = c(x = "ANY"),
          function(x){ x@mcn_dataset })
#' @exportMethod mcn_dataset<-
#' @aliases mcn_dataset<-
#' @param value The value for the slot.
#' @rdname mcn_dataset-class
setReplaceMethod("mcn_dataset", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, mcn_dataset = value)
                 })
## ------------------------------------- 
#' @exportMethod latest
#' @aliases latest
#' @description \code{latest}: get the first data in \code{dataset} slot and
#' format as "tbl".
#' @family datasets
#' @family latests
# @seealso [fun()]
#' @rdname mcn_dataset-class
#' @examples
#' \dontrun{
#' latest(...)
#' }
setMethod("latest", 
          signature = c(x = "mcn_dataset"),
          function(x){
            tibble::as_tibble(entity(dataset(x)[[1]]))
          })
## ------------------------------------- 
#' @exportMethod extract_mcnset
#' @aliases extract_mcnset
#' @description \code{extract_mcnset}: For fast extract data in object which containing
#' \code{mcn_dataset} slot. Normally not used.
#' @param x ...
#' @param subscript ...
# @family ...s
# @seealso [fun()]
#' @rdname mcn_dataset-class
#' @examples
#' \dontrun{
#' extract_mcnset(...)
#' }
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

