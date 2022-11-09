# ==========================================================================
# a class for statistic analysis
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportClass statistic_set
#'
#' @aliases statistic_set
#'
#' @title Data used for statistic analysis
#'
#' @description A class object for statistic analysis, associate with package of "limma"
#' for binary comparison.
#'
#' @slot design_matrix matrix. Create by [stats::model.matrix()].
#' @slot contrast_matrix matrix. Create by [limma::makeContrasts()].
#' @slot dataset ANY. Dataset used for [limma::lmFit()], [limma::eBayes()]
#' and other functions.
#' @slot top_table list with names. Each element of list should be "data.frame" or "tbl".
#'
#' @rdname statistic_set-class
#'
#' @examples
#' \dontrun{
#' new('statistic_set', ...)
#' }
.statistic_set <- 
  setClass("statistic_set", 
           contains = character(),
           representation = 
             representation(design_matrix = "matrix",
                            contrast_matrix = "matrix",
                            dataset = "ANY",
                            top_table = "list"
                            ),
           prototype = NULL
           )

# ==========================================================================
# method
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#' @exportMethod statistic_set
#' @aliases statistic_set
#' @description \code{statistic_set}, \code{statistic_set<-}: getter and setter
#' for the \code{statistic_set} slot of the object.
#' @rdname statistic_set-class
setMethod("statistic_set", 
          signature = c(x = "ANY"),
          function(x){ x@statistic_set })

#' @exportMethod statistic_set<-
#' @aliases statistic_set<-
#' @param value The value for the slot.
#' @rdname statistic_set-class
setReplaceMethod("statistic_set", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, statistic_set = value)
                 })

## ------------------------------------- 
#' @exportMethod design_matrix
#' @aliases design_matrix
#' @description \code{design_matrix}, \code{design_matrix<-}: getter and setter
#' for the \code{design_matrix} slot of the object.
#' @rdname statistic_set-class
setMethod("design_matrix", 
          signature = c(x = "ANY"),
          function(x){ x@design_matrix })

#' @exportMethod design_matrix<-
#' @aliases design_matrix<-
#' @param value The value for the slot.
#' @rdname statistic_set-class
setReplaceMethod("design_matrix", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, design_matrix = value)
                 })

## ------------------------------------- 
#' @exportMethod contrast_matrix
#' @aliases contrast_matrix
#' @description \code{contrast_matrix}, \code{contrast_matrix<-}: getter and setter
#' for the \code{contrast_matrix} slot of the object.
#' @rdname statistic_set-class
setMethod("contrast_matrix", 
          signature = c(x = "ANY"),
          function(x){ x@contrast_matrix })

#' @exportMethod contrast_matrix<-
#' @aliases contrast_matrix<-
#' @param value The value for the slot.
#' @rdname statistic_set-class
setReplaceMethod("contrast_matrix", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, contrast_matrix = value)
                 })

## ------------------------------------- 
#' @exportMethod top_table
#' @aliases top_table
#' @description \code{top_table}, \code{top_table<-}: getter and setter
#' for the \code{top_table} slot of the object.
#' @rdname statistic_set-class
setMethod("top_table", 
          signature = c(x = "ANY"),
          function(x){ x@top_table })

#' @exportMethod top_table<-
#' @aliases top_table<-
#' @param value The value for the slot.
#' @rdname statistic_set-class
setReplaceMethod("top_table", 
                 signature = c(x = "ANY"),
                 function(x, value){
                   initialize(x, top_table = value)
                 })
