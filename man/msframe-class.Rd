% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-msframe.R
\docType{class}
\name{msframe-class}
\alias{msframe-class}
\alias{.msframe}
\alias{msframe}
\alias{show,msframe-method}
\alias{show}
\alias{msframe,ANY-method}
\alias{msframe<-,ANY-method}
\alias{msframe<-}
\alias{latest,msframe,ANY,ANY-method}
\alias{latest}
\alias{entity,msframe-method}
\alias{entity}
\alias{entity<-,msframe-method}
\alias{entity<-}
\alias{format_msframe,msframe,missing,missing,missing,missing,function-method}
\alias{format_msframe}
\alias{format_msframe,data.frame,missing,missing,missing,missing,function-method}
\alias{format_msframe,msframe,character,missing,character,missing,missing-method}
\alias{format_msframe,msframe,missing,missing,missing,missing,missing-method}
\alias{format_msframe,msframe,missing,function,missing,function,missing-method}
\alias{filter_msframe,msframe,function,missing-method}
\alias{filter_msframe}
\alias{filter_msframe,msframe,function,formula-method}
\title{format and filter table data}
\usage{
\S4method{show}{msframe}(object)

\S4method{msframe}{ANY}(x)

\S4method{msframe}{ANY}(x) <- value

\S4method{latest}{msframe,ANY,ANY}(x)

\S4method{entity}{msframe}(x)

\S4method{entity}{msframe}(x) <- value

\S4method{format_msframe}{msframe,missing,missing,missing,missing,function}(x, fun_format)

\S4method{format_msframe}{data.frame,missing,missing,missing,missing,function}(x, fun_format)

\S4method{format_msframe}{msframe,character,missing,character,missing,missing}(x, names, types)

\S4method{format_msframe}{msframe,missing,missing,missing,missing,missing}(x)

\S4method{format_msframe}{msframe,missing,function,missing,function,missing}(x, fun_names, fun_types)

\S4method{filter_msframe}{msframe,function,missing}(x, fun_filter, f, ...)

\S4method{filter_msframe}{msframe,function,formula}(x, fun_filter, f, ...)
}
\arguments{
\item{x}{\linkS4class{msframe} object.}

\item{value}{The value for the slot.}

\item{fun_format}{function to format slot \code{entity}.
e.g., \code{MCnebula2:::.format_msframe()}}

\item{names}{character with names.
e.g., c(tani.score = "tanimotoSimilarity", mol.formula = "molecularFormula").}

\item{types}{character with names.
e.g., c(tani.score = "numeric", mol.formula = "character").}

\item{fun_names}{function to get names.
e.g., \code{MCnebula2:::.get_attribute_name_sirius.v4()}}

\item{fun_types}{function to get types.
e.g., \code{MCnebula2:::.get_attribute_type_sirius.v4()}}

\item{fun_filter}{function used to filter the slot \code{entity} (data.frame).
e.g., \code{dplyr::filter()}, \code{head()}.}

\item{f}{formula passed to \code{split()}.}

\item{...}{extra parameter passed to fun_filter.}
}
\description{
Class for table data manipulation inside this package.

\code{msframe}, \code{msframe<-}: getter and setter
for the \code{msframe} slot of the object.

\code{latest}: get data inside \code{entity(object)} and format as
'tbl'.

\code{entity}, \code{entity<-}: getter and setter
for the \code{entity} slot of the object.

\code{format_msframe}:

\code{filter_msframe}: filter data in slot \code{entity} (data.frame).
}
\section{Slots}{

\describe{
\item{\code{entity}}{data.frame.}

\item{\code{subscript}}{character(1). See \linkS4class{subscript}.}
}}

\note{
The class is not for normal use of the package.
}
\seealso{
\code{\link[tibble:as_tibble]{tibble::as_tibble()}}

Other subscripts: 
\code{\link{mcnebula-class}},
\code{\link{project_conformation-class}},
\code{\link{subscript-class}}

Other latests: 
\code{\link{mcn_dataset-class}},
\code{\link{mcnebula-class}},
\code{\link{project_dataset-class}},
\code{\link{project_metadata-class}}
}
\concept{latests}
\concept{subscripts}
