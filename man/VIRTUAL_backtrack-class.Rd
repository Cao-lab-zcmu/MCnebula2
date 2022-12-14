% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-VIRTUAL_slots.R
\docType{class}
\name{backtrack-class}
\alias{backtrack-class}
\alias{VIRTUAL_backtrack}
\alias{backtrack}
\alias{backtrack,ANY-method}
\alias{backtrack<-,ANY-method}
\alias{backtrack<-}
\title{Share slots and methods for classes inherite from VIRTUAL_backtrack}
\usage{
\S4method{backtrack}{ANY}(x)

\S4method{backtrack}{ANY}(x) <- value
}
\arguments{
\item{x}{object inherit class \code{backtrack}.}

\item{value}{The value for the slot.}
}
\description{
This VIRTUAL class provides a slot for storing discarded data.

\code{backtrack}, \code{backtrack<-}: getter and setter
for the \code{backtrack} slot of the object.
}
\section{Slots}{

\describe{
\item{\code{backtrack}}{list with names.}
}}

\concept{backtracks}
