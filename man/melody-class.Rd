% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/class-melody.R
\docType{class}
\name{melody-class}
\alias{melody-class}
\alias{.melody}
\alias{melody}
\alias{show,melody-method}
\alias{show}
\alias{melody,ANY-method}
\alias{melody<-,ANY-method}
\alias{melody<-}
\alias{palette_set,melody-method}
\alias{palette_set}
\alias{palette_set<-,melody-method}
\alias{palette_set<-}
\alias{palette_gradient,melody-method}
\alias{palette_gradient}
\alias{palette_gradient<-,melody-method}
\alias{palette_gradient<-}
\alias{palette_stat,melody-method}
\alias{palette_stat}
\alias{palette_stat<-,melody-method}
\alias{palette_stat<-}
\alias{palette_col,melody-method}
\alias{palette_col}
\alias{palette_col<-,melody-method}
\alias{palette_col<-}
\alias{palette_label,melody-method}
\alias{palette_label}
\alias{palette_label<-,melody-method}
\alias{palette_label<-}
\title{Mutiple color palette in hexadecimal code}
\usage{
\S4method{show}{melody}(object)

\S4method{melody}{ANY}(x)

\S4method{melody}{ANY}(x) <- value

\S4method{palette_set}{melody}(x)

\S4method{palette_set}{melody}(x) <- value

\S4method{palette_gradient}{melody}(x)

\S4method{palette_gradient}{melody}(x) <- value

\S4method{palette_stat}{melody}(x)

\S4method{palette_stat}{melody}(x) <- value

\S4method{palette_col}{melody}(x)

\S4method{palette_col}{melody}(x) <- value

\S4method{palette_label}{melody}(x)

\S4method{palette_label}{melody}(x) <- value
}
\arguments{
\item{value}{The value for the slot.}
}
\description{
This is a class object store Hex color used for visualization.
In default (use \code{\link[=initialize_mcnebula]{initialize_mcnebula()}} to initialize the object),
these these Hex color in each palette were get from package \code{ggsci}.
Most of these palette in this package would passed to \link[ggplot2:scale_manual]{ggplot2::scale_fill_manual} for
filling color. So, let these Hex color with names may work well to specify target.

\code{melody}, \code{melody<-}: getter and setter
for the \code{melody} slot of the object.

\code{palette_set}, \code{palette_set<-}: getter and setter
for the \code{palette_set} slot of the object.

\code{palette_gradient}, \code{palette_gradient<-}: getter and setter
for the \code{palette_gradient} slot of the object.

\code{palette_stat}, \code{palette_stat<-}: getter and setter
for the \code{palette_stat} slot of the object.

\code{palette_col}, \code{palette_col<-}: getter and setter
for the \code{palette_col} slot of the object.

\code{palette_label}, \code{palette_label<-}: getter and setter
for the \code{palette_label} slot of the object.
}
\section{Slots}{

\describe{
\item{\code{palette_set}}{character with names or not. Hex color.}

\item{\code{palette_gradient}}{character with names or not. Hex color.}

\item{\code{palette_stat}}{character with names or not. Hex color.}

\item{\code{palette_col}}{character with names or not. Hex color.}

\item{\code{palette_label}}{character with names or not. Hex color.}
}}

\seealso{
\code{\link[ggsci:pal_simpsons]{ggsci::pal_simpsons()}}, \code{\link[ggsci:pal_igv]{ggsci::pal_igv()}}, \code{\link[ggsci:pal_ucscgb]{ggsci::pal_ucscgb()}},
\code{\link[ggsci:pal_d3]{ggsci::pal_d3()}}...
}
