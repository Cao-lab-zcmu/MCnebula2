% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions-report.R
\name{gather_sections}
\alias{gather_sections}
\title{Quickly gather all the 'sections' in environment}
\usage{
gather_sections(prefix = "s", envir = parent.frame(), sort = T, get = T)
}
\arguments{
\item{prefix}{character(1). The character prefix of the variable name.}

\item{envir}{environment. The environment to get the variables.}

\item{sort}{logical(1). If \code{TRUE}, sort the variable names
according to the first number string that accompanies the prefix.}

\item{get}{logical(1). If \code{TRUE}, return with a list of the value of
the variables. If \code{FALSE}, return with the variable names.}
}
\description{
\code{gather_sections}:
Gathers all eligible variable names in an environment by means of Regex
matches.  These variables must: have a uniform character prefix, and the first
character that follows must be a number. e.g., "s1", "s2", "s12.2",
"s15.5.figure"...
}
