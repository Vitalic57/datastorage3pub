% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fin_instr_fun.R, R/generics.R
\name{stock.Data}
\alias{stock.Data}
\alias{stock}
\title{Adds stock to this}
\usage{
\method{stock}{Data}(this, ...)

stock(this, ...)
}
\arguments{
\item{this}{Data}

\item{...}{character vector of stocks}

\item{src}{character scalar, source for normal download}

\item{online_src}{character scalar, source for updating last point, if it is omitted then update_src will be used.}

\item{update_src}{character scalar, source for updating series to current date}
}
\value{
Data object

Data object
}
\description{
Adds stock to this
}
