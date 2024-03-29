% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{.updateOrder}
\alias{.updateOrder}
\title{Update column order}
\usage{
.updateOrder(input = NULL)
}
\arguments{
\item{input}{\link[=data.frame]{data.frame(1)}\cr a table that contains at least the
columns \code{fid} and \code{gid}.}
}
\value{
A new table where the columns have the correct order.
}
\description{
Set the order of the table columns to \code{c("fid", "gid", rest)}
}
