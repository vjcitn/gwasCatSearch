% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/query.R
\name{EFOparents}
\alias{EFOparents}
\title{A function to query the efo_edges table and return the parent terms for the input EFO IDs}
\usage{
EFOparents(EFOID)
}
\arguments{
\item{EFOID}{a character vector of the EFO CURIE symbols}
}
\value{
A named vector, names are from the input EFOIDs and the values are the
corresponding CURIEs for the parent terms. There can be zero or more matches for each input ID.
}
\description{
This function provides an interface to the SQL database containing EFO edges (parent child relationships).
}
\details{
The function returns the set of parents for the input terms.
}
\examples{
EFOparents(c("EFO:0000768", "MONDO:0002429"))
}
\author{
Robert Gentleman
}
