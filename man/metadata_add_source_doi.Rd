% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup.R
\name{metadata_add_source_doi}
\alias{metadata_add_source_doi}
\title{Adds citation details from a doi to a metadata file for a \code{dataset_id}}
\usage{
metadata_add_source_doi(..., doi, bib = NULL)
}
\arguments{
\item{...}{Arguments passed from metadata_add_source_bibtex()}

\item{doi}{doi of reference to add}

\item{bib}{(Only use for testing purposes) Result of calling \verb{bib rcrossref::cr_cn(doi)}}
}
\value{
\code{metadata.yml} file with citation details added
}
\description{
Uses rcrossref package to access publication details from the crossref
database
}
