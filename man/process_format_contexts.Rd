% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process.R
\name{process_format_contexts}
\alias{process_format_contexts}
\title{Format context data from list to tibble}
\usage{
process_format_contexts(my_list, dataset_id, traits)
}
\arguments{
\item{my_list}{List of input information}

\item{dataset_id}{Identifier for a particular study in the AusTraits database}

\item{traits}{Table of trait data (for this function, just the data.csv file with custom_R_code applied)}
}
\value{
Tibble with context details if available
}
\description{
Format context data read in from the `metadata.yml` file. Converts from list to tibble.
}
\examples{
\dontrun{
process_format_contexts(read_metadata("data/Apgaua_2017/metadata.yml")$context, dataset_id, traits)
}
}
