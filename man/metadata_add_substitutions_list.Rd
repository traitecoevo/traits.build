% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup.R
\name{metadata_add_substitutions_list}
\alias{metadata_add_substitutions_list}
\title{Add a dataframe of trait value substitutions into a metadata file for a dataset_id}
\usage{
metadata_add_substitutions_list(dataset_id, substitutions)
}
\arguments{
\item{dataset_id}{Identifier for a particular study in the database}

\item{substitutions}{Dataframe of trait value substitutions}
}
\value{
`metadata.yml` file with multiple trait value substitutions added
}
\description{
Add a dataframe of trait value substitutions into a metadata file for a dataset_id
}
