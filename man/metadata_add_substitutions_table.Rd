% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup.R
\name{metadata_add_substitutions_table}
\alias{metadata_add_substitutions_table}
\title{Substitutions from a dataframe}
\usage{
metadata_add_substitutions_table(
  dataframe_of_substitutions,
  dataset_id,
  trait_name,
  find,
  replace
)
}
\arguments{
\item{dataframe_of_substitutions}{Dataframe with columns indicating \code{dataset_id}, \code{trait_name},
original trait values (\code{find}), and database aligned trait value (\code{replace})}

\item{dataset_id}{Name of column containing study \code{dataset_id}(s) in database}

\item{trait_name}{Name of column containing trait name(s) for which a trait value replacement needs to be made}

\item{find}{Name of column containing trait values submitted by the contributor for a data observation}

\item{replace}{Name of column containing database aligned trait values}
}
\value{
Modified metadata files with trait value replacements
}
\description{
Function that simultaneously adds many trait value replacements, potentially
across many \code{trait_name}'s and \code{dataset_id}'s, to the respective \code{metadata.yml} files.
This function will be used to quickly re-align/re-assign trait values across all studies.
}
\examples{
\dontrun{
read_csv("export/dispersal_syndrome_substitutions.csv") \%>\%
  select(-extra) \%>\%
  filter(dataset_id == "Angevin_2011") -> dataframe_of_substitutions
metadata_add_substitutions_table(dataframe_of_substitutions, dataset_id, trait_name, find, replace)
}
}
