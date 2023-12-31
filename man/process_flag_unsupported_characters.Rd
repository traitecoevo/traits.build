% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process.R
\name{process_flag_unsupported_characters}
\alias{process_flag_unsupported_characters}
\title{Flag values with unsupported characters}
\usage{
process_flag_unsupported_characters(data)
}
\arguments{
\item{data}{Tibble or dataframe containing the study data}
}
\value{
Tibble with flagged values containing unsupported characters
}
\description{
Disallowed characters are flagged as errors, including for numeric traits, prior to
unit conversions to avoid their conversion to NAs during the unit conversion process.
}
