% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/notetaker.R
\name{notetaker_as_note}
\alias{notetaker_as_note}
\title{Create a tibble with two columns with note and link}
\usage{
notetaker_as_note(note, link = NA_character_)
}
\arguments{
\item{note}{character string}

\item{link}{character string, default is NA_character_ which generates a random string}
}
\value{
a tibble with two columns named note and link
}
\description{
Creates a tibble with two columns with one column consisting
of a randomly generated string of letters
}
