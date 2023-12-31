% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{util_append_to_list}
\alias{util_append_to_list}
\title{Add an item to the end of a list}
\usage{
util_append_to_list(my_list, to_append)
}
\arguments{
\item{my_list}{A list}

\item{to_append}{A list}
}
\value{
A list merged with an added item at the end
}
\description{
Add an item to the end of a list
}
\examples{
 \dontrun{
util_append_to_list(as.list(dplyr::starwars)[c(1,2)], as.list(dplyr::starwars)[c(3,4)])
}
}
