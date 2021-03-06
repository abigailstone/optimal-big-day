% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_find_hotspots.R
\name{get_first_best}
\alias{get_first_best}
\title{Select the location with the highest probability sum}
\usage{
get_first_best(probs)
}
\arguments{
\item{probs}{A tibble with the probability of observing each species at each locality}
}
\value{
The name of the location with the highest expected number of species
}
\description{
Select the location with the highest probability sum
}
\examples{
get_first_best(drop_effort_cols(sample_prob_per_loc))
}
