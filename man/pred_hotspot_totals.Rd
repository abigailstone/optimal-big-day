% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fct_find_hotspots.R
\name{pred_hotspot_totals}
\alias{pred_hotspot_totals}
\title{Return the expected number of species at each of the selected hotspots}
\usage{
pred_hotspot_totals(hotspots, probs)
}
\arguments{
\item{hotspots}{a character vector of hotspots of interest}

\item{probs}{A tibble with the probability of observing each species at each locality}
}
\value{
A numerical vector with the expected number of species at each hotspot
}
\description{
Return the expected number of species at each of the selected hotspots
}
\examples{
hotspots <- c('Otter View Park', 'Button Bay State Park')
pred_hotspot_totals(hotspots, drop_effort_cols(sample_prob_per_loc))
}
