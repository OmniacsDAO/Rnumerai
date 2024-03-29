% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/baseapi.R
\name{round_model_performances}
\alias{round_model_performances}
\title{Fetch round model performance of any user}
\usage{
round_model_performances(username, tournament = 8)
}
\arguments{
\item{username}{User Name}

\item{tournament}{Tournament ID, 8 for Main, 11 for Signal}
}
\value{
list of round model performance entries
}
\description{
Fetch round model performance of any user
}
\examples{
\dontrun{
round_model_performances(username = "bayo")
}
}
