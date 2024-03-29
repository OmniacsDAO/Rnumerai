% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/baseapi.R
\name{diagnostics}
\alias{diagnostics}
\title{Fetch results of diagnostics run}
\usage{
diagnostics(model_id, tournament = 8, diagnostics_id)
}
\arguments{
\item{model_id}{Target model UUID}

\item{tournament}{Tournament ID, 8 for Main, 11 for Signal}

\item{diagnostics_id}{id returned by "upload_diagnostics"}
}
\value{
Diagnostic results
}
\description{
Fetch results of diagnostics run
}
\examples{
\dontrun{
diagnostics(model_id = get_models()[["bayo"]],tournament=8, diagnostics_id = "")
}
}
