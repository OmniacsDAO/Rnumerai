% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/baseapi.R
\name{set_bio}
\alias{set_bio}
\title{Set bio field for a model id}
\usage{
set_bio(model_id, bio)
}
\arguments{
\item{model_id}{Target model UUID}

\item{bio}{Bio to change}
}
\value{
If the bio was changed successfully
}
\description{
Set bio field for a model id
}
\examples{
\dontrun{
set_bio(model_id = get_models()[["bayo"]], bio = "This Model Rocks")
}
}
