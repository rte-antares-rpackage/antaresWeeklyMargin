% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_wm_load_fr.R
\name{create-wm-load-fr}
\alias{create-wm-load-fr}
\alias{create_wm_load_fr}
\alias{offset_opts}
\title{Create Load TS for France}
\usage{
create_wm_load_fr(path, start, start_prev_hebdo, type = c("prevu",
  "premis", "offset"), offset_options = NULL,
  opts = antaresRead::simOptions())

offset_opts(peak_morning = c(NA, NA, "13:00", "13:00", "13:00", "13:00",
  "13:00"), peak_evening = c(NA, NA, "19:00", "19:00", "19:00", "19:00",
  "23:00"), offpeak_night = c(NA, NA, "04:00", "04:00", "04:00", "04:00",
  "04:00"), offpeak_day = c(NA, NA, "22:00", "22:00", "22:00", "22:00",
  "22:00"))
}
\arguments{
\item{path}{Path to CNES directory, see \code{\link{read_cnes}} for usage.}

\item{start}{Beginning of the simulation.}

\item{start_prev_hebdo}{Beginning of previsions.}

\item{type}{Forecast to use \code{"prevu"}, \code{"premis"} or \code{"offset"}.}

\item{offset_options}{DEPRECATED. Peak and off-peak if \code{type = "offset"}.}

\item{opts}{List of simulation parameters returned by the function
\code{antaresRead::setSimulationPath}}

\item{peak_morning}{Morning peak.}

\item{peak_evening}{Evening peak.}

\item{offpeak_night}{Night off-peak.}

\item{offpeak_day}{Day off-peak.}
}
\description{
Create Load TS for France
}
\examples{
\dontrun{

create_wm_load_fr("Prev_hebdo_CNES/", "2018-02-27", "prevu")

}
}
