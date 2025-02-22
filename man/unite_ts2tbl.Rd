% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unite_ts2tibble.R
\name{unite_ts2tbl}
\alias{unite_ts2tbl}
\title{Unite ts objects, and convert them to a tibble object}
\usage{
unite_ts2tbl(...)
}
\arguments{
\item{...}{dot-dot-dot}
}
\description{
Unite ts objects, and convert them to a tibble object
}
\examples{
\dontrun{
ts1 <- ts(1:12, freq=4, start=c(2020,1))
ts2 <- ts(12:1, freq=4, start=c(2020,1))
ts3 <- ts(13:24, freq=4, start=c(2020,1))

unite_ts2tbl(ts1, ts2, ts3)
}
}
