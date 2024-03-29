% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/disambr_messaging.r
\name{disambr_message_finish}
\alias{disambr_message_finish}
\title{Post a starting message for disambr procedure. Records time started in \code{disambr_start_time} variable in its \code{parent.frame()}}
\usage{
disambr_message_finish(
  mess = "Finished -",
  append_running_procedure_name = TRUE,
  ...
)
}
\arguments{
\item{mess}{Prefix for staring message}

\item{append_running_procedure_name}{Whether to append running procedure name}

\item{...}{
  Arguments passed on to \code{\link[=disambr_message]{disambr_message}}
  \describe{
    \item{\code{h_prefix}}{Character vector of prefixes for each outline level}
  }}
}
\value{
time started
}
\description{
Post a starting message for disambr procedure. Records time started in \code{disambr_start_time} variable in its \code{parent.frame()}
}
