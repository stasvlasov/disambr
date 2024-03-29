% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/disambr_sets.r
\name{disambr_get_strong_set}
\alias{disambr_get_strong_set}
\title{Get sets with strength parameter of 1 and rbind them into single set}
\usage{
disambr_get_strong_set(sets, ...)
}
\arguments{
\item{sets}{sets}

\item{...}{
  Arguments passed on to \code{\link[=disambr_in_sets]{disambr_in_sets}}
  \describe{
    \item{\code{match_attr_value_parcially}}{whether attribute values can be matched partially}
    \item{\code{check_attr_names_prefix}}{Whether to check for short names of attributes. See \code{attr_names_prefix}.}
    \item{\code{attr_names_prefix}}{If name does not start with this prefix (default is 'disambr_set_'), it will add this prefix before attribute name.}
  }}
}
\value{
set or NULL if none found
}
\description{
Get sets with strength parameter of 1 and rbind them into single set
}
