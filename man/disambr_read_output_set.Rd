% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/disambr_sets.r
\name{disambr_read_output_set}
\alias{disambr_read_output_set}
\title{Reads last output set saved on disk}
\usage{
disambr_read_output_set(
  read_output_set = getOption("disambr_read_output_set"),
  save_set_prefix = getOption("disambr_save_set_prefix"),
  save_set_dir = getOption("disambr_save_set_dir"),
  attr_names_prefix = "disambr_set_"
)
}
\arguments{
\item{read_output_set}{toggle. default is no}

\item{save_set_prefix}{file prefix}

\item{save_set_dir}{file dir}

\item{attr_names_prefix}{arrt prefix}
}
\description{
Reads last output set saved on disk
}
