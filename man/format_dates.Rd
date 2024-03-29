% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/format_dates.R
\name{format_dates}
\alias{format_dates}
\title{Formats dates into YYYY-MM-DD.}
\usage{
format_dates(day = 1:2, month = 4:5, year = 2011:2012)
}
\arguments{
\item{day}{a 1 or 2 digit character string (DD) or (D)}

\item{month}{a 1 or 2 digit character string (MM) or (M)}

\item{year}{a 4 digit character string (YYYY)}
}
\value{
a list of rasters with elements corresponding to the
  input vaiable "data".
}
\description{
This function creates a temporary folder and downloads data
  from the SNODAS website for the inputed date, map type, and data map
  that the user provides. This data is stored in the temporary folder.
  The function unzips the selected data files and stores the specified
  maps into a list.
}
