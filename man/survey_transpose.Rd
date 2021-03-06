% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{survey_transpose}
\alias{survey_transpose}
\title{Transpose survey data to prep for CXi calculation}
\usage{
survey_transpose(survey_data, ...)
}
\arguments{
\item{survey_data}{Raw survey data. Needs to be one row per survey with the three CXi question
responses having column names of needs, ease and emotion}

\item{...}{optional colunns by which to group the CXi calculation. There is no limit to
the number of grouping variables chosen. Too many will likely result in CXi calculations
that are too fragmented / based on very small survey counts.}
}
\value{
Data frame with one row per survey response for each of the three CXi questions
}
\description{
Transposes survey data from wide to long to prepare for tidy calculation of CXi
}
\details{
This function is called by other functions in the package that do the final CXi
calculations.
}
