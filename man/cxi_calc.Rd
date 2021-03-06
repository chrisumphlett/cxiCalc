% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cxi_calc.R
\name{cxi_calc}
\alias{cxi_calc}
\title{Tidy Calculation of Customer Experience Index}
\usage{
cxi_calc(survey_data, ...)
}
\arguments{
\item{survey_data}{Raw survey data. Needs to be one row per survey with the three CXi question
responses having column names of needs, ease and emotion}

\item{...}{optional colunns by which to group the CXi calculation. There is no limit to
the number of grouping variables chosen. Too many will likely result in CXi calculations
that are too fragmented / based on very small survey counts.}
}
\value{
Data frame with CXi and survey count for each combination of the grouping variables
}
\description{
Simplifies the calculation of Customer Experience Index (CXi) from raw survey data within
the tidyverse framework.
}
\details{
Customer Experience Index is a metric created by Forrester to help companies systematically
measure customer experience in a way that their research has found is connected to 
improving customer loyalty. More information can be found at https://go.forrester.com/analytics/cx-index/

The calculation across an entire sample of surveys is simple. A customer experience
manager may want to calculate CXi across many different dimensions and filtering
in different ways; the functions in this package utilize the tidy framework to streamline
calculating CXi along as many dimensions as desired.
}
