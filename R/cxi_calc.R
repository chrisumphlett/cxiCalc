#' Tidy Calculation of Customer Experience Index
#'
#' Simplifies the calculation of Customer Experience Index (CXi) from raw survey data within
#' the tidyverse framework.
#' 
#' Customer Experience Index is a metric created by Forrester to help companies systematically
#' measure customer experience in a way that their research has found is connected to 
#' improving customer loyalty. More information can be found at https://go.forrester.com/analytics/cx-index/
#' 
#' The calculation across an entire sample of surveys is simple. A customer experience
#' manager may want to calculate CXi across many different dimensions and filtering
#' in different ways; the functions in this package utilize the tidy framework to streamline
#' calculating CXi along as many dimensions as desired.
#'
#' @param survey_data Raw survey data. Needs to be one row per survey with the three CXi question
#' responses having column names of needs, ease and emotion
#' @param ... optional colunns by which to group the CXi calculation. There is no limit to
#' the number of grouping variables chosen. Too many will likely result in CXi calculations
#' that are too fragmented / based on very small survey counts.
#'
#' @return Data frame with CXi and survey count for each combination of the grouping variables
#' 
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @import tidyr
#' 
#' @export

cxi_calc <- function(survey_data, ...) {
  
  survey_transpose <- survey_transpose(survey_data, ...)
  
  cxi <- survey_transpose %>%
    dplyr::group_by(..., question, response_class) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::ungroup() %>%
    tidyr::spread(response_class, count) %>%
    dplyr::mutate(High = if_else(is.na(High), 0, as.numeric(High)),
                  Mid = if_else(is.na(Mid), 0, as.numeric(Mid)),
                  Low = if_else(is.na(Low), 0, as.numeric(Low)),
                  question_score = (High - Low) / (High + Mid + Low) * 100)
  
  cxi2 <- cxi %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(cxi = mean(question_score),
                     survey_count = sum(High + Low + Mid) / 3) %>%
    dplyr::ungroup()
  
  return(cxi2)
}