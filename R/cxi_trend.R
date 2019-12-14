#' Tidy Calculation of Customer Experience Index trends by group
#'
#' Simplifies the calculation of Customer Experience Index (CXi) trends over time from raw survey
#' data within the tidyverse framework. 
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
#' The trend version of the function allows you to specify one column as a date over which to 
#' trend the data. This allows quick filtering to eliminate groupings that fail to meet 
#' user-specified thresholds for average or minimum survey counts per time period.
#' 
#' The resulting data set is set up for creating faceted line plots using ggplot2.
#'
#' @param survey_data Raw survey data. Needs to be one row per survey with the three CXi question
#' responses having column names of needs, ease and emotion
#' @param trend_var Column that represents an element of time, eg week number, date, month & year.
#' @param min_surveys Minimum surveys found in every period for each group to be included
#' @param avg_surveys AVerage surveys found in every period for each group to be included
#' @param ... optional colunns by which to group the CXi calculation. There is no limit to
#' the number of grouping variables chosen. Too many will likely result in CXi calculations
#' that are too fragmented / based on very small survey counts.
#'
#' @return Data frame with CXi and survey count for each combination of the grouping variables over
#' the time variable.
#'
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @import tidyr
#'
#' @export

cxi_trend <- function(survey_data, trend_var, min_surveys, avg_surveys, ...) {
  
  survey_transpose <- survey_transpose(survey_data, ...)
  
  cxi <- survey_transpose %>%
    dplyr::group_by(..., {{trend_var}}, .data$question, .data$response_class) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::ungroup() %>%
    tidyr::spread(.data$response_class, count) %>%
    dplyr::mutate(High = if_else(is.na(.data$High), 0, as.numeric(.data$High)),
                  Mid = if_else(is.na(.data$Mid), 0, as.numeric(.data$Mid)),
                  Low = if_else(is.na(.data$Low), 0, as.numeric(.data$Low)),
                  question_score = (.data$High - .data$Low) / 
                    (.data$High + .data$Mid + .data$Low) * 100)
  
  cxi2 <- cxi %>%
    dplyr::group_by(..., {{trend_var}}) %>%
    dplyr::summarise(cxi = mean(.data$question_score),
                     survey_count = sum(.data$High + .data$Low + .data$Mid) / 3) %>%
    dplyr::ungroup()
  
  cxi3 <- cxi2 %>%
    dplyr::group_by(...) %>%
    dplyr::summarise(avg_survey_ct = mean(.data$survey_count),
                     min_survey_ct = min(.data$survey_count)) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(cxi2) %>%
    dplyr::filter(.data$avg_survey_ct >= avg_surveys, .data$min_survey_ct >= min_surveys)
  
  
  return(cxi3)
  
}