#' Transpose survey data to prep for CXi calculation
#'
#' Transposes survey data from wide to long to prepare for tidy calculation of CXi
#' 
#' This function is called by other functions in the package that do the final CXi
#' calculations.
#'
#' @param survey_data Raw survey data. Needs to be one row per survey with the three CXi question
#' responses having column names of needs, ease and emotion
#' @param ... optional colunns by which to group the CXi calculation. There is no limit to
#' the number of grouping variables chosen. Too many will likely result in CXi calculations
#' that are too fragmented / based on very small survey counts.
#'
#' @return Data frame with one row per survey response for each of the three CXi questions
#' 
#' @import dplyr
#' @importFrom magrittr "%>%"
#' @import tidyr


survey_transpose <- function(survey_data, ...) {
  survey_transpose <- {{survey_data}} %>%
    # dplyr::select(survey_id, ..., needs, ease, emotion) %>%
    tidyr::pivot_longer(cols = c(needs, ease, emotion), names_to = "question", values_to = "response") %>%
    dplyr::mutate(response_class = case_when(
      response >= 4 ~ "High",
      response <= 2 ~ "Low",
      TRUE ~ "Mid")) %>%
    dplyr::select(-response)
  
  return(survey_transpose)
}
