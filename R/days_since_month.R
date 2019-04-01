#' Calculate days from a given calendar month
#'
#' An a-historical method to calculate pentades by calculating distance from a given calendar month.
#'
#' This functions assumes that all years are 365 days. Necessary corrections for leap years are made.
#'
#'
#' @param date A `date` object. This can be a single date or a vector of dates.
#' @param start_month An integer between 1 and 12 (inclusive) that indicates the month you want to measure distance in days from. Default is 7.
#' @export
#' @return This function returns an integer between 1 and 365 indicating the number of days elapsed since the first of the month indicated in `start_month`
#' @examples
#'
#' # single date conversion example
#' days_since_month(date = as.Date('2018-09-14'), start_month = 7)
#'
#'# multiple dates conversion examples
#'sequence_of_dates <- c(as.Date("2019-01-01"), as.Date("2019-03-19"))
#'days_since_month(date = sequence_of_dates, start_month = 6)
#'
days_since_month <- function(date, start_month = 7){

  # check where the input month is
  current_month <- lubridate::month(date)

  # get start year to measure distance from
  start_year <- dplyr::if_else(current_month >= start_month, lubridate::year(date), lubridate::year(date) - 1)

  # create vector of start dates
  start_date <- as.Date(glue::glue('{start_year}-{start_month}-1'))

  # correct for leap year
  n_days = dplyr::if_else(
    (
      (lubridate::leap_year(date)) & (current_month %in% 3:12) & (current_month < start_month)) |
      ((lubridate::leap_year(date)) & (current_month %in% 3:12) & (current_month > start_month) & (start_month %in% 1:2)) |
      ((lubridate::leap_year(start_date)) & (start_month == 2) & (current_month < start_month)),

    as.numeric(date - start_date),
    as.numeric(date - start_date) + 1)

 n_days

}
