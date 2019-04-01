#' Calculate the reporting rate of a species
#'
#' Obtain a reporting rate of a species over a specified time period and spatial configuration.
#'
#' This functions assumes that all years are 365 days. Necessary corrections for leap years are made.
#'
#'
#' @param df A dataframe obtained from the species API call. Use `extract_data` if required.
#' @param species_id An integer id. The KBM website has a list of all species with their ids.
#' @param start_date A character or date object denoting the starting point from which reporting rate is to be measured.
#' @param end_date A character or date object denoting the end point from which reporting rate is to be measured.
#' @param pentad_id A character vector denoting the pentad id.
#' @export
#' @return A numeric object between 0 and 1 (inclusive) denoting the reporting rate of a species. The reporting rate is equal to the number of times a species has been recorded divided by the number of full protocol cards in the specified time and region.
#' @examples
#'
#' \dontrun{
#'
#'# the reporting rate of African Black-shouldered Kite in Nairobi National Park
#' reporting_rate(df, species_id = 130, start_date = '1970-01-01', end_date = Sys.Date(), pentad_id = '0120-3650')
#'
#' }
#'
#'
reporting_rate <- function(df, species_id, start_date = '1970-01-01', end_date = Sys.Date(), pentad_id){

  # convert to correct date format
  start_date = as.Date(start_date)
  end_date = as.Date(end_date)

  # get baseline data
  df <- df %>%
    filter(Pentad %in% pentad_id) %>%
    filter(StartDate >= start_date) %>%
    filter(StartDate <= end_date)


  # get number of reports in spatial, temporal space defined
  n_obs <- df %>%
    filter(Spp == species_id) %>%
    nrow()

  total_obs <- df %>%
    nrow()

  # reporting rate
  rr = n_obs / total_obs

  return(rr)



}
