#' Calculate the reporting rate of a species
#'
#' Obtain a reporting rate of a species over a specified time period and spatial configuration.
#'
#' @details
#' \itemize{
#' \item This functions assumes that all years are 365 days. Necessary corrections for leap years are made.
#' \item For a full list of all possible values for selection_area, run `View(africabirdmap::pentads_geographical_features)` in the console.
#'}
#'
#' @param df A dataframe obtained from the species API call. Use `extract_data` if required.
#' @param species_id An integer id. The KBM website has a list of all species with their ids.
#' @param start_date A character or date object denoting the starting point from which reporting rate is to be measured.
#' @param selected_area Either a pentad (eg: 0105_3930), country (eg: Kenya), county (eg: Kitui) or province (eg: rift valley). Lists of the same can be applied as well. For instance, for multiple pentads you could use, c('0105_3930', '0110c3620').
#' @param selection_type Can take either of the four values: 'Pentad', 'Country', 'County' or 'Province'.
#' @export
#' @return A numeric object between 0 and 1 (inclusive) denoting the reporting rate of a species. The reporting rate is equal to the number of times a species has been recorded divided by the number of full protocol cards in the specified time and area.
#' @examples
#'
#' \dontrun{
#'
#'# the reporting rate of African Paradise Flycatchers in the coastal region in Kenya.
#' reporting_rate(df,
#' species_id = 682,
#' start_date = '1970-01-01',
#' end_date = Sys.Date(),
#' selected_area = "Coast" ,
#' selection_type = "Province")
#'
#' }
#'
#'
reporting_rate <- function(df,
                           species_id,
                           start_date = '1970-01-01',
                           end_date = Sys.Date(),
                           selected_area = "Kenya" ,
                           selection_type = "Country"){

  # convert to correct date format
  start_date = as.Date(start_date)
  end_date = as.Date(end_date)

  # get list of pentads
  selected_pentads = filter_pentads(selected_area = selected_area,
                                    selection_type = selection_type)


  # get baseline data
  # applying the filters defined in the function here
  df <- df %>%
    filter(Pentad %in% selected_pentads) %>%
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
