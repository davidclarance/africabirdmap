#' Fit the underhill smoother to a reporting rate distribution
#'
#' Fit the underhill smoother (put reference here) to reporting rate distribution over pentades.The underhill smoother is a locally weighted binomial general linear model using a logit link, where the weights are generated using a exponential distribution.
#'
#'
#' @details
#' \itemize{
#' \item Pentades are blocks of 5 consecutive days. The implementation measures pentades from a defined month `start_month`. This implementation needs review and should not be considered final.
#' \item For a full list of all possible values for selection_area, run `View(africabirdmap::pentads_geographical_features)` in the console.
#' }
#'
#' @param raw_data Data for a species extracted uing `extract_data()`. The StartDate column should be of type `date`.
#' @param species_id The species_id for which data is extracted. A complete list of species name and ids are available on the Kenya Bird Map website.
#' @param start_month The month to start the pentades from. Default is 7.
#' @param selected_area Either a pentad (eg: 0105_3930), country (eg: Kenya), county (eg: Kitui) or province (eg: rift valley). Lists of the same can be applied as well. For instance, for multiple pentads you could use, c('0105_3930', '0110c3620').
#' @param selection_type Can take either of the four values: 'Pentad', 'Country', 'County' or 'Province'.
#' @param pentade_window The number of pentades on either side of the target day to give the weights to.
#' @param first_pentade The starting value of the pentades. Default is 1.
#' @param last_pentade The last value of the pentades. Default is 73.
#' @export
#' @return A dataframe where each row is a pentade with the `fit` values, the standard errors (`se`), the `reporting_rates` and its associated inputs. Ignore the various date columns as those are only meant to be used in graphs.
#' @examples
#'
#' \dontrun{
#'
#'
#' underhill_smoother(raw_data, species_id, start_month = 7, pentade_window, first_pentade = 1, last_pentade = 73)
#'
#' }
#'
#'
underhill_smoother <- function(raw_data,
                               species_id,
                               start_month = 7,
                               selected_area = "Kenya" ,
                               selection_type = "Country",
                               pentade_window,
                               first_pentade = 1,
                               last_pentade = 73){
  # get list of pentads
  selected_pentads = filter_pentads(selected_area = selected_area,
                                    selection_type = selection_type)

  species_name = unique(na.omit(raw_data$Common_name))

  # get select columns and create pentades
  clean_data <- raw_data %>%
    filter(Pentad %in% selected_pentads) %>%
    select(CardNo,
           StartDate,
           Pentad,
           TotalSpp,
           Spp) %>%
    # remove rows with no start date
    filter(!(is.na(StartDate))) %>%
    # indicate if the species is present on the card
    mutate(IsPresent = if_else(!(is.na(Spp)), 1, 0)) %>%
    select(-Spp) %>%
    # get the number of days since first july
    # probably need to rename this but TODO: Ask Les
    mutate(DaysSinceJuly1 = days_since_month(StartDate, start_month)) %>%
    mutate(Pentade = floor((DaysSinceJuly1+4)/5))

  # performing summaries
  analysis_data <- clean_data %>%
    group_by(Pentade) %>%
    summarize(

      nIsPresent = sum(IsPresent == 1),
      nCards = n_distinct(CardNo),
      ReportingRate = nIsPresent / nCards

    ) %>%
    ungroup() %>%
    mutate(

      nNotPresent = nCards - nIsPresent
    )

  # glm() takes a response matrix that's the number of successes and the number of failures
  model_response = matrix(c(analysis_data$nIsPresent, analysis_data$nNotPresent),
                          nrow = last_pentade,
                          ncol = 2)

  model_output <- data.frame(pentades = 0, 'fit' = 0, 'se' = 0)

  pentades = first_pentade:last_pentade

  # loop to get model predictions
  for (i in first_pentade:last_pentade){

    # exponential distribution of weight factors for target pentade i
    weights <- exp(-((i - first_pentade:last_pentade)/pentade_window)^2)

    # build model
    model <- glm(model_response ~ pentades,
                 family=binomial(link='logit'),
                 weights = weights)

    # store current pentade
    # can possibly eliminate this step
    current_pentad <- data.frame(pentades = i)

    # get predictions
    # this bit is taken from Marc's script as is
    preds <- data.frame(pentades = i,
                        fit = predict.glm(model, newdata = current_pentad, type = "response", se.fit = TRUE)$fit,
                        se = predict.glm(model, newdata = current_pentad, se.fit = TRUE)$se.fit)
    model_output <- rbind(model_output, preds)				# store just the estimates for the target pentade

  }

  # create output df that has everything we want
  output_df <- model_output %>%
    # to remove the initiator, otherwise this screws up the graph
    filter(pentades > 0) %>%
    left_join(., analysis_data, by = c('pentades' = 'Pentade')) %>%
    # a work around to get neat x-axis values
    mutate(nDays = pentades * 5) %>%
    mutate(DateInYear = as.Date(paste0('2019-0',start_month, '-01')) + nDays) %>%
    mutate(Month = months(DateInYear)) %>%
    mutate(SpeciesId = species_id) %>%
    mutate(SpeciesName = species_name)

  output_df



}
