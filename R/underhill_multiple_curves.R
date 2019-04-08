#' Plot the underhill smoother and associated confidence intervals for multiple species.
#'
#' Fit a line through the reporting rates and its associated 2 sigma confidence intervals for multiple species.
#'
#' To utilize this function first you need to create a list of dataframe. Do this by calling the `list()` function and passing the dataframes as arguments
#'
#' @param multiple_underhill_smoothers A list of the dataframes returned by the `underhill_smoother function`.

#' @return A ggplot plot
#' @examples
#'
#' \dontrun{
#'
#' list_of_interesting_species = list(underhill_smoother1 , underhill_smoother2, underhill_smoother3)
#' underhill_curves(list_of_interesting_species)
#'
#' }
#'
#'
underhill_multiple_curves <- function(multiple_underhill_smoothers){

  df = bind_rows(multiple_underhill_smoothers)

  output_plot <- df %>%
    ggplot(aes(y = fit)) +
    theme_light() +
    geom_smooth(aes(x = DateInYear, ymin = fit - 1*se, ymax = fit + 1*se, fill = SpeciesName, color = SpeciesName), stat = 'identity', alpha = 0.1) +
    # fix scales
    scale_x_date(labels = date_format("%b"), date_breaks = 'month') +
    scale_y_continuous(labels = percent_format(),limits = c(0, 1)) +
    # add labels
    xlab('Month') +
    ylab('Reporting rate') +
    ggtitle("Reporting rates for multiple species")


  output_plot
}
