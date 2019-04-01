#' Plot the underhill smoother and associated confidence intervals for a species.
#'
#' Fit a line through the reporting rates and its associated 1 and 2 sigma confidence intervals. 1 and 2 sigma roughly correspond to 67 percent and 95 percent confidence intervals. The line is over Pentades defined in the `underhill_smoother()` function.
#'
#' The dark, thinner interval is the 1 sigma interval and corresponds to 67% confidence, whereas the 2 sigma interval corresponds to a 95% confidence.
#'
#' @param underhill_smoother The dataframe returned by the `underhill_smoother function`.
#' @param species_id The species_id for which data is extracted. A complete list of species name and ids are available on the Kenya Bird Map website.
#' @param species_name The name of the species you are plotting. This is to use in the title of the plots.
#' @export
#' @return A ggplot plot
#' @examples
#'
#' \dontrun{
#'
#'
#' underhill_curves(underhill_smoother, species_id = 103, species_name = "African Black-shouldered Kite")
#'
#' }
#'
#'
underhill_curves <- function(underhill_smoother, species_id, species_name){

  output_plot <- underhill_smoother %>%
    ggplot(aes(y = fit)) +
    theme_light() +
    # plot points, line and the intervals
    geom_point(aes(x = DateInYear, y = ReportingRate)) +
    geom_smooth(aes(x = DateInYear, ymin = fit - 2*se, ymax = fit + 2*se), stat = 'identity') +
    geom_smooth(aes(x = DateInYear, ymin = fit - 1*se, ymax = fit + 1*se), stat = 'identity') +
    # fix scales
    scale_x_date(labels = date_format("%b"), date_breaks = 'month') +
    scale_y_continuous(labels = percent_format()) +
    # add labels
    xlab('Month') +
    ylab('Reporting rate') +
    ggtitle(paste0(species_id, ": ", species_name))

  output_plot
}
