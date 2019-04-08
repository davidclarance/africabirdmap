#' Select pentads based on features
#'
#' Select pentads lying within a defined geographic region. Used as a helper function in the reporting rate functions.
#'
#' @details
#' `filter_pentads` is used to generate a character list of pentads associated with a given feature. Currently all features are political (counties, provices, countries). Over time we might consider adding habitat and climate features.
#'
#'
#' The pentad assignments are correct for all pentads that lie strictly within the national boundaries of Kenya. However since the algorithm uses the midpoint of a pentad to discover its assignment, there are cases where the midpoint lies outside Kenya and so the pentads do not have a classification. In the data these show up as `unclassified`.
#'
#'
#'Ideally we would want to use an assignment based on area. This will likely show up in the next release.
#'
#'
#' @param selected_area Either a pentad (eg: 0105_3930), country (eg: Kenya), county (eg: Kitui) or province (eg: rift valley). Lists of the same can be applied as well. For instance, for multiple pentads you could use, c('0105_3930', '0110c3620').
#' @param selection_type Can take either of the four values: 'Pentad', 'Country', 'County' or 'Province'.
#' @export
#' @return A dataframe where each row is a pentade with the `fit` values, the standard errors (`se`), the `reporting_rates` and its associated inputs. Ignore the various date columns as those are only meant to be used in graphs.
#' @examples
#'
#' # get list of pentads in Rift Valley
#' filter_pentads(selected_area = "Rift Valley", selection_type = "County")
#'
filter_pentads <- function(selected_area, selection_type){

  # correct case
  selected_area = tolower(selected_area)
  selection_type = tolower(selection_type)

  # prepare location features dataset for filtering
  # pentads_geographical_features is the master copy
  selected_pentads <- pentads_geographical_features %>%
    mutate(SelectedPentad = Pentad) %>%
    gather(c("Country", "Province", "County", "Pentad"), key = "Category", value = "Area") %>%
    mutate(Area = tolower(Area)) %>%
    mutate(Category = tolower(Category))

  # check if the selected features exist

  if(!(all(selected_area %in% selected_pentads$Area))){stop("selected_area not in list, check `pentads_geographical_features` for all possible areas")}

  if(!(all(selection_type %in% selected_pentads$Category))){stop("selection_type not in list, possible types are: Pentad, Country, County, Province ")}


  # filter by user selection
  selected_pentads <- selected_pentads %>%
    filter((Area %in% selected_area) & (Category %in% selection_type)) %>%
    select(SelectedPentad) %>%
    pull()


  if(length(selected_pentads) == 0){stop("This specific combination of area and type do not exist, check `pentads_geographical_features` for all possible comvbinations")}

  message(paste0(length(selected_pentads), " pentads selected for ", selected_area))

  return(selected_pentads)

}
