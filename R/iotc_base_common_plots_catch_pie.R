#'Plots nominal catches by species as a pie chart. The original data should be grouped by year
#'and a factor (e.g. species, species group, fleet etc.) that's also used to colorize the bars
#'@param data A data frame containing nominal catches by \code{YEAR} and a given factor
#'@param fill_by The name of the column to be used to colorize the bar components
#'@param max_categories The number of maximum distinct categories (from the \code{fill_by} column) to be kept in the result. Everything else will be aggregated as 'All others'
#'@param colors A data frame containing the colors (FILL and OUTLINE) for the factors, if set to \code{NA} these will be determined by the \code{FILL_BY} parameter
#'@param num_legend_rows The number of rows to display in the legend
#'@param callouts If \code{TRUE} plots informative callouts for each category
#'@return the plot corresponding to the given input parameters
#'@examples catch_pie(data, fill_by = "SPECIES", colors = factorize_species(all_species_colors()))
#'@export
catch_pie = function(data,
                     fill_by,
                     max_categories = NA,
                     colors = NA,
                     num_legend_rows = 2,
                     callouts = FALSE) {
  return(
    value_pie(
      data,
      CATCH,
      fill_by,
      max_categories,
      colors,
      num_legend_rows,
      callouts
    )
  )
}

#'Alias for \code{\link{catch_pie}}
#'@export
pie.catch = catch_pie
