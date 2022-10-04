#'Plots nominal catches by species as a treemap chart. The original data should be grouped by year
#'and a factor (e.g. species, species group, fleet etc.) that's also used to colorize the bars
#'@param data A data frame containing nominal catches by \code{C_YEAR} and a given factor
#'@param fill_by The name of the column to be used to colorize the bar components
#'@param max_categories The number of maximum distinct categories (from the \code{fill_by} column) to be kept in the result. Everything else will be aggregated as 'All others'
#'@param colors A data frame containing the colors (FILL and OUTLINE) for the factors, if set to \code{NA} these will be determined by the \code{FILL_BY} parameter
#'@param num_legend_rows The number of rows to display in the legend
#'@param show_percentages \code{TRUE} to display percentages of each area over the total
#'@param trim_labels If \code{TRUE} trims all category labels to a maximum of 24 characters
#'@return the plot corresponding to the given input parameters
#'@examples catch_pie(data, fill_by = C_SPECIES, colors = factorize_species(all_species_colors()))
#'@export
catch_treemap = function(data,
                         fill_by,
                         max_categories = NA,
                         colors = NA,
                         num_legend_rows = 2,
                         show_percentages = TRUE,
                         trim_labels = TRUE) {
  return(
    value_treemap(
      data,
      C_CATCH,
      fill_by,
      max_categories,
      colors,
      num_legend_rows,
      show_percentages,
      trim_labels = trim_labels
    )
  )
}

#'Alias for \code{\link{catch_pie}}
#'@export
treemap.catch = catch_treemap
