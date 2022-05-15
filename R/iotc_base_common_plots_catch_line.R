#'Plots nominal catches by year as separate lines. The original data should be grouped by year
#'and a factor (e.g. species, species group, fleet etc.) that's also used to colorize the bars
#'@param data A data frame containing nominal catches by \code{YEAR} and a given factor
#'@param color_by The name of the column to be used to colorize the lines
#'@param max_categories The number of maximum distinct categories (from the \code{fill_by} column) to be kept in the result. Everything else will be aggregated as 'All others'
#'@param colors A data frame containing the colors (FILL and OUTLINE) for the factors, if set to \code{NA} these will be determined by the \code{FILL_BY} parameter
#'@param plot_points A flag to force plotting solid dots at each data point
#'@param num_legend_rows The number of rows to display in the legend
#'@param trim_labels If \code{TRUE} trims all category labels to a maximum of 24 characters
#'@return the plot corresponding to the given input parameters
#'@examples catch_line(NC_est(species_group_codes = "TUNAS"), SPECIES_CODE)
#'@examples catch_line(NC_est(species_group_codes = "TUNAS"), SPECIES_CODE, colors = color_table(unique_colors(32)))
#'@export
catch_line = function(data,
                      color_by,
                      max_categories = NA,
                      colors = NA,
                      plot_points = FALSE,
                      num_legend_rows = NA,
                      trim_labels = TRUE) {
  return(
    value_line(
      data,
      CATCH,
      YEAR,
      color_by,
      max_categories,
      colors,
      plot_points,
      num_legend_rows,
      scale = 1000,
      y_axis_label = "Total catch (x1,000 t)",
      x_breaks_every = 5,
      trim_labels
    )
  )
}

#'Alias for \code{\link{catch_line}}
#'@export
line.catch = catch_line
