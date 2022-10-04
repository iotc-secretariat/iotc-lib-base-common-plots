#'Pareto plot of catches by two factors, one used to determine the categories on the X axis and another to determine the
#'sub-categories for each X category (typically C_FLEET_CODE and C_FISHERY_CODE).
#'@param data A data frame containing nominal catches by \code{C_YEAR} and at least two given factors (category / fill)
#'@param categorize_by The name of the column to be used to assign labels to the bars
#'@param fill_by The name of the column to be used to colorize the bar components
#'@param max_categories The maximum number of categories to display on the X axis (an "All other" category will be added at the end if required)
#'@param max_fill_categories The number of maximum distinct categories (from the \code{fill_by} column) to be kept in the result. Everything else will be aggregated as 'All others'
#'@param colors A data frame containing the colors (FILL and OUTLINE) for the factors, if set to \code{NA} these will be determined by the \code{FILL_BY} parameter
#'@param num_legend_rows The number of rows to display in the legend
#'@return the plot corresponding to the given input parameters
#'@examples catch_pareto(data, 2015, 2019, C_FLEET, C_FISHERY_GROUP, max_fill_categories = 9)
#'@export
catch_pareto = function(data,
                        categorize_by,
                        fill_by,
                        max_categories = 5,
                        max_fill_categories = NA,
                        colors = NA,
                        num_legend_rows = NA) {
  return(
    value_pareto(
      data,
      C_CATCH,
      categorize_by,
      fill_by,
      max_categories,
      max_fill_categories,
      colors,
      num_legend_rows,
      scale = 1000,
      y_axis_label = "Average yearly total catch (x1, 000 t)"
    )
  )
}

#'Alias for \code{\link{catch_pareto}}
#'@export
pareto.catch = catch_pareto
