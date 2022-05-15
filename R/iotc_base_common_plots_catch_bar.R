#'Plots catches by year as stacked bars. The original data should be grouped by year
#'and a factor (e.g. species, species group, fleet etc.) that's also used to colorize the components of each bar
#'@param data A data frame containing nominal catches by \code{YEAR} and a given factor
#'@param fill_by The name of the column to be used to colorize the bar components
#'@param max_categories The number of maximum distinct categories (from the \code{fill_by} column) to be kept in the result. Everything else will be aggregated as 'All others'
#'@param colors A data frame containing the colors (FILL and OUTLINE) for the factors, if set to \code{NA} these will be determined by the \code{FILL_BY} parameter
#'@param num_legend_rows The number of rows to display in the legend
#'@param trim_labels If \code{TRUE} trims all category labels to a maximum of 24 characters
#'@return the plot corresponding to the given input parameters
#'@examples catch_bar(NC_est(), SPECIES_CODE)                   # Bar chart with yearly estimated catches by species code
#'@examples catch_bar(NC_est(), FLEET_CODE, max_categories = 5) # Bar chart with yearly estimated catches by fleet code, limited to the first five fleets in terms of total catches
#'@examples catch_bar(NC_raw_std(years = c(2000:2010), species_group_codes = "TUNAS"), SPECIES_CODE, colors = color_table(unique_colors(16))) # Bar chart with yearly (2000-2010) raw catches by species code (for tuna species) with colors re-assigned
#'@export
catch_bar = function(data,
                     fill_by,
                     max_categories = NA,
                     colors = NA,
                     num_legend_rows = NA,
                     trim_labels = TRUE) {
  return(
    value_bar(data,
              CATCH,
              YEAR,
              fill_by,
              max_categories,
              colors,
              num_legend_rows,
              scale = 1000,
              y_axis_label = "Total catch (x1,000 t)",
              x_breaks_every = 5,
              trim_labels
    )
  )
}

#'Plots the relative catches by year as stacked bars. The original data should be grouped by year
#'and a factor (e.g. species, species group, fleet etc.) that's also used to colorize the components of each bar
#'@param data A data frame containing nominal catches by \code{YEAR} and a given factor
#'@param fill_by The name of the column to be used to colorize the bar components
#'@param max_categories The number of maximum distinct categories (from the \code{fill_by} column) to be kept in the result. Everything else will be aggregated as 'All others'
#'@param colors A data frame containing the colors (FILL and OUTLINE) for the factors, if set to \code{NA} these will be determined by the \code{FILL_BY} parameter
#'@param num_legend_rows The number of rows to display in the legend
#'@param trim_labels If \code{TRUE} trims all category labels to a maximum of 24 characters
#'@return the plot corresponding to the given input parameters
#'@examples catch_bar_expanded(NC_est(), SPECIES_CODE)                   # Expanded bar chart with yearly estimated catches by species code
#'@examples catch_bar_expanded(NC_est(), FLEET_CODE, max_categories = 5) # Expanded bar chart with yearly estimated catches by fleet code, limited to the first five fleets in terms of total catches
#'@examples catch_bar_expanded(NC_raw_std(years = c(2000:2010), species_group_codes = "TUNAS"), SPECIES_CODE, colors = color_table(unique_colors(16))) # Expanded bar chart with yearly (2000-2010) raw catches by species code (for tuna species) with colors re-assigned
#'@export
catch_bar_rel = function(data,
                         fill_by,
                         max_categories = NA,
                         colors = NA,
                         num_legend_rows = NA,
                         trim_labels = TRUE) {
  return(
    value_bar_rel(
      data,
      CATCH,
      YEAR,
      fill_by,
      max_categories,
      colors,
      num_legend_rows,
      y_axis_label = "% total catch",
      x_breaks_every = 5,
      trim_labels
    )
  )
}

#'Alias for \code{\link{catch_bar}}
#'@export
bar.catch = catch_bar

#'Alias for \code{\link{catch_bar_rel}}
#'@export
bar.catch.rel = catch_bar_rel
