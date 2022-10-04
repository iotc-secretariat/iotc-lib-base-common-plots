RED = "red"
DARK_RED = darken(RED, amount = 0.2)

#'Pareto plot of values by two factors, one used to determine the categories on the X axis and another to determine the
#'sub-categories for each X category (typically C_FLEET_CODE and C_FISHERY_CODE).
#'@param data A data frame containing values by \code{C_YEAR} and at least two given factors (category / fill)
#'@param value The name of the column holding the actual values
#'@param categorize_by The name of the column to be used to assign labels to the bars
#'@param fill_by The name of the column to be used to colorize the bar components
#'@param max_categories The maximum number of categories to display on the X axis (an "All other" category will be added at the end if required)
#'@param max_fill_categories The number of maximum distinct categories (from the \code{fill_by} column) to be kept in the result. Everything else will be aggregated as 'All others'
#'@param colors A data frame containing the colors (FILL and OUTLINE) for the factors, if set to \code{NA} these will be determined by the \code{FILL_BY} parameter
#'@param num_legend_rows The number of rows to display in the legend
#'@param scale A scaling factor that should be used to divide total values to normalize their extent
#'@param y_axis_label The label for the Y axis
#'@return the plot corresponding to the given input parameters
#'@export
value_pareto = function(data,
                        value,
                        categorize_by,
                        fill_by,
                        max_categories = 5,
                        max_fill_categories = NA,
                        colors = NA,
                        num_legend_rows = NA,
                        scale = 1,
                        y_axis_label = "Value") {
  fail_if_empty(data)

  if(fill_by == categorize_by)
    stop(paste0("The categorical variable (x-axis) and the fill variable cannot be the same ('", fill_by, "')"))

  ALL_OTHERS = "All others"

  customized_colors = is_available(colors)

  if(!customized_colors) colors = factorize_colors(data, fill_by)

  colnames(data)[which(colnames(data) == value)] = "VALUE"

  colnames(data)[which(colnames(data) == categorize_by)] = "CATEGORY"
  colnames(data)[which(colnames(data) == fill_by)]  = "FILL_BY"

  colnames(colors)[which(colnames(colors) == fill_by)]  = "FILL_BY"

  if(!is.na(max_fill_categories)) {
    reduced = shrink_categories_pareto(data, colors, max_fill_categories, customized_colors)

    data = reduced$data
    colors = reduced$colors
  }

  num_years = max(data$YEAR) - min(data$YEAR) + 1

  #Aggregate original data and calculate average yearly value (scaled, if necessary) by category and fill
  data = data[, .(VALUE = sum(VALUE) / scale / num_years), keyby = .(CATEGORY, FILL_BY)]

  #Calculate value by category
  data = data[, VALUE_BY_CATEGORY := sum(VALUE), by = CATEGORY]

  #Order data by descending value by category
  setorderv(data, "VALUE_BY_CATEGORY", -1)

  #Identify all unique categories in descending order
  #Identify the first <max_categories>...
  #...and the remaining ones

  data[, CATEGORY := as.character(CATEGORY)]

  all_categories   = unique(data$CATEGORY)

  major_categories = head(all_categories, min(max_categories, length(all_categories)))
  minor_categories = tail(all_categories, max(0, length(all_categories) - max_categories))

  #Update the category for all minor categories into ALL_OTHERS
  #!!! Unbelievable!!! Also, without that c(categorize_by) it won't work... :O
  data[CATEGORY %in% minor_categories, CATEGORY := ALL_OTHERS]

  #Filter out all data by category and fill that belongs to the *major* categories
  #Filter out all data by category and fill that belongs to the *minor* categories
  data_by_category_and_fill        = subset(data, CATEGORY != ALL_OTHERS)
  data_by_category_and_fill_others = subset(data, CATEGORY == ALL_OTHERS)

  data_by_category_and_fill_others = data_by_category_and_fill_others[, .(VALUE = sum(VALUE), VALUE_BY_CATEGORY = max(VALUE_BY_CATEGORY)), keyby = .(CATEGORY, FILL_BY)]

  #Aggregate data by category for all *major* categories
  #Aggregate data by category for all *minor* categories
  data_by_category        = data_by_category_and_fill[, .(VALUE_BY_CATEGORY = sum(VALUE)), keyby = CATEGORY]
  data_by_category_others = data_by_category_and_fill_others[, .(VALUE_BY_CATEGORY = sum(VALUE)), keyby = CATEGORY]

  #Order the data by category and fill by descending value by category values
  #Order the data by category by descending value by category values
  setorderv(data_by_category_and_fill, "VALUE_BY_CATEGORY", -1)
  setorderv(data_by_category, "VALUE_BY_CATEGORY", -1)

  #Prepares a data frame with all data by category and fill, containing the main categories first and then the "All others" category as last row
  #Prepares a data frame with all data by category, containing the main categories first and then the "All others" category as last row
  data_by_category_and_fill_all = rbind(data_by_category_and_fill, data_by_category_and_fill_others)
  data_by_category_all          = rbind(data_by_category         , data_by_category_others)

  #Calculates the cumulative percentage for the data by category
  data_by_category_all[, VALUE_CUMULATIVE_PERCENTAGE     :=  cumsum(VALUE_BY_CATEGORY / sum(VALUE_BY_CATEGORY) * 100)]

  #Minimum / Maximum value by category values
  cMin  = min(data_by_category_all$VALUE_BY_CATEGORY)
  cMax  = max(data_by_category_all$VALUE_BY_CATEGORY)

  #Calculates the upper range of the Y axis, rounded to the closest multiple of the magnitude
  cMaxx = calculate_limit(cMin, cMax)

  #Adjusts the value cumulative percentage in order to be able to plot the trendline with the right scale
  data_by_category_all[, VALUE_CUMULATIVE_PERCENTAGE_ADJ :=  VALUE_CUMULATIVE_PERCENTAGE * cMaxx / 100]

  #Sets the order of the rows of the data by category and fill in descending value by category order
  setorderv(data_by_category_and_fill_all, "VALUE_BY_CATEGORY", 1)

  #Factorizes the category column in the data by category and fill / data by category according to the major categories ordering
  data_by_category_and_fill_all[, CATEGORY := factor(CATEGORY, levels = c(major_categories, ALL_OTHERS), ordered = TRUE)]
  data_by_category_all         [, CATEGORY := factor(CATEGORY, levels = c(major_categories, ALL_OTHERS), ordered = TRUE)]

  if(is.na(num_legend_rows))
    num_legend_rows = calculate_legend_rows(length(colors))

  p =
    initialize_plot(data_by_category_and_fill_all) +

    theme(axis.text.x        = element_text(size = 10, angle = 90, vjust = 0.3, hjust = 1),
          axis.line.y.right  = element_line(color = DARK_RED),
          axis.ticks.y.right = element_line(color = DARK_RED),
          axis.text.y.right  = element_text(color = DARK_RED),
          axis.title.y.right = element_text(color = DARK_RED, angle = 270)) +

    theme(panel.grid.minor.x = element_blank()) +

    scale_fill_manual  (values = colors$FILL, guide = guide_legend(nrow = num_legend_rows)) +
    scale_colour_manual(values = colors$OUTLINE) +

    geom_col  (aes(x = CATEGORY,
                   y = VALUE,
                   fill  = FILL_BY,
                   color = FILL_BY),
               position = "stack") +

    geom_line (data = data_by_category_all[CATEGORY != ALL_OTHERS ],
               aes(x = CATEGORY,
                   y = VALUE_CUMULATIVE_PERCENTAGE_ADJ),
               colour = DARK_RED,
               group = 1) +

    geom_point(data = data_by_category_all[CATEGORY != ALL_OTHERS ],
               aes(x = CATEGORY,
                   y = VALUE_CUMULATIVE_PERCENTAGE_ADJ),
               colour = DARK_RED) +

    geom_text (data = data_by_category_all[CATEGORY != ALL_OTHERS ],
               aes(x = CATEGORY,
                   y = VALUE_CUMULATIVE_PERCENTAGE_ADJ,
                   label = paste(round(VALUE_CUMULATIVE_PERCENTAGE), "%", sep = "")),
               hjust = 0.40,
               vjust = -1,
               colour = "black") +

    scale_x_discrete() +
    scale_y_continuous(breaks = breaks_for(cMin, cMax),
                       labels = function(x) { format(x, big.mark = ",", scientific = FALSE) },
                       sec.axis = sec_axis(breaks = seq(0, 100, 10),
                                           ~. * 100 / cMaxx, name = "% cumulative total value")) +

    coord_cartesian(ylim = limit_for(cMin, cMax)) +

    labs(x = "", y = y_axis_label)

  return (p)
}

#'Alias for \code{\link{value_pareto}}
#'@export
pareto.value = value_pareto
