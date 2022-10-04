#'Plots values by year as separate lines. The original data should be grouped by year
#'and a factor (e.g. species, species group, fleet etc.) that's also used to colorize the bars
#'@param data A data frame containing values by \code{C_YEAR} and a given factor
#'@param value The name of the column holding the actual values
#'@param time The name of the column representing the 'time' variable
#'@param color_by The name of the column to be used to colorize the lines
#'@param max_categories The number of maximum distinct categories (from the \code{fill_by} column) to be kept in the result. Everything else will be aggregated as 'All others'
#'@param colors A data frame containing the colors (FILL and OUTLINE) for the factors, if set to \code{NA} these will be determined by the \code{FILL_BY} parameter
#'@param plot_points A flag to force plotting solid dots at each data point
#'@param num_legend_rows The number of rows to display in the legend
#'@param scale A scaling factor that should be used to divide total values to normalize their extent
#'@param x_axis_label The label for the X axis
#'@param y_axis_label The label for the Y axis
#'@param x_breaks_every The size of uniform breaks on the X axis
#'@param trim_labels If \code{TRUE} trims all category labels to a maximum of 24 characters
#'@return the plot corresponding to the given input parameters
#'@export
value_line = function(data,
                      value,
                      time = C_YEAR,
                      color_by,
                      max_categories = NA,
                      colors = NA,
                      plot_points = FALSE,
                      num_legend_rows = NA,
                      scale = 1,
                      x_axis_label = NA,
                      y_axis_label = "%",
                      x_breaks_every = 5,
                      trim_labels = TRUE) {
  fail_if_empty(data)

  customized_colors = is_available(colors)

  if(!customized_colors) colors = factorize_colors(data, color_by)

  colnames(data)[which(colnames(data) == value)] = "VALUE"
  colnames(data)[which(colnames(data) == time)]  = "TIME"

  colnames(data)[which(colnames(data) == color_by)] = "FILL_BY"
  colnames(colors)[which(colnames(colors) == color_by)] = "FILL_BY"

  data = data[, .(VALUE = sum(VALUE)), keyby = .(TIME, FILL_BY)]

  if(!is.na(max_categories)) {
    reduced = shrink_categories_value(data, colors, max_categories, customized_colors)

    data = reduced$data
    colors = reduced$colors
  }

  data$VALUE = data$VALUE / scale

  xMin = min(data$TIME)
  xMax = max(data$TIME)

  if(time == C_YEAR) {
    xCur = as.integer(format(Sys.Date(), "%Y"))

    #YEARS = min(data$TIME):max(data$TIME)
    #CATEGORIES = unique(data$FILL_BY)

    #TIME_X_CATEGORIES = expand.grid(TIME = YEARS, FILL_BY = CATEGORIES)

    #data = merge(data, TIME_X_CATEGORIES, by = c("TIME", "FILL_BY"), all.y = TRUE)
  }

  xCur = xMax

  dMin = floor(xMin / x_breaks_every) * x_breaks_every
  dMax = floor(xMax / x_breaks_every) * x_breaks_every

  breaks = unique(c(xMin, seq(dMin, dMax, x_breaks_every), xMax, xCur))

  yData = data[, .(VALUE = sum(VALUE)), keyby = .(TIME)]

  yMin = min(data$VALUE)
  yMax = max(data$VALUE)

  number_categories = length(unique(data$FILL_BY))

  categories = as.character(sort(unique(data$FILL_BY)))

  if(trim_labels) { labels = unlist(lapply(categories, strlen_max_labels)) }
  else labels = categories

  if(is.na(num_legend_rows)) {
    num_legend_rows = calculate_legend_rows(number_categories)
  }

  p =
    initialize_plot(data, aesthetics = aes(x = TIME,
                                           y = VALUE,
                                           color = FILL_BY,
                                           group = FILL_BY)) +

    geom_line(size = 1)

  if(plot_points) {
    p = p +
      geom_point(aes(x = TIME,
                     y = VALUE,
                     color = FILL_BY)
      )
  }

  p = p +
    scale_colour_manual(values = colors$FILL, labels = labels, guide = guide_legend(nrow = num_legend_rows)) +

    scale_x_continuous(expand = c(0, 0), breaks = breaks) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = breaks_for(yMin, yMax),
                       labels = function(x) { format(x, big.mark = ",", scientific = FALSE) },
                       sec.axis = dup_axis(name = element_blank())) +

    coord_cartesian(ylim = limit_for(yMin, yMax)) +

    labs(x = ifelse(is.na(x_axis_label), "", x_axis_label), y = y_axis_label)

  return (p)
}

#'Plots relative generic 'values' by a time-variable (generally \code{C_YEAR}) as relative line plots. The original data should be grouped by year
#'and a factor (e.g. species, species group, fleet etc.) that's also used to colorize the components of each bar
#'@param data A data frame containing values by \code{C_YEAR} and a given factor
#'@param value The name of the column holding the actual values
#'@param time The name of the column representing the 'time' variable
#'@param color_by The name of the column to be used to colorize the line components
#'@param max_categories The number of maximum distinct categories (from the \code{fill_by} column) to be kept in the result. Everything else will be aggregated as 'All others'
#'@param colors A data frame containing the colors (FILL and OUTLINE) for the factors, if set to \code{NA} these will be determined by the \code{FILL_BY} parameter
#'@param plot_points A flag to force plotting solid dots at each data point
#'@param num_legend_rows The number of rows to display in the legend
#'@param x_axis_label The label for the X axis
#'@param y_axis_label The label for the Y axis
#'@param x_breaks_every The size of uniform breaks on the X axis
#'@param trim_labels If \code{TRUE} trims all category labels to a maximum of 24 characters
#'@return the plot corresponding to the given input parameters
#'@export
value_line_rel = function(data,
                          value,
                          time = C_YEAR,
                          color_by,
                          max_categories = NA,
                          colors = NA,
                          plot_points = FALSE,
                          num_legend_rows = NA,
                          x_axis_label = NA,
                          y_axis_label = "Values",
                          x_breaks_every = 5,
                          trim_labels = TRUE) {
  fail_if_empty(data)

  customized_colors = is_available(colors)

  if(!customized_colors) colors = factorize_colors(data, color_by)

  colnames(data)[which(colnames(data) == value)] = "VALUE"
  colnames(data)[which(colnames(data) == time)]  = "TIME"

  colnames(data)[which(colnames(data) == color_by)] = "FILL_BY"
  colnames(colors)[which(colnames(colors) == color_by)] = "FILL_BY"

  data = data[, .(VALUE = sum(VALUE)), keyby = .(TIME, FILL_BY)]

  if(!is.na(max_categories)) {
    reduced = shrink_categories_value(data, colors, max_categories, customized_colors)

    data = reduced$data
    colors = reduced$colors
  }

  data = data[, VALUE_PERC := VALUE / sum(VALUE) * 100, by = .(TIME)]

  xMin = min(data$TIME)
  xMax = max(data$TIME)

  if(time == C_YEAR) xCur = as.integer(format(Sys.Date(), "%Y"))

  xCur = xMax

  dMin = floor(xMin / x_breaks_every) * x_breaks_every
  dMax = floor(xMax / x_breaks_every) * x_breaks_every

  breaks = c(xMin, seq(dMin, dMax, x_breaks_every), xMax, xCur)

  yMin = 0
  yMax = 100

  number_categories = length(unique(data$FILL_BY))

  categories = as.character(sort(unique(data$FILL_BY)))

  if(trim_labels) { labels = unlist(lapply(categories, strlen_max_labels)) }
  else labels = categories

  if(is.na(num_legend_rows)) {
    num_legend_rows = calculate_legend_rows(number_categories)
  }

  p =
    initialize_plot(data, aesthetics = aes(x = TIME,
                                           y = VALUE_PERC,
                                           color = FILL_BY,
                                           group = FILL_BY)) +

    geom_line(size = 1)

  if(plot_points) {
    p = p +
      geom_point(aes(x = TIME,
                     y = VALUE_PERC,
                     color = FILL_BY)
      )
  }

  p = p +
    scale_colour_manual(values = colors$FILL, labels = labels, guide = guide_legend(nrow = num_legend_rows)) +

    scale_x_continuous(expand = c(0, 0), breaks = breaks) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = seq(0, 100, 10),
                       labels = function(x) { format(x, big.mark = ",", scientific = FALSE) },
                       sec.axis = dup_axis(name = element_blank())) +

    coord_cartesian(ylim = limit_for(yMin, yMax)) +

    labs(x = ifelse(is.na(x_axis_label), "", x_axis_label), y = y_axis_label)

  return (p)
}

#'Alias for \code{\link{value_line}}
#'@export
line.value = value_line

#'Alias for \code{\link{value_line_rel}}
#'@export
line.value.rel = value_line_rel
