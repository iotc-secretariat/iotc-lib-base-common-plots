#'Plots generic 'values' by a time-variable (generally \code{C_YEAR}) as stacked bars. The original data should be grouped by year
#'and a factor (e.g. species, species group, fleet etc.) that's also used to colorize the components of each bar
#'@param data A data frame containing values by \code{C_YEAR} and a given factor
#'@param value The name of the column holding the actual values
#'@param time The name of the column representing the 'time' variable
#'@param fill_by The name of the column to be used to colorize the bar components
#'@param max_categories The number of maximum distinct categories (from the \code{fill_by} column) to be kept in the result. Everything else will be aggregated as 'All others'
#'@param colors A data frame containing the colors (FILL and OUTLINE) for the factors, if set to \code{NA} these will be determined by the \code{FILL_BY} parameter
#'@param num_legend_rows The number of rows to display in the legend
#'@param scale A scaling factor that should be used to divide total values to normalize their extent
#'@param x_axis_label The label for the X axis
#'@param y_axis_label The label for the Y axis
#'@param x_breaks_every The size of uniform breaks on the X axis
#'@param trim_labels If \code{TRUE} trims all category labels to a maximum of 24 characters
#'@return the plot corresponding to the given input parameters
#'@export
value_bar = function(data,
                     value,
                     time = C_YEAR,
                     fill_by,
                     max_categories = NA,
                     colors = NA,
                     num_legend_rows = NA,
                     scale = 1,
                     x_axis_label = NA,
                     y_axis_label = "Value",
                     x_breaks_every = 5,
                     trim_labels = TRUE) {
  fail_if_empty(data)

  customized_colors = is_available(colors)

  if(!customized_colors) colors = factorize_colors(data, fill_by)

  colnames(data)[which(colnames(data) == value)] = "VALUE"
  colnames(data)[which(colnames(data) == time)]  = "TIME"

  colnames(data)[which(colnames(data) == fill_by)] = "FILL_BY"
  colnames(colors)[which(colnames(colors) == fill_by)] = "FILL_BY"

  data = data[, .(VALUE = sum(VALUE)), keyby = .(TIME, FILL_BY)]

  if(!is.na(max_categories)) {
    reduced = shrink_categories_value(data, colors, max_categories, customized_colors)

    data = reduced$data
    colors = reduced$colors
  }

  data$VALUE = data$VALUE / scale

  xMin = min(data$TIME)
  xMax = max(data$TIME)

  #if(time == C_YEAR) xCur = as.integer(format(Sys.Date(), "%Y"))
  #xCur = xMax

  dMin = floor(xMin / x_breaks_every) * x_breaks_every
  dMax = floor(xMax / x_breaks_every) * x_breaks_every

  x_breaks = c(xMin, seq(dMin, dMax, x_breaks_every))

  if(x_breaks_every == 1 | ( xMax - dMax ) > 1)
    x_breaks = append(x_breaks, xMax)

  x_breaks = unique(x_breaks)

  yData = data[, .(VALUE = sum(VALUE)), keyby = .(TIME)]

  yMin = min(yData$VALUE)
  yMax = max(yData$VALUE)

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
                                           fill  = FILL_BY,
                                           color = FILL_BY)) +

    geom_bar(stat = "identity",
             position = "stack",
             width = 1,
             size = .2) +

    scale_fill_manual  (values = colors$FILL, labels = labels, guide = guide_legend(nrow = num_legend_rows)) +
    scale_colour_manual(values = colors$OUTLINE, guide = guide_none())

  p = p + scale_x_continuous(expand = c(0, 0), breaks = x_breaks)

  p = p +
    scale_y_continuous(expand = c(0, 0),
                       breaks = breaks_for(yMin, yMax),
                       labels = function(x) { format(x, big.mark = ",", scientific = FALSE) },
                       sec.axis = dup_axis(name = element_blank())) +

    coord_cartesian(ylim = limit_for(yMin, yMax)) +

    labs(x = ifelse(is.na(x_axis_label), "", x_axis_label), y = y_axis_label)

  return (p)
}

#'Plots relative generic 'values' by a time-variable (generally \code{C_YEAR}) as stacked bars. The original data should be grouped by year
#'and a factor (e.g. species, species group, fleet etc.) that's also used to colorize the components of each bar
#'@param data A data frame containing values by \code{C_YEAR} and a given factor
#'@param value The name of the column holding the actual values
#'@param time The name of the column representing the 'time' variable
#'@param fill_by The name of the column to be used to colorize the bar components
#'@param max_categories The number of maximum distinct categories (from the \code{fill_by} column) to be kept in the result. Everything else will be aggregated as 'All others'
#'@param colors A data frame containing the colors (FILL and OUTLINE) for the factors, if set to \code{NA} these will be determined by the \code{FILL_BY} parameter
#'@param num_legend_rows The number of rows to display in the legend
#'@param x_axis_label The label for the X axis
#'@param y_axis_label The label for the Y axis
#'@param x_breaks_every The size of uniform breaks on the X axis
#'@param trim_labels If \code{TRUE} trims all category labels to a maximum of 24 characters
#'@return the plot corresponding to the given input parameters
#'@export
value_bar_rel = function(data,
                         value,
                         time = C_YEAR,
                         fill_by,
                         max_categories = NA,
                         colors = NA,
                         num_legend_rows = NA,
                         x_axis_label = NA,
                         y_axis_label = "%",
                         x_breaks_every = 5,
                         trim_labels = TRUE) {
  fail_if_empty(data)

  customized_colors = is_available(colors)

  if(!customized_colors) colors = factorize_colors(data, fill_by)

  colnames(data)[which(colnames(data) == value)] = "VALUE"
  colnames(data)[which(colnames(data) == time)]  = "TIME"

  colnames(data)[which(colnames(data) == fill_by)] = "FILL_BY"
  colnames(colors)[which(colnames(colors) == fill_by)] = "FILL_BY"

  data = data[, .(VALUE = sum(VALUE)), keyby = .(TIME, FILL_BY)]

  if(!is.na(max_categories)) {
    reduced = shrink_categories_value(data, colors, max_categories, customized_colors)

    data = reduced$data
    colors = reduced$colors
  }

  data = data[, VALUE_PERC := VALUE / sum(VALUE) * 100, by = .(TIME)]

  xMin = min(data$TIME)
  xMax = max(data$TIME)

  #if(time == C_YEAR) xCur = as.integer(format(Sys.Date(), "%Y"))
  #xCur = xMax

  dMin = floor(xMin / x_breaks_every) * x_breaks_every
  dMax = floor(xMax / x_breaks_every) * x_breaks_every

  x_breaks = c(xMin, seq(dMin, dMax, x_breaks_every))

  if(x_breaks_every == 1 | ( xMax - dMax ) > 1)
    x_breaks = append(x_breaks, xMax)

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
                                           fill  = FILL_BY,
                                           color = FILL_BY)) +

    geom_bar(stat = "identity",
             position = "stack",
             width = 1,
             size = .2) +

    scale_fill_manual  (values = colors$FILL, labels = labels, guide = guide_legend(nrow = num_legend_rows)) +
    scale_colour_manual(values = colors$OUTLINE, guide = guide_none())

  p = p + scale_x_continuous(expand = c(0, 0), breaks = x_breaks)

  p = p +
    scale_y_continuous(expand = c(0, 0),
                       breaks = seq(0, 100, 10),
                       labels = function(x) { format(x, scientific = FALSE) },
                       sec.axis = dup_axis(name = element_blank())) +

    labs(x = ifelse(is.na(x_axis_label), "", x_axis_label), y = y_axis_label)

  return (p)
}

#'Alias for \code{\link{value_bar}}
#'@export
bar.value = value_bar

#'Alias for \code{\link{value_bar_rel}}
#'@export
bar.value.rel = value_bar_rel
