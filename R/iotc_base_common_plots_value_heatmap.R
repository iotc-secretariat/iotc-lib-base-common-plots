#'String constant to force the calculation of heatmap relative values across the X-axis
#'@export
HEATMAP_RELATIVE_X  = "X"

#'String constant to force the calculation of heatmap relative values across the Y-axis
#'@export
HEATMAP_RELATIVE_Y  = "Y"

#'String constant to force the calculation of heatmap relative values across both axis
#'@export
HEATMAP_RELATIVE_XY = "XY"

#'Produces a heatmap of values categorized by two factors.
#'
#'@param data a data table
#'@param value the name of the column holding the value
#'@param category_x the name of the column holding the X-axis category
#'@param category_y the name of the column holding the Y-axis category
#'@param unit an optional unit to be displayed for the value
#'@param midpoint an optional midpoint for the range of possible values
#'@param relative if \code{NA}, the heatmap will use the absolute values, otherwise these will be normalized across the chosen axis, namely
#'\code{HEATMAP_RELATIVE_X} for the X-axis, \code{HEATMAP_RELATIVE_Y} for the Y-axis and \code{HEATMAP_RELATIVE_XY} for both axis
#'@param color_low an optional color for the lower end of the gradient (defaults to red)
#'@param color_mid an optional color for the middle of the gradient (defaults to yellow)
#'@param color_high an optional color for the higher end of the gradient (defaults to green)
#'@export
value_heatmap = function(data,
                         value,
                         category_x,
                         category_y,
                         unit = NA,
                         midpoint = NA,
                         relative = NA,
                         color_low = "red",
                         color_mid = "yellow",
                         color_high = "green") {
  fail_if_empty(data)

  colnames(data)[which(colnames(data) == value)]      = "VALUE"
  colnames(data)[which(colnames(data) == category_x)] = "X"
  colnames(data)[which(colnames(data) == category_y)] = "Y"

  data = data[, .(VALUE = sum(VALUE)), keyby = .(X, Y)]

  legend_title = normalize_text(value, TRUE)

  if(!is.na(unit)) legend_title = paste0(legend_title, " (", unit, ")")

  if(!is.na(relative)) {
    legend_title = paste("%", normalize_text(value), "by")

    if(relative == "X") {
      data = data[, MAX_VALUE := max(VALUE), by = .(Y)]
      data$VALUE = data$VALUE / data$MAX_VALUE

      legend_title = paste(legend_title, normalize_text(category_y))
    } else if(relative == "Y") {
      data = data[, MAX_VALUE := max(VALUE), by = .(X)]
      data$VALUE = data$VALUE / data$MAX_VALUE

      legend_title = paste(legend_title, normalize_text(category_x))
    } else if(relative == "XY") {
      M = max(data$VALUE)
      data$VALUE = data$VALUE / M

      legend_title = paste(legend_title, normalize_text(category_x), "and", normalize_text(category_y))
    }

    data$VALUE = data$VALUE * 100

    limits = c(0, 100)
  } else {
    limits = c(min(data$VALUE), max(data$VALUE))
  }

  if(is.na(midpoint)) midpoint = ( max(data$VALUE) + min(data$VALUE) ) / 2

  heatmap =
    ggplot(data, aes(X, Y, fill = VALUE)) +
    geom_tile() +
    scale_fill_gradient2(
      name     = legend_title,
      low      = color_low,
      mid      = color_mid,
      high     = color_high,
      midpoint = midpoint,
      limits   = limits,
      guide    = "colourbar"
    ) +
    theme_bw() +
    theme(legend.position = "right",
          axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.2)) +
    xlab(normalize_text(category_x, TRUE)) +
    ylab(normalize_text(category_y, TRUE))

  return (heatmap)
}

#'Alias for \code{\link{value_heatmap}}
#'@export
heatmap.value = value_heatmap
