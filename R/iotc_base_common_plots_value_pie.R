#'Plots generic 'values' as a pie chart. The original data should be grouped by year
#'and a factor (e.g. species, species group, fleet etc.) that's also used to colorize the bars
#'@param data A data frame containing values by \code{C_YEAR} and a given factor
#'@param value The name of the column holding the actual values
#'@param fill_by The name of the column to be used to colorize the bar components
#'@param max_categories The number of maximum distinct categories (from the \code{fill_by} column) to be kept in the result. Everything else will be aggregated as 'All others'
#'@param colors A data frame containing the colors (FILL and OUTLINE) for the factors, if set to \code{NA} these will be determined by the \code{FILL_BY} parameter
#'@param num_legend_rows The number of rows to display in the legend
#'@param callouts If \code{TRUE} plots informative callouts for each category
#'@param trim_labels If \code{TRUE} trims all category labels to a maximum of 24 characters
#'@return the plot corresponding to the given input parameters
#'@export
value_pie = function(data,
                     value,
                     fill_by,
                     max_categories = NA,
                     colors = NA,
                     num_legend_rows = 2,
                     callouts = FALSE,
                     trim_labels = TRUE) {
  fail_if_empty(data)

  customized_colors = is_available(colors)

  if(!customized_colors) colors = factorize_colors(data, fill_by)

  colnames(data)[which(colnames(data) == value)] = "VALUE"

  colnames(data)[which(colnames(data) == fill_by)] = "FILL_BY"
  colnames(colors)[which(colnames(colors) == fill_by)] = "FILL_BY"

  data = data[, .(VALUE = sum(VALUE)), keyby = .(YEAR, FILL_BY)]

  if(!is.na(max_categories)) {
    reduced = shrink_categories_value(data, colors, max_categories, customized_colors)

    data = reduced$data
    colors = reduced$colors
  }

  categories = as.character(sort(unique(data$FILL_BY)))

  if(trim_labels) { labels = unlist(lapply(categories, strlen_max_labels)) }
  else labels = categories

  colors = head(reduced$colors, length(unique(data$FILL_BY)))

  colors$FILL_BY = categories

  totals = sum(data$VALUE)

  p_data = data[, .(VALUE = sum(VALUE), VALUE_PERC = sum(VALUE) / totals), keyby = .(FILL_BY)]

  if(callouts) {
    # See: https://stackoverflow.com/questions/44436214/ggplot-pie-chart-labeling
    p_data$LPOS = ( cumsum( c(0, p_data$VALUE_PERC)) + c(p_data$VALUE_PERC / 2, .01))[1:nrow(p_data)]
  }

  pie =
    initialize_plot(p_data, custom_theme = theme_void, aesthetics = aes(x = factor(1),
                                                                        y = VALUE_PERC,
                                                                        fill = FILL_BY)) +
    theme_void()

  if(callouts) {
    pie = pie +
      theme(legend.position = "none",
            axis.text.x = element_blank())
  } else {
    pie = pie +
      theme(legend.position = "top",
            axis.text.x = element_blank())
  }

  pie = pie +
    geom_col(color = colors$OUTLINE,
             position = position_stack(reverse = TRUE))

  if(callouts) {
    pie = pie +
      geom_text_repel(
        aes(label = paste(FILL_BY, paste("(", round(VALUE_PERC * 100, 1), "%", ")", sep="")),
            x = 1.4,
            y = LPOS),
        nudge_x = .5,
        segment.size = .3,
        size  = 4
      )
  } else {
    pie = pie +
      geom_text(
        aes(label = paste(round(VALUE_PERC * 100, 1), "%", sep="")),
        position  = position_stack(reverse = TRUE, vjust = 0.5),
        color = "black",
        size  = 4
      )
  }

  pie = pie +
    coord_polar("y") +
    #We explicityl provide name = "" otherwise it will print "FILL_BY" on the legend...
    scale_fill_manual(name = "", values = colors$FILL, labels = labels, guide = guide_legend(nrow = num_legend_rows))
    #+ scale_colour_manual(values = colors$OUTLINE)

  return (pie)
}

#'Alias for \code{\link{value_pie}}
#'@export
pie.value = value_pie
