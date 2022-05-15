#'Plots generic 'values' as a treemap chart. The original data should be grouped by year
#'and a factor (e.g. species, species group, fleet etc.) that's also used to colorize the bars
#'@param data A data frame containing values by \code{YEAR} and a given factor
#'@param value The name of the column holding the actual values
#'@param fill_by The name of the column to be used to colorize the bar components
#'@param max_categories The number of maximum distinct categories (from the \code{fill_by} column) to be kept in the result. Everything else will be aggregated as 'All others'
#'@param colors A data frame containing the colors (FILL and OUTLINE) for the factors, if set to \code{NA} these will be determined by the \code{FILL_BY} parameter
#'@param num_legend_rows The number of rows to display in the legend
#'@param show_percentages \code{TRUE} to display percentages of each area over the total
#'@param trim_labels If \code{TRUE} trims all category labels to a maximum of 24 characters
#'@return the plot corresponding to the given input parameters
#'@export
value_treemap = function(data,
                         value,
                         fill_by,
                         max_categories = NA,
                         colors = NA,
                         num_legend_rows = 2,
                         show_percentages = TRUE,
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

  colors = head(colors, length(unique(data$FILL_BY)))

  colors$FILL_BY = categories

  totals = sum(data$VALUE)

  p_data = data[, .(VALUE = sum(VALUE), VALUE_PERC = sum(VALUE) / totals), keyby = .(FILL_BY)]

  p_data$LABEL = p_data$FILL_BY

  if(show_percentages)
    p_data$LABEL = paste(p_data$LABEL, paste0("(", round(p_data$VALUE_PERC * 100, 1), "%", ")"))

  treemap =
    initialize_plot(p_data, custom_theme = theme_void, aesthetics = aes(area = VALUE_PERC, label = LABEL)) +
    theme_void()

  treemap = treemap +
    theme(legend.position = "top",
          plot.margin = unit(c(0, 10, 10, 10), "pt"),
          axis.text.x = element_blank())

  treemap = treemap +
    geom_treemap(aes(fill = FILL_BY, color = FILL_BY),
                 radius = unit(5, "pt"),
                 start = "topleft") +

    #We explicityl provide name = "" otherwise it will print "FILL_BY" on the legend...
    scale_fill_manual  (values = colors$FILL, labels = labels, guide = guide_legend(nrow = num_legend_rows), name = "") +
    scale_colour_manual(values = colors$OUTLINE, guide = guide_none())


  if(show_percentages) {
    new_colors = sapply(colors$FILL, function(c) { ifelse(is_dark(c), lighten(c, amount = 0.4), darken(c, amount = 0.4)) })
    names(new_colors) = NULL

    treemap = treemap +
      new_scale_color() +
      geom_treemap_text(aes(color = FILL_BY),
                        padding.x = unit(.01, "npc"),
                        padding.y = unit(.01, "npc"),
                        reflow = TRUE,
                        start = "topleft",
                        place = "center") +
      scale_colour_manual(values = new_colors)
  }

  return (treemap)
}

#'Alias for \code{\link{value_treemap}}
#'@export
treemap.value = value_treemap
