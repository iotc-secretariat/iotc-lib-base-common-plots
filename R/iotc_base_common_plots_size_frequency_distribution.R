#'Plots a faceted set of size frequencies (as histogram or line plots)
#'@param data A size-frequency data set (as returned by \code{SF_raw}, \code{SF_est} and similar methods)
#'@param categorize_by The name of the column to be used to categorize the facets
#'@param measure_type The measurement type to use as X-axis label
#'@param measure_unit The measurement unit (to be used in the X-axis label)
#'@param draw_line When \code{TRUE} a line geometry (i.e. \code{geom_line}) will be used to plot the distribution
#'@param bin_size The bin size
#'@param colors A data frame containing the colors (FILL and OUTLINE) for the factors, if set to \code{NA} these will be determined by the \code{FILL_BY} parameter
#'@return the plot
#'@export
size_distribution <- function(data,
                              categorize_by = C_FISHERY_CODE,
                              measure_type = "Fork length",
                              measure_unit = "cm",
                              draw_line = FALSE,
                              bin_size = 1,
                              colors = NA) {
  fail_if_empty(data)

  customized_colors = is_available(colors)

  if(!customized_colors) colors = factorize_colors(data, categorize_by)

  colnames(data)[which(colnames(data) == categorize_by)] = "CATEGORY"

  data[, YEAR := factor(YEAR, levels = min(YEAR):max(YEAR), ordered = TRUE)]

  if(bin_size > 1) {
    data[, CLASS_LOW := floor( CLASS_LOW / bin_size ) * bin_size]
  }

  data = data[, .(FISH_COUNT = sum(FISH_COUNT)), keyby = .(YEAR, CLASS_LOW, CATEGORY)]

  data[, FISH_COUNT_NORM := FISH_COUNT / sum(FISH_COUNT, na.rm=T) * 100, by = .(YEAR, CATEGORY)]

  labels = data[, .(NUM_SAMPLES = paste0("n=", format(round(sum(FISH_COUNT, na.rm=T)), big.mark = ",", scientific = FALSE))),
                    keyby = .(YEAR, CATEGORY)]

  p =

    initialize_plot(data, aesthetics = aes(x = CLASS_LOW,
                                           y = FISH_COUNT_NORM,
                                           fill = CATEGORY,
                                           color = CATEGORY)) +
    scale_fill_manual (values = colors$FILL) +
    scale_color_manual(values = colors$OUTLINE) +
    theme(strip.background = element_blank())

  if(draw_line)
    p = p + geom_line()
  else
    p = p + geom_bar(stat = "identity")

  xlab = "Size"

  if(!is.na(measure_type)) xlab = measure_type
  if(!is.na(measure_unit)) xlab = paste(xlab, paste0("(", measure_unit, ")"))

  p = p +

    facet_wrap(ncol = length(levels(factor(data$CATEGORY))),
               facets = ~YEAR + ~CATEGORY,
               labeller = function(labels) { label_value(labels, multi_line = FALSE )}) +

    facet_grid(vars(YEAR), vars(CATEGORY)) +

    geom_text(data = labels,
              color = "black",
              mapping = aes(x = +Inf,
                            y = +Inf,
                            hjust = 1.1,
                            vjust = 1.2,
                            label = NUM_SAMPLES)) +

    scale_x_continuous(expand = c(0.05, 0.05)) +
    scale_y_continuous(expand = c(0.05, 0.05)) +

    labs(x = xlab, y="% samples") +

    theme(legend.position = "none")

  return (p)
}
