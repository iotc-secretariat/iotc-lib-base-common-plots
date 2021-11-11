#'Plots a faceted set of size frequencies (as ridge plots)
#'@param data A size-frequency data set (as returned by \code{SF_raw}, \code{SF_est} and similar methods)
#'@param categorize_by The name of the column (in \code{data} that contains the categories)
#'@param colors A data frame containing the colors (FILL and OUTLINE) for the factors, if set to \code{NA} these will be determined by the \code{FILL_BY} parameter
#'@param rel_min_height A filter on the minimum relative height (0..1) that points of each yearly time series should have to be maintained in the plot. Defaults to 0 (i.e., all points are kept)
#'@param scale The scale to be used for the ridge lines (the higher, the more the ridge lines for consecutive years will overlap)
#'@param measure_type The measurement type to use as X-axis label
#'@param measure_unit The measurement unit (to be used in the X-axis label)
#'@param show_samples_proportion When \code{TRUE} each ridgeline in the same facet will be assigned an opacity that's proportional to the relative yearly number of samples (for that category)
#'@param show_median When \code{TRUE} plots the median size (for each year in each category) as a green dot
#'@param show_mean When \code{TRUE} plots the mean size (for each year in each category) as a red dot
#'@param num_cols The number of columns in the faceted plot
#'@param num_rows The number of rows in the faceted plot
#'@return the plot
#'@export
size_ridges = function(data,
                       categorize_by = FISHERY_CODE,
                       colors = NA,
                       rel_min_height = 0,
                       scale = 3,
                       measure_type = "Fork length",
                       measure_unit = "cm",
                       show_samples_proportion = FALSE,
                       show_median = TRUE,
                       show_mean = FALSE,
                       num_cols = NULL,
                       num_rows = NULL) {
  fail_if_empty(data)

  DATA = copy(data)

  customized_colors = is_available(colors)

  if(!customized_colors) colors = factorize_colors(DATA, categorize_by)

  DATA[, YEAR := factor(YEAR, levels = max(YEAR):min(YEAR), ordered = TRUE)]

  colnames(DATA)[which(colnames(data) == categorize_by)] = "CATEGORY"

  color_mean   = darken("red",   amount = 0.3)
  color_median = darken("green", amount = 0.3)

  DATA[, CLASS := ( CLASS_LOW + CLASS_HIGH ) / 2]

  DATA = DATA[, .(FISH_COUNT = sum(FISH_COUNT)), keyby = .(YEAR, CATEGORY, CLASS)]

  DATA = DATA[, NUM_YEAR_C    := length(unique(YEAR)), by = .(CATEGORY)]
  DATA = DATA[, FISH_COUNT_C  := sum(FISH_COUNT), by = .(CATEGORY)]
  DATA = DATA[, FISH_COUNT_YC := sum(FISH_COUNT), by = .(YEAR, CATEGORY)]

  DATA = DATA[, FISH_COUNT := FISH_COUNT / FISH_COUNT_YC]

  DATA = DATA[, PROP := FISH_COUNT_YC / max(FISH_COUNT_YC), by = .(CATEGORY)]

  DATA = (DATA[rep(seq(1:nrow(DATA)), DATA$FISH_COUNT * 10000)]
          [, FISH_COUNT := NULL]
          [, NUM_YEAR_C := NULL]
          [, FISH_COUNT_C := NULL]
          [, FISH_COUNT_YC := NULL])

  if(show_samples_proportion) aesthetics = aes(x = CLASS, y = YEAR, alpha = PROP)
  else aesthetics = aes(x = CLASS, y = YEAR, alpha = 1)

  p =
    ggplot(DATA, aesthetics) +
    geom_density_ridges(size  = .5,
                        #fill = fill,
                        #color = darken(outline, amount = 0.2),
                        #quantile_lines = TRUE,
                        #quantiles = c(0.1, 0.9),
                        scale = scale,
                        rel_min_height = rel_min_height,
                        panel_scaling = FALSE)

  if(show_mean)   p = p + stat_summary(fun = mean,   color = color_mean,   alpha = 1, pch = 20)
  if(show_median) p = p + stat_summary(fun = median, color = color_median, alpha = 1, pch = 20)

  xlab = "Size"

  if(!is.na(measure_type)) xlab = measure_type
  if(!is.na(measure_unit)) xlab = paste(xlab, paste0("(", measure_unit, ")"))

  p =
    p +

    facet_wrap(~CATEGORY, ncol = num_cols, nrow = num_rows) +
               aes(fill = CATEGORY, color = CATEGORY) +
               scale_fill_manual(values = colors$FILL) +
               scale_color_manual(values = colors$OUTLINE) +

    theme_bw() +
    theme(strip.background = element_rect(fill = "white"),
          panel.spacing = unit(2, "lines"),
          legend.position = "top") +

    scale_x_continuous(expand = c(0   , 0),    name = xlab) +
    scale_y_discrete  (expand = c(0.01, 0.01), name = "Year") +
    guides(fill = guide_none(), color = guide_none())

  if(show_samples_proportion) {
    p =
      p +

      scale_alpha_continuous(name = "Relative samples proportion (by category)",
                             labels = function(x) { paste0(format(x * 100, scientific = FALSE), "%") })
  } else {
    p =
      p +

      scale_alpha_continuous(range = c(0, 1), guide = "none")
  }

  return (p)
}
