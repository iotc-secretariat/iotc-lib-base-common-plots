#'Hexbin plot of length / value pairs
#'@param data The source data that shall contain a \code{LENGTH} and a \code{WEIGHT} column
#'@param facet_by The data column to use to produce a faceted plot. Defaults to \code{SPECIES_CODE}
#'@param swap_axis If \code{TRUE} weights will be mapped to the X-axis and lengths to the Y-axis
#'@param bins The number of bins in both vertical and horizontal directions (see also \code{\link{geom_hex}})
#'@param num_breaks The number of breaks to use for the scale
#'@param colors An alternative palette with as many colors as the \code{num_breaks}
#'@examples lw_hexbin(ros.LW(species_codes = "BET"))
#'@examples lw_hexbin(ros.LW(species_codes = "BET"), facet_by = FLEET_CODE)
#'@export
lw_hexbin = function(data,
                     facet_by = SPECIES_CODE,
                     swap_axis = FALSE,
                     bins = 75,
                     num_breaks = 5,
                     colors = NA) {

  fail_if_empty(data)

  colnames(data)[which(colnames(data) == facet_by)] = "FACET_BY"

  aesthetics = aes(x = LENGTH, y = WEIGHT)

  if(swap_axis) aesthetics = aes(x = WEIGHT, y = LENGTH)

  p = initialize_plot(data, aesthetics) +
      theme(legend.position = "right",
            legend.title    = element_text(size = 9),
            strip.background= element_rect(fill = "white"),
            panel.spacing   = unit(2, "lines"))

  if(is.na(colors)) {
    if(num_breaks > 11) {
      num_breaks = 11

      warning("Setting the number of breaks to 10, as this is the maximum number allowed when using the default color scale")
    }

    colors = brewer.pal(name = "RdBu", n = 11)
  }

  p = p +
    geom_hex(bins = bins) +

    scale_fill_stepsn(name = "Num. samples",
                      n.breaks = num_breaks,
                      show.limits = TRUE,
                      colors = colors)

  if(swap_axis) p = p + xlab("Weight (kg)") + ylab("Length (cm)")
  else p = p + xlab("Length (cm)") + ylab("Weight (kg)")

  return (p + facet_wrap(~FACET_BY))
}

#'X-Y plot of length / value pairs
#'@param data The source data that shall contain a \code{LENGTH} and a \code{WEIGHT} column
#'@param categorize_by The data column that contains the categories for each series. Defaults to \code{SPECIES_CODE}
#'@param swap_axis If \code{TRUE} weights will be mapped to the X-axis and lengths to the Y-axis
#'@param jitter_x \code{TRUE} to add X-axis jittering to the plots
#'@param jitter_y \code{FALSE} to add Y-axis jittering to the plots
#'@param colors An alternative palette with as many colors as the \code{num_breaks}
#'@param alpha The transparency value to use when plotting the points
#'@examples lw_plot(ros.LW(species_codes = "BET"))
#'@examples lw_plot(ros.LW(species_codes = "BET"), categorize_by = FLEET_CODE)
#'@examples lw_plot(ros.LW(species_codes = "BET"), categorize_by = LENGTH_MEASURE_TYPE_CODE, colors = color_table(unique_colors(16)))
#'@export
lw_plot = function(data,
                   categorize_by = SPECIES_CODE,
                   swap_axis = FALSE,
                   jitter_x = TRUE,
                   jitter_y = FALSE,
                   colors = NA,
                   alpha = .7) {
  fail_if_empty(data)

  customized_colors = is_available(colors)

  if(!customized_colors) colors = factorize_colors(data, categorize_by)

  colnames(data)[which(colnames(data)     == categorize_by)] = "CATEGORIZE_BY"
  colnames(colors)[which(colnames(colors) == categorize_by)] = "CATEGORIZE_BY"

  data2 = data[, .(COUNT = .N), keyby = .(CATEGORIZE_BY)]

  categories = as.character(sort(unique(data$CATEGORIZE_BY)))
  labels     = unlist(lapply(categories, strlen_max_labels))

  aesthetics = aes(x = jitter(LENGTH, factor = ifelse(jitter_x, 1, 0)),
                   y = jitter(WEIGHT, factor = ifelse(jitter_y, 1, 0)),
                   group = CATEGORIZE_BY)

  if(swap_axis) aesthetics = aes(x = jitter(WEIGHT, factor = ifelse(jitter_x, 1, 0)),
                                 y = jitter(LENGTH, factor = ifelse(jitter_y, 1, 0)),
                                 group = CATEGORIZE_BY)

  p = initialize_plot(data, aesthetics)

  p = p +
    geom_point(aes(color = CATEGORIZE_BY)) +
    scale_color_manual(values = alpha(colors$OUTLINE, alpha),
                       labels = paste0(data2$CATEGORIZE_BY, " (n=", format(data2$COUNT, big.mark=","), ")")
    ) + guides(color = guide_legend(override.aes = list(alpha = 1)))

  if(swap_axis) {
    p = p + xlab(paste0("Weight (", paste0(unique(data$WEIGHT_MEASURE_UNIT_CODE), collapse = " ,"), ")")) +
            ylab(paste0("Length (", paste0(unique(data$LENGTH_MEASURE_UNIT_CODE), collapse = " ,"), ")"))
  } else {
    p = p + xlab(paste0("Length (", paste0(unique(data$LENGTH_MEASURE_UNIT_CODE), collapse = " ,"), ")")) +
            ylab(paste0("Weight (", paste0(unique(data$WEIGHT_MEASURE_UNIT_CODE), collapse = " ,"), ")"))
  }

  return (p)
}

#'Computes the smoothed conditional means of the original L-W data for a given X-Y LW plot
#'@param plot The original L-W plot (produced with \code{\link{lw_plot}})
#'@param show_ci Whether or not display the confidence interval around the computed means (defaults to \code{TRUE})
#'@param level The level of confidence interval to use (defaults to \code{0.95})
#'@examples lw_smooth(lw_plot(ros.LW(species_codes = "BET", length_measure_type_codes = c("EF", "FL", "TL", "PC")), categorize_by = LENGTH_MEASURE_TYPE_CODE, colors = color_table(unique_colors(16))))
#'@export
lw_smooth = function(plot, show_ci = TRUE, level = 0.95) {
  return(
    plot +
      geom_smooth(
        inherit.aes = TRUE,
        se = show_ci,
        level = level,
        color = "black"
      )
  )
}

#'Alias for \code{\link{lw_hexbin}}
#'@export
LW.bin.hex = lw_hexbin

#'Alias for \code{\link{lw_plot}}
#'@export
LW.plot = lw_plot

#'Alias for \code{\link{lw_smooth}}
#'@export
LW.smooth = lw_smooth
