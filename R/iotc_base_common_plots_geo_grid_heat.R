#'Implementation of georeferenced, gridded heatmaps with ggplot2.
#'@param data The data (shall contain a \code{C_FISHING_GROUND_CODE} column)
#'@param value The name of the column (in \code{data}) that contains the value to plot
#'@param yearly_average If \code{TRUE} uses the yearly average to plot the pies (assuming that the input data contains a \code{C_YEAR} column)
#'@param num_intervals The number of intervals ('breaks') to split the range of values into. The result will be calculated as the quantiles with probabilities increasing by \code{with n * 1 / num_intervals} at each step.
#'@param unit The value unit (for display purposes)
#'@param breaks Externally provided breaks. Alternative to \code{num_intervals}
#'@param show_grid_borders when \code{TRUE}, each regular grid will have a darker border around it
#'@param show_IO when \code{TRUE}, the layers for F51 and F57 are plotted on the map
#'@param show_EEZs When \code{TRUE}, the overall EEZ layer is plotted on the map (not showing explicit distinctions among different EEZs)
#'@param show_high_seas When \code{TRUE}, the overall high seas layer is plotted on the map
#'@param standard_grid Transforms the input data in order to only use grids of the provided \code{standard_grid} type (one among \{ \code{grid_1x1}, \code{grid_5x5}, \code{grid_10x10}, \code{grid_10x20}, \code{grid_20x20}, \code{grid_30x30} \}). The transformation is based on pre-calculated mappings (see \code{[IOTCStatistics].[dbo].[CL_FISHING_GROUND_AGGREGATIONS]}) and might lead to data loss if no mapping exists for some of the input grid codes.
#'@param palette A palette function capable of returning \code{n} colors. Defaults to \code{colorspace::heat_hcl}
#'@param legend_title The title to use for the legend (none if \code{NULL}). Defaults to the value provided for \code{VALUE}
#'@return The \code{ggplot2} object representing the plot
#'@export
geo_grid_heatmap = function(data,
                            value,
                            yearly_average = TRUE,
                            num_intervals = 5,
                            unit = "t", #Default unit for the plotted value
                            breaks = NA,
                            show_grid_borders = TRUE,
                            xlim = IO_map_xlim,
                            ylim = IO_map_ylim,
                            show_IO = TRUE,
                            show_EEZs = FALSE,
                            show_high_seas = FALSE,
                            standard_grid = grid_5x5,
                            palette = colorspace::heat_hcl,
                            legend_title = NULL) {
  fail_if_empty(data)

  if(yearly_average & !(C_YEAR %in% colnames(data))) {
    stop("Cannot calculate yearly average as the data does not include a 'YEAR' column")
  }

  colnames(data)[which(colnames(data) == value)]   = "VALUE"

  data$FISHING_GROUND_CODE = trimws(data$FISHING_GROUND_CODE, which = "both")

  if(yearly_average) {
    unit = paste0(unit, " / year")
    years = max(data$YEAR) - min(data$YEAR) + 1

    data = data[, VALUE := VALUE / years]
  }

  data = data[, .(VALUE = sum(VALUE)), keyby = .(FISHING_GROUND_CODE)]

  #Performs the conversion of input grid codes to standardized ones
  if(!is.na(standard_grid))
    data = spatially_disaggregate_heatmap(data, standard_grid)

  FGs = sf_by_code(unique(data$FISHING_GROUND_CODE))

  FGs = merge(x = FGs, y = data, by.x = "CODE", by.y = "FISHING_GROUND_CODE")

  #Orders by decreasing fishing ground area, to ensure that overlapping grids / features
  #are shown from the largest to the smallest
  FGs = FGs[order(-FGs$OCEAN_AREA_SURFACE_KM2), ]

  if(!is_available(breaks))
    break_values = create_breaks(FGs$VALUE, num_intervals)
  else {
    break_values = breaks

    num_intervals = length(break_values)
  }

  break_labels = labels_for_breaks(break_values)

  FGs$FILL =
    cut(
      FGs$VALUE,
      include.lowest = TRUE,
      right = FALSE,
      breaks = break_values,
      labels = break_labels
    )

  map =
    IO_map(
      xlim = xlim,
      ylim = ylim,
      show_IO = show_IO,
      show_EEZs = show_EEZs,
      show_high_seas = show_high_seas,
      draw_content_first = TRUE,
      content_drawer = function(map) {
        suppressWarnings({
          fill = rev(palette(num_intervals))

          if(show_grid_borders) outline = darken(fill, .2)
          else outline = fill

          map = map +
            geom_sf(
              data = FGs,
              mapping = aes(
                x = CENTER_LAT,
                y = CENTER_LON,
                fill = FILL,
                color = FILL)
            ) +
            scale_color_manual(values = outline) +
            scale_fill_manual( values = fill)
        })
      })

  map = map +
    guides(
      color = guide_none(),
      fill = guide_legend(
        title = paste0(
          ifelse(is_available(legend_title), legend_title, value), " (", unit, ")"
        )
      )
    )

  return(map)
}

spatially_disaggregate_heatmap = function(data, standard_grid) {
  value_before = coalesce(sum(data$VALUE), 0)

  FGs = unique(data$FISHING_GROUND_CODE)

  if(!is.na(standard_grid)) {
    grid_mappings = grid_intersections_by_target_grid_type(
      FGs,
      standard_grid
    )
  } else {
    grid_mappings = data.table(SOURCE_FISHING_GROUND_CODE = FGs,
                               TARGET_FISHING_GROUND_CODE = FGs,
                               PROPORTION = 1.0)
  }


  data = merge(x = data,
               y = grid_mappings,
               all.x = FALSE,
               by.x = "FISHING_GROUND_CODE",
               by.y = "SOURCE_FISHING_GROUND_CODE",
               allow.cartesian = TRUE)

  nFGs = unique(data[is.na(TARGET_FISHING_GROUND_CODE)]$FISHING_GROUND_CODE)

  data = data[!is.na(TARGET_FISHING_GROUND_CODE)][, .(VALUE = sum(VALUE * PROPORTION)), keyby = .(TARGET_FISHING_GROUND_CODE)]

  names(data)[names(data) == "TARGET_FISHING_GROUND_CODE"] = "FISHING_GROUND_CODE"

  value_after     = coalesce(sum(data$VALUE), 0)
  value_diff      = value_after - value_before
  value_diff_perc = ifelse(value_before == 0, 0, value_diff / value_before)

  if(value_diff_perc > 0.05) {
    warning(
      paste0(
        "Difference detected between values after rearranging grids to standard ones: ",
        round(value_after), "vs.", round(value_before), ". ",
        length(nFGs), " ",
        "unmapped grid codes in original data"
      )
    )
  }

  return(data)
}

#'Creates \code{num_intervals} quantiles on the provided \code{values}
#'@param values An array of values
#'@param num_intervals The number of intervals the values should be split into
#'@return The calculated quantiles (to be used as breaks)
#'@export
create_breaks = function(values, num_intervals) {
  dp = 1.0 / num_intervals

  return(unique(quantile(values, probs = seq(0, 1, dp), na.rm = TRUE)))
}

#'Creates break labels (in the form \code{\( VALUE_FROM - VALUE_TO \]\)}) for a given set of breaks
#'@param breaks The breaks to create label for
#'@return The labels for the provided breaks
#'@export
labels_for_breaks = function(breaks) {
  labels = c()

  for(v in c(1:(length(breaks) - 1)))
    labels =
      append(
        labels,
        paste("(", prettyNum(round(breaks[v  ][[1]]), big.mark = ","),
              "-", prettyNum(round(breaks[v+1][[1]]), big.mark = ","),
              "]")
      )

  return(labels)
}

#'Alias for \code{\link{geo_grid_heatmap}}
#'@export
geo.grid.heatmap = geo_grid_heatmap
