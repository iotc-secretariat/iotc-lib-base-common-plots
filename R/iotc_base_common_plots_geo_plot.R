#'Implementation of a geospatial plot with ggplot2
#'@param data The data (shall contain a \code{FISHING_GROUND_CODE} column)
#'@param categorize_by The name of the column (in \code{data}) that contains the categories
#'@param longitude The name of the column (in \code{data}) that contains the \code{LONGITUDE} information
#'@param latitude The name of the column (in \code{data}) that contains the \code{LATITUDE} information
#'@param xlim The map horizontal limits (as a pair of longitude coordinates)
#'@param ylim The map vertical limits (as a pair of latitude coordinates)
#'@param show_EEZs When \code{TRUE}, the overall EEZ layer is plotted on the map (not showing explicit distinctions among different EEZs)
#'@param show_high_seas When \code{TRUE}, the overall high seas layer is plotted on the map
#'@param colors The colors to assign to each \code{fill_by} category. If not specified, it is determined automatically from \code{fill_by} (as long as this is one of the standard categories for which a color mapping exists)
#'@param alpha The transparency value to use when plotting the points
#'@param legend_title The title to use for the legend (none if \code{NULL}). Defaults to the value provided for \code{FILL_BY}
#'@return The \code{ggplot2} object representing the plot
#'@export
#'@examples geo_plot(ros.raw.SETS(years = 2018), FISHERY_GROUP_CODE, START_LON, START_LAT)
#'geo_plot(ros.)
geo_plot = function(data,
                    categorize_by,
                    longitude,
                    latitude,
                    xlim = IO_map_xlim,
                    ylim = IO_map_ylim,
                    show_EEZs = FALSE,
                    show_high_seas = FALSE,
                    colors = NA,
                    alpha = .7,
                    legend_title = NULL) {
  fail_if_empty(data)

  user_defined_colors = is_available(colors)

  if(!user_defined_colors) colors = factorize_colors(data, categorize_by)

  colnames(data)[which(colnames(data) == longitude)] = "LON"
  colnames(data)[which(colnames(data) == latitude)]  = "LAT"
  colnames(data)[which(colnames(data) == categorize_by)] = "CATEGORIZE_BY"

  colnames(colors)[which(colnames(colors) == categorize_by)] = "CATEGORIZE_BY"

  categories = as.character(sort(unique(data$CATEGORIZE_BY)))
  labels     = unlist(lapply(categories, strlen_max_labels))

  map =
    IO_map(
      xlim = xlim,
      ylim = ylim,
      show_EEZs = show_EEZs,
      show_high_seas = show_high_seas,
      content_drawer = function(map) {
        map = map +
          geom_point(
            data = data,
            mapping = aes(
              x = LON,
              y = LAT,
              group = CATEGORIZE_BY,
              color = CATEGORIZE_BY
            )
          ) +
          scale_color_manual(values = alpha(colors$FILL, alpha), labels = labels)
      }
    )

  map = map +
    guides(alpha = guide_none(),
           color = guide_legend(
             title = ifelse(is_available(legend_title), legend_title, categorize_by),
             override.aes = list(alpha = 1)
           )
    )

  return(map)
}

#'Alias for \code{\link{geo_plot}}
#'@export
geo.plot = geo_plot
