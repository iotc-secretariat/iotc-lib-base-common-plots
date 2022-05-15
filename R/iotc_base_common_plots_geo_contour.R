#'Implementation of a contour plot based on a set of given georeferenced observations.
#'@param data The data (shall contain a \code{FISHING_GROUND_CODE} column)
#'@param categorize_by The name of the column (in \code{data} that contains the categories)
#'@param longitude The name of the column (in \code{data}) that contains the \code{LONGITUDE} information
#'@param latitude The name of the column (in \code{data}) that contains the \code{LATITUDE} information
#'@param xlim The map horizontal limits (as a pair of longitude coordinates)
#'@param ylim The map vertical limits (as a pair of latitude coordinates)
#'@param show_IO when \code{TRUE}, the layers for F51 and F57 are plotted on the map
#'@param show_EEZs When \code{TRUE}, the overall EEZ layer is plotted on the map (not showing explicit distinctions among different EEZs)
#'@param show_high_seas When \code{TRUE}, the overall high seas layer is plotted on the map
#'@param colors The colors to assign to each \code{fill_by} category. If not specified, it is determined automatically from \code{fill_by} (as long as this is one of the standard categories for which a color mapping exists)
#'@param uniform_fill When \code{TRUE} each area will be colored uniformly, otherwise its coloring will be proportional to the density
#'@param contour_var The variable to use to determine the contour of the regions. One among \{ \code{"count"}, \code{"density"}, \code{"ndensity"} \}. See also \code{ggplot2::stat_density2d_filled}
#'@param legend_title The title to use for the legend (none if \code{NULL}). Defaults to the value provided for \code{FILL_BY}
#'@param trim_labels If \code{TRUE} trims all category labels to a maximum of 24 characters
#'@return The \code{ggplot2} object representing the plot
#'@examples geo_contourmap(ros.raw.SETS(fishery_group_codes = "LL"), FLEET_CODE, START_LON, START_LAT)
#'@examples geo_contourmap(ros.raw.SETS(fishery_group_codes = "LL"), FLEET_CODE, START_LON, START_LAT, uniform_fill = TRUE)
#'@examples geo_contourmap(ros.raw.SETS(fishery_group_codes = "LL"), FLEET_CODE, START_LON, START_LAT, contour_var = "count",  uniform_fill = TRUE)
#'@export
geo_contourmap = function(data,
                          categorize_by,
                          longitude= "LON",
                          latitude = "LAT",
                          xlim = IO_map_xlim,
                          ylim = IO_map_ylim,
                          show_IO = TRUE,
                          show_EEZs = FALSE,
                          show_high_seas = FALSE,
                          colors = NA,
                          uniform_fill = FALSE,
                          contour_var = "ndensity", #Defaults to normalized density
                          legend_title = NULL,
                          trim_labels = TRUE) {
  fail_if_empty(data)

  user_defined_colors = is_available(colors)

  if(!user_defined_colors) colors = factorize_colors(data, categorize_by)

  colnames(data)[which(colnames(data) == longitude)]     = "LON"
  colnames(data)[which(colnames(data) == latitude)]      = "LAT"
  colnames(data)[which(colnames(data) == categorize_by)] = "CATEGORIZE_BY"

  data = data[!is.na(LON) & !is.na(LAT)]

  colnames(colors)[which(colnames(colors) == categorize_by)] = "CATEGORIZE_BY"

  categories = as.character(sort(unique(data$FILL_BY)))

  if(trim_labels) { labels = unlist(lapply(categories, strlen_max_labels)) }
  else labels = categories

  # See: https://stackoverflow.com/questions/53075331/error-using-geom-density-2d-in-r-computation-failed-in-stat-density2d-b
  bandwidth = c(MASS::bandwidth.nrd(data$LON), MASS::bandwidth.nrd(data$LAT))

  map =
    IO_map(
      xlim = xlim,
      ylim = ylim,
      show_IO = show_IO,
      show_EEZs = show_EEZs,
      show_high_seas = show_high_seas,
      content_drawer = function(map) {
        if(uniform_fill) {
          map = map +
            stat_density2d_filled(
              geom = "density_2d_filled",
              contour_var = contour_var,
              data = data,
              mapping = aes(
                x = LON,
                y = LAT,
                group = CATEGORIZE_BY,
                fill  = CATEGORIZE_BY,
              ),
              alpha = .5,
              size = 2,
              contour = TRUE,
              h = bandwidth
           )
        } else {
          map = map +
            stat_density2d_filled(
              geom = "density_2d_filled",
              contour_var = contour_var,
              data = data,
              mapping = aes(
                x = LON,
                y = LAT,
                group = CATEGORIZE_BY,
                fill  = CATEGORIZE_BY,
                alpha = ..level..
              ),
              size = 2,
              contour = TRUE,
              h = bandwidth
            )
        }
      }
    ) +
    scale_fill_manual(values = colors$FILL, labels = labels)

  map = map +
    guides(alpha = guide_none(),
           fill  = guide_legend(
             title = ifelse(is_available(legend_title), legend_title, categorize_by)
           )
    )

  return(map)
}

#'Alias for \code{\link{geo_contourmap}}
#'@export
geo.contour = geo_contourmap
