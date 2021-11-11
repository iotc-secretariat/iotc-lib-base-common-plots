#'Implementation of a contour plot based on a set of given georeferenced observations aggregated over regular grids.
#'@param data The data (shall contain a \code{FISHING_GROUND_CODE} column)
#'@param value The name of the column (in \code{data}) that contains the value identifying the number of observations in the grid
#'@param categorize_by The name of the column (in \code{data} that contains the categories)
#'@param scale A multiplicative factor to forcibly increase the number of observations
#'@param contour_var The variable to use to determine the contour of the regions. One among \{ \code{"count"}, \code{"density"}, \code{"ndensity"} \}. See also \code{ggplot2::stat_density2d_filled}
#'@param colors The colors to assign to each \code{fill_by} category. If not specified, it is determined automatically from \code{fill_by} (as long as this is one of the standard categories for which a color mapping exists)
#'@param max_categories The maximum number of \code{fill_by} categories to display. When specified, categories are sorted by their total value (in \code{data})) and only the first \code{max_categories} are kept. Everything else is assigned to a generic '\code{All other}' category.
#'@param uniform_fill When \code{TRUE} each area will be colored uniformly, otherwise its coloring will be proportional to the density
#'@param use_centroid When \code{TRUE}, pies are centered in the centroid location of the fishing ground they belong to. This might markedly differ from the default center of a regular grid when this contains a sensible proportion of land mass
#'@param xlim The map horizontal limits (as a pair of longitude coordinates)
#'@param ylim The map vertical limits (as a pair of latitude coordinates)
#'@param show_IO when \code{TRUE}, the layers for F51 and F57 are plotted on the map
#'@param show_EEZs When \code{TRUE}, the overall EEZ layer is plotted on the map (not showing explicit distinctions among different EEZs)
#'@param show_high_seas When \code{TRUE}, the overall high seas layer is plotted on the map
#'@param standard_grid Transforms the input data in order to only use grids of the provided \code{standard_grid} type (one among \{ \code{grid_1x1}, \code{grid_5x5}, \code{grid_10x10}, \code{grid_10x20}, \code{grid_20x20}, \code{grid_30x30} \}). The transformation is based on pre-calculated mappings (see \code{[IOTCStatistics].[dbo].[CL_FISHING_GROUND_AGGREGATIONS]}) and might lead to data loss if no mapping exists for some of the input grid codes.
#'@param legend_title The title to use for the legend (none if \code{NULL}). Defaults to the value provided for \code{FILL_BY}
#'@return The \code{ggplot2} object representing the plot
#'@examples geo_grid_contourmap(ros.SETS(fishery_group_codes = "LL"), FLEET_CODE, START_LON, START_LAT)
#'@examples geo_grid_contourmap(ros.SETS(fishery_group_codes = "LL"), FLEET_CODE, uniform_fill = TRUE)
#'@examples geo_grid_contourmap(ros.SETS(fishery_group_codes = "LL"), FLEET_CODE, contour_var = "count",  uniform_fill = TRUE)
#'@export
geo_grid_contourmap = function(data,
                               value,
                               categorize_by,
                               scale = .01,
                               contour_var = "ndensity",
                               colors = NA,
                               max_categories = NA,
                               uniform_fill = FALSE,
                               use_centroid = TRUE,
                               xlim = IO_map_xlim,
                               ylim = IO_map_ylim,
                               show_IO = TRUE,
                               show_EEZs = FALSE,
                               show_high_seas = FALSE,
                               standard_grid = grid_5x5,
                               legend_title = NULL) {
  fail_if_empty(data)

  user_defined_colors = is_available(colors)

  if(!user_defined_colors) colors = factorize_colors(data, categorize_by)

  colnames(data)[which(colnames(data) == categorize_by)] = "FILL_BY"
  colnames(data)[which(colnames(data) == value)]         = "VALUE"

  colnames(colors)[which(colnames(colors) == categorize_by)] = "FILL_BY"

  data$FISHING_GROUND_CODE = trimws(data$FISHING_GROUND_CODE, which = "both")

  data = data[, .(VALUE = sum(VALUE)), keyby = .(FILL_BY,  FISHING_GROUND_CODE)]

  if(!is.na(max_categories)) {
    reduced = shrink_categories_geo(data, colors, max_categories, user_defined_colors)

    data = reduced$data
    colors = reduced$colors
  }

  #When required, performs the conversion of input grid codes to standardized ones
  if(!is.na(standard_grid)) data = spatially_disaggregate_geo(data, standard_grid)

  categories = as.character(sort(unique(data$FILL_BY)))
  labels     = unlist(lapply(categories, strlen_max_labels))

  data =
    prepare_map_data_contour(
      data,
      categories,
      use_centroid,
      scale
    )

  # See: https://stackoverflow.com/questions/53075331/error-using-geom-density-2d-in-r-computation-failed-in-stat-density2d-b
  bandwidth = c(MASS::bandwidth.nrd(data$LON_X), MASS::bandwidth.nrd(data$LAT_X))

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
                x = LON_X,
                y = LAT_X,
                group = FILL_BY,
                fill  = FILL_BY,
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
                x = LON_X,
                y = LAT_X,
                group = FILL_BY,
                fill  = FILL_BY,
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

prepare_map_data_contour = function(data, categories, use_centroid, scale) {
  num_categories = length(categories)

  # The intensity is proportional to the SQRT of the value
  #data$ALPHA = round(data$VALUE) * scale

  max_value = max(data$VALUE)

  data$ALPHA = round(data$VALUE / max_value * scale)

  #data$ALPHA = round(data$VALUE)

  # Alternative version: the intensity is the log of the proportion between a value and the maximum for that category (FILL_BY)
  # data = data[, ALPHA := 1 + max(0, log10(VALUE) / log10(max(VALUE))), by = FILL_BY]
  # data[is.na(ALPHA)] = NULL

  grid_codes = unique(data$FISHING_GROUND_CODE)

  #Same as above
  grids = filter_grids()[trimws(FISHING_GROUND_CODE) %in% grid_codes]

  ngrid_codes = length(grid_codes)
  ngrids      = nrow(grids)

  if(ngrid_codes < ngrids)
    warning(
      paste("Issue detected:",
            ngrid_codes,
            "unique grid codes in the original data were mapped on",
            ngrids,
            "regular grids.",
            "Potential data loss when attempting to plot the data")
    )

  data = merge(data, grids, by="FISHING_GROUND_CODE", allow.cartesian=TRUE)

  if(!use_centroid) {
    data$LAT_X = data$LAT
    data$LON_X = data$LON
  }

  #Need to repeat rows at least once to avoid dropping records with an ALPHA * scale < 1
  #data = (data[rep(seq(1:nrow(data)), max(round(data$ALPHA * scale), 1))])
  data = (data[rep(seq(1:nrow(data)), max(data$ALPHA, 1))])

  cols = c("FILL_BY", "LAT_X", "LON_X")

  data = data[, ..cols]

  return(data)
}

#'Alias for \code{\link{geo_grid_contourmap}}
#'@export
geo.grid.contour = geo_grid_contourmap
