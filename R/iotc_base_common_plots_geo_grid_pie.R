#'Implementation of gridded, georeferenced piemaps with ggplot2 and scatterpie libs.
#'@param data The data (shall contain a \code{C_FISHING_GROUND_CODE} column)
#'@param value The name of the column (in \code{data}) that contains the value to plot
#'@param fill_by The name of the column (in \code{data} that contains the categories)
#'@param yearly_average If \code{TRUE} uses the yearly average to plot the pies (assuming that the input data contains a \code{C_YEAR} column)
#'@param reference_value The reference value for the standard pie. If not specified, it will be assumed to equal the 99\% of all values
#'@param unit The value unit (for display purposes)
#'@param colors The colors to assign to each \code{fill_by} category. If not specified, it is determined automatically from \code{fill_by} (as long as this is one of the standard categories for which a color mapping exists)
#'@param max_categories The maximum number of \code{fill_by} categories to display. When specified, categories are sorted by their total value (in \code{data})) and only the first \code{max_categories} are kept. Everything else is assigned to a generic '\code{All other}' category.
#'@param opacity Sets the opacity of the pies appearing in the plot (defaults to 1, i.e. fully opaque)
#'@param fixed_radius Sets a fixed value for the radius of the pies (in degrees). If no \code{opacity} is specified, then the \code{opacity} of the pie will depend on its relative total value
#'@param user_centroid When \code{TRUE}, pies are centered in the centroid location of the fishing ground they belong to. This might markedly differ from the default center of a regular grid when this contains a sensible proportion of land mass
#'@param xlim The map horizontal limits (as a pair of longitude coordinates)
#'@param ylim The map vertical limits (as a pair of latitude coordinates)
#'@param show_IO_areas when \code{TRUE}, the layers for F51 and F57 are plotted on the map
#'@param show_EEZs When \code{TRUE}, the overall EEZ layer is plotted on the map (not showing explicit distinctions among different EEZs)
#'@param show_high_seas When \code{TRUE}, the overall high seas layer is plotted on the map
#'@param standard_grid Transforms the input data in order to only use grids of the provided \code{standard_grid} type (one among \{ \code{grid_1x1}, \code{grid_5x5}, \code{grid_10x10}, \code{grid_10x20}, \code{grid_20x20}, \code{grid_30x30} \}). The transformation is based on pre-calculated mappings (see \code{[IOTCStatistics].[dbo].[CL_FISHING_GROUND_AGGREGATIONS]}) and might lead to data loss if no mapping exists for some of the input grid codes.
#'@param legend_title The title to use for the legend (none if \code{NULL}). Defaults to the value provided for \code{VALUE}
#'@param show_scatterpie_legend When \code{TRUE}, shows the scatterpie legend in the bottom-left corner of the map
#'@param trim_labels If \code{TRUE} trims all category labels to a maximum of 24 characters
#'@return The \code{ggplot2} object representing the plot
#'@export
geo_grid_piemap = function(data,
                           value,
                           fill_by,
                           yearly_average = TRUE,
                           max_categories = NA,
                           reference_value = NA,
                           unit = "t", #Default unit for the plotted value
                           colors = NA,
                           opacity = NA,
                           fixed_radius = NA,
                           use_centroid = TRUE,
                           xlim = IO_map_xlim,
                           ylim = IO_map_ylim,
                           show_IO_areas = TRUE,
                           show_EEZs = FALSE,
                           show_high_seas = FALSE,
                           standard_grid = grid_5x5,
                           legend_title = NULL,
                           show_scatterpie_legend = TRUE,
                           trim_labels = TRUE) {
  fail_if_empty(data)

  if(yearly_average & !(C_YEAR %in% colnames(data))) {
    stop("Cannot calculate yearly average as the data does not include a 'YEAR' column")
  }

  user_defined_colors = is_available(colors)

  if(!user_defined_colors) colors = setDT(factorize_colors(data, fill_by))

  colnames(data)[which(colnames(data) == fill_by)] = "FILL_BY"
  colnames(data)[which(colnames(data) == value)]   = "VALUE"

  colnames(colors)[which(colnames(colors) == fill_by)] = "FILL_BY"

  data$FISHING_GROUND_CODE = trimws(data$FISHING_GROUND_CODE, which = "both")

  if(yearly_average) {
    unit = paste0(unit, " / year")
    years = max(data$YEAR) - min(data$YEAR) + 1

    data = data[, VALUE := VALUE / years]
  }

  data = data[, .(VALUE = sum(VALUE)), keyby = .(FILL_BY, FISHING_GROUND_CODE)]

  if(!is.na(max_categories)) {
    reduced = shrink_categories_geo(data, colors, max_categories, user_defined_colors)

    data = reduced$data
    colors = reduced$colors
  }

  #When required, performs the conversion of input grid codes to standardized ones
  if(!is.na(standard_grid)) data = spatially_disaggregate_geo(data, standard_grid)

  categories = as.character(sort(unique(data$FILL_BY)))

  if(trim_labels) { labels = unlist(lapply(categories, strlen_max_labels)) }
  else labels = categories

  num_categories = length(categories)

  #When num_cagories == 1, the geom_scatterpie function returns an error.
  #For this reason, we create a dummy entry (and its accompanying color) that
  #allows geom_scatterpie to run without errors, and then we remove the dummy
  #entry from the legend (see below)
  if(num_categories == 1) {
    data = rbind(data, data.table(FILL_BY = "_DUMMY_",
                                  FISHING_GROUND_CODE = data[1]$FISHING_GROUND_CODE,
                                  VALUE = 0.0))

    categories = as.character(sort(unique(data$FILL_BY)))
    labels     = unlist(lapply(categories, strlen_max_labels))

    extra_color = colors[1]
    extra_color$FILL_BY = "_DUMMY_"
    extra_color$FILL    = "#00000000"
    extra_color$OUTLINE = "#00000000"

    if(length(extra_color) == 4)
      extra_color[, 4]    = "_DUMMY_"

    colors = rbind(colors, extra_color)
  }

  pmap = prepare_map_data(data,
                          categories,
                          reference_value,
                          fixed_radius,
                          opacity,
                          use_centroid)

  #Following 'if' statement already appears in 'prepare_map_data' but is also needed here to
  #ensure the code works properly. The 'prepare_map_data' function should be further factorized
  #to remove this HORRIBLE duplication... Maybe later on.

  #In lack of an explicit reference value, sets it to the 99% quantile of all total values
  if(is.na(reference_value))
    reference_value = round(quantile(pmap$TOTAL, .99)[1][[1]])

  fix_radius  = !is.na(fixed_radius)
  fix_opacity = !is.na(opacity)

  map =
    IO_map(
      xlim = xlim,
      ylim = ylim,
      show_IO_areas = show_IO_areas,
      show_EEZs = show_EEZs,
      show_high_seas = show_high_seas) +

    #The 'limits' parameter is used to remove the dummy category when the original dataset only has one proper category
    #See also: https://community.rstudio.com/t/how-keep-aesthetic-mapping-but-remove-a-specific-item-from-legend-with-ggplot/52818/3
    scale_fill_manual (values = colors$FILL, labels = labels, limits = colors[FILL_BY != "_DUMMY_"]$FILL_BY) +
    scale_color_manual(values = colors$OUTLINE,               limits = colors[FILL_BY != "_DUMMY_"]$FILL_BY)

  if(!fix_radius) { # Radius is not fixed (it will be proportional to the VALUE in that grid)
    legend_coords = legend_coords_for(xlim, ylim)

    map = map +
      geom_scatterpie(
        aes(x = LON_X,
            y = LAT_X,
            r = RADIUS,
            group = FISHING_GROUND_CODE),
        data = pmap,
        cols = categories,
        alpha = ifelse(fix_opacity, opacity, 1),
        #sorted_by_radius = TRUE slows down the plotting quite sensibly, but ensures that
        #pies are plotted in descending radius order
        sorted_by_radius = FALSE
      )

    if(show_scatterpie_legend)
      map = map +
        geom_scatterpie_legend(
          pmap$RADIUS,
          x = legend_coords[1],
          y = legend_coords[2],
          n = 3,
          labeller = function(x) { paste(prettyNum(round((x / 4) ^ 2 * reference_value), big.mark=','), unit) }
        )
  } else { # Radius is fixed
    if(fix_opacity) { # Opacity is also fixed, it has to be set through the 'alpha' parameter
      map = map +
        geom_scatterpie(
          aes(x = LON_X,
              y = LAT_X,
              r = RADIUS,
              group = FISHING_GROUND_CODE),
          data = pmap,
          cols = categories,
          alpha = opacity,
          sorted_by_radius = FALSE
        )
    } else {
      # Opacity is not fixed, it has to be set through the 'aesthetics', using the relative radius.
      # But there's a problem here, as the geom_scatterpie by default applies and opacity
      # of 0.7 (or something like that) that affects the 'alpha' param provided to the
      # aesthetics, so in the end there will never be any pie with 100% opacity in the
      # final result.

      # Setting 'alpha' as one of the geom_scatterpie params overrides the 'alpha'
      # aesthetics completely, so the results we're getting now are less than optimal.
      map = map +
        geom_scatterpie(
          aes(x = LON_X,
              y = LAT_X,
              r = RADIUS,
              alpha = RADIUS_REL,
              group = FISHING_GROUND_CODE),
          data = pmap,
          cols = categories,
          sorted_by_radius = FALSE
        )
    }
  }

  map = map +
    guides(alpha = guide_none(),
           fill  = guide_legend(
             title = ifelse(is_available(legend_title), legend_title, fill_by)
           ))

  return(map)
}

prepare_map_data = function(data, categories, reference_value, fixed_radius, opacity, use_centroid) {
  num_categories = length(categories)

  pmap = dcast.data.table(data,
                          FISHING_GROUND_CODE ~ FILL_BY,
                          fill = 0.0001,
                          value.var = 'VALUE')

  pmap[, TOTAL := apply(pmap[ , -c('FISHING_GROUND_CODE')],
                        1,
                        sum,
                        na.rm=TRUE)]

  grid_codes = unique(pmap$FISHING_GROUND_CODE)

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

  pmap = merge(pmap, grids, by='FISHING_GROUND_CODE')

  #In lack of an explicit reference value, sets it to the 99% quantile of all total values
  if(is.na(reference_value))
    reference_value = round(quantile(pmap$TOTAL, .99)[1][[1]])

  #Normalizes the radius with respect to the reference value
  pmap[, RADIUS := TOTAL / reference_value]

  #Further reduces the radius to a manageable value
  pmap[, RADIUS := 4 * sqrt(RADIUS)]

  #Useless, unfortunately
  pmap = pmap[order(-TOTAL), ]

  fix_radius  = !is.na(fixed_radius)
  fix_opacity = !is.na(opacity)

  if(fix_radius) {
    MAX_RADIUS = max(pmap$RADIUS)

    #The actual opacity will be a fixed 30% + 70% (at max) of the relative radius
    pmap[, RADIUS_REL := .3 + .7 * (RADIUS / MAX_RADIUS)]
    pmap[, RADIUS := fixed_radius]
  }

  #Only needed if sorted_by_radius = TRUE (to fix a bug in the geom_scatterpie code...)
  pmap[, r := RADIUS]

  if(!use_centroid) {
    pmap$LAT_X = pmap$LAT
    pmap$LON_X = pmap$LON
  }

  return(pmap)
}

legend_coords_for = function(xlim, ylim) {
  x = xlim[1]; X = xlim[2]
  y = ylim[1]; Y = ylim[2]

  return(c( round(x + ( X - x ) * 0.08), round(Y - ( Y - y ) * 0.92 )))
}

#'Alias for \code{\link{geo_grid_piemap}}
#'@export
geo.grid.pie = geo_grid_piemap

