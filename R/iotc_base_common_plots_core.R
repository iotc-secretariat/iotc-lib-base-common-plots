calculate_delta = function(min, max) {
  #Magnitude of the max catch by label value
  magnitude = round(log10(max))

  #Delta corresponds to the previous order magnitude

  #delta = max(10, 10^(magnitude - 1))
  delta = 10^(magnitude - 1)

  return(delta)
}

calculate_limit = function(min, max) {
  delta = calculate_delta(min, max)

  #Calculates the upper range of the Y axis, rounded to the closest multiple of the magnitude
  return (ceiling(max / delta) * delta)
}

breaks_for = function(min, max) {
  maxx  = calculate_limit(min, max)
  delta = calculate_delta(min, max)

  #The step is twice as wide as the delta
  step =  2 * delta

  #return (c(seq(0, maximum_value, ifelse(magnitude == 3, 200, 50)), round(maximum_value)))
  return (c(seq(0, maxx, step), maxx))
}

limit_for = function(min, max) {
  breaks = breaks_for(min, max)

  return(c(min(breaks), max(breaks)))
  #return (c(0, calculate_limit(min, max) * 1.1))
}


calculate_legend_rows = function(number_of_categories) {
  return (max(1, (floor(number_of_categories / 4))))
}

initialize_species_colors_by_category = function(category, species, amount = 0.2) {
  BASE_COLOR = copy(colors_for_species_category(category))

  species_for_category = species[SPECIES_CATEGORY_CODE == category & !is.na(SORT)][order(+SORT)]
  species_for_category = data.table(SORT = unique(species_for_category$SORT), CODE = unique(species_for_category$CODE))

  species_for_category = species_for_category[order(-SORT)]

  fill = darken(BASE_COLOR$FILL, amount = amount)

  for(r in 1:nrow(species_for_category)) {
    record = species_for_category[r]

    add_species_colors(record$CODE, fill)

    fill = lighten(fill, amount = amount)
  }
}

initialize_species_colors = function(category, species) {
  species_for_category = species[SPECIES_CATEGORY_CODE == category]

  colors = unique_colors(nrow(species_for_category))

  for(r in 1:nrow(species_for_category)) {
    record = species_for_category[r]

    add_species_colors(record$CODE, colors[r])

    fill = lighten(colors[r], amount = 0.15)
  }
}

#'Initializes the species colors using the species category code as a basis
#'@param connection A connection to \code{\link{IOTDB}}
#'@export
initialize_all_species_colors = function(connection = DB_IOTDB()) {
  reset_species_colors()

  species = if(!is.null(connection)){
    all_species(connection)
  }else{
    iotc.data.reference.codelists::LEGACY_SPECIES_IOTDB
  }

  initialize_species_colors_by_category(SC_BILLFISH, species)
  initialize_species_colors_by_category(SC_SEERFISH, species)
  initialize_species_colors_by_category(SC_NERITIC,  species)
  initialize_species_colors_by_category(SC_TEMPERATE,species)
  initialize_species_colors_by_category(SC_TROPICAL, species, amount = 0.4)
  initialize_species_colors_by_category(SC_TUNAS_NEI,species)
  initialize_species_colors_by_category(SC_SHARKS,   species)
  initialize_species_colors_by_category(SC_RAYS,     species)

  initialize_species_colors(SC_SEABIRDS,  species)
  initialize_species_colors(SC_CETACEANS, species)
  initialize_species_colors(SC_TURTLES,   species)
}

#'Initializes the gear colors
#'@param connection A connection to \code{\link{IOTDB}}
#'@export
initialize_all_gears_colors = function(connection = DB_IOTDB()) {
  reset_gear_colors()

  gears  = if(!is.null(connection)){
    all_codes("GEARS", connection)[USED == TRUE][order(+SORT)]
  }else{
    iotc.data.reference.codelists::LEGACY_GEARS_IOTDB
  }
  colors = unique_colors(nrow(gears))

  for(r in 1:nrow(gears)) {
    record = gears[r]

    add_gear_colors(record$CODE, colors[r])

    fill = lighten(colors[r], amount = 0.15)
  }
}

initialize_plot = function(data, aesthetics = NA, custom_theme = theme_bw) {
  p = NA

  if(anyNA(aesthetics)) {
    p = ggplot(data)
  } else {
    p = ggplot(data, aesthetics)
  }

  p =
    p +

    custom_theme() +

    theme(legend.title = element_blank(),
          legend.key = element_blank(),
          legend.background = element_blank(),
          legend.text = element_text(size = 9),
          legend.position = "top",
          legend.key.width  = unit(.4, "cm"),
          legend.key.height = unit(.4, "cm"),
          axis.text.x = element_text(size = 9)) +

    theme(plot.margin = unit(c(5.5, 5.5, 5.5, 5.5), "points")) +

    theme(#panel.grid.minor.x = element_line(size = 0.3, color = "lightgrey")
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_line(size = 0.3, color = "lightgrey"),
          panel.grid.minor.y = element_line(size = 0.3, color = "lightgrey", linetype = "dashed"),
          panel.grid.major.y = element_line(size = 0.3, color = "lightgrey"))

  return (p)
}

#'Saves a (gg)plot with standard width and height
#'@param filename The name of the output file
#'@param plot The plot to save
#'@param width The plot width
#'@param height The plot height
#'@return The plot itself
#'@export
save_plot = function(filename, plot, width = 6.5, height = 5, background = "white") {
  ggsave(filename = filename, plot, width = width, height = height, bg = ifelse(is.na(background), "transparent", background))

  return (invisible(plot))
}
