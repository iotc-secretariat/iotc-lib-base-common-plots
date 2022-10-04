#'Produces the color factors for a data frame given a \code{FILL_BY} column
#'@param data A data frame
#'@param fill_by The name of the column that contains the factors to use to colorize the results. Should be one among
#'{\code{C_FISHING_GROUND_CODE}, \code{C_FLEET_CODE},
#' \code{C_IUCN_STATUS_CODE}, \code{C_SPECIES_WP_CODE}, \code{C_SPECIES_GROUP_CODE}, \code{C_SPECIES_CATEGORY_CODE}, \code{C_SPECIES_CODE},
#' \code{C_FISHERY_TYPE_CODE}, \code{C_FISHERY_GROUP_CODE}, \code{C_FISHERY_CODE}, \code{C_GEAR_CODE},
#' \code{C_QUALITY_CODE}, \code{C_FATE_TYPE_CODE}, \code{C_FATE_CODE}, \code{C_CONDITION_TYPE_CODE}, \code{C_CONDITION_CODE}, \code{C_RAISE_CODE}}
#'@return the color factors to use
#'@export
factorize_colors = function(data, fill_by = C_SPECIES_CODE) {
  if(fill_by %in% c(C_QUARTER))                                   return (factorize_quarters(quarter_colors_for(data)))
  if(fill_by %in% c(C_FISHING_GROUND, C_FISHING_GROUND_CODE))     return (factorize_fishing_grounds(fishing_ground_colors_for(data)))

  if(fill_by %in% c(C_FLEET, C_FLEET_CODE))                       return (factorize_fleets(fleet_colors_for(data)))

  if(fill_by %in% c(C_FISHERY_TYPE, C_FISHERY_TYPE_CODE))         return (factorize_fishery_types(fishery_type_colors_for(data)))
  if(fill_by %in% c(C_FISHERY_GROUP, C_FISHERY_GROUP_CODE))       return (factorize_fishery_groups(fishery_group_colors_for(data)))
  if(fill_by %in% c(C_FISHERY, C_FISHERY_CODE))                   return (factorize_fisheries(fishery_colors_for(data)))
  if(fill_by %in% c(C_GEAR, C_GEAR_CODE))                         return (factorize_gears(gear_colors_for(data)))

  if(fill_by %in% c(C_IUCN_STATUS, C_IUCN_STATUS_CODE))           return (factorize_IUCN_status(IUCN_colors_for(data)))
  if(fill_by %in% c(C_SPECIES_WP, C_SPECIES_WP_CODE))             return (factorize_species_wps(species_wp_colors_for(data)))
  if(fill_by %in% c(C_SPECIES_GROUP, C_SPECIES_GROUP_CODE))       return (factorize_species_groups(species_group_colors_for(data)))
  if(fill_by %in% c(C_SPECIES_CATEGORY, C_SPECIES_CATEGORY_CODE)) return (factorize_species_categories(species_category_colors_for(data)))
  if(fill_by %in% c(C_SPECIES, C_SPECIES_CODE))                   return (factorize_species(species_colors_for(data)))

  if(fill_by %in% c(C_QUALITY, C_QUALITY_CODE))                   return (factorize_qualities(quality_colors_for(data)))
  if(fill_by %in% c(C_FATE_TYPE, C_FATE_TYPE_CODE))               return (factorize_fate_types(fate_type_colors_for(data)))
  if(fill_by %in% c(C_FATE, C_FATE_CODE))                         return (factorize_fates(fate_colors_for(data)))
  if(fill_by %in% c(C_CONDITION_TYPE, C_CONDITION_TYPE_CODE))     return (factorize_condition_types(condition_type_colors_for(data)))
  if(fill_by %in% c(C_CONDITION, C_CONDITION_CODE))               return (factorize_conditions(condition_colors_for(data)))
  if(fill_by %in% c(C_RAISING, C_RAISE_CODE))                     return (factorize_raisings(raising_colors_for(data)))

  #Forcing 'data' to be a data.table as sometimes damn' R runtime turns this into a data.frame
  #causing issues in subsequent attempts at manipulating (read / write) the object

  setDT(data)

  return (data)
}

fail_if_empty = function(data) {
  if(!is_available(data) | nrow(data) == 0) {
    stop("No data provided")
  }

  return(data)
}

do_initialize_shrink_categories = function(data, max_categories) {
  data_by_category = data[, .(VALUE = sum(VALUE)), keyby = .(FILL_BY)]
  data_by_category = data_by_category[order(-VALUE)]

  number_categories = nrow(data_by_category)

  if(number_categories > max_categories) {
    top_categories = data.table::first(as.character(data_by_category$FILL_BY), max_categories)

    data[!(FILL_BY %in% top_categories)]$FILL_BY = "All others"
  }

  return(data)
}

do_shrink_categories = function(data, post_shrinker, colors, colors_are_custom = FALSE, max_categories) {
  #Required because under certain circumstances (e.g. factoring by fleet) it is
  #provided as a data.table and this introduces errors down in the code below! :|

  colors = as.data.table(colors)
  current_categories = length(unique(data$FILL_BY))
  num_colors = nrow(colors)

  has_max_categories = !is.na(max_categories)

  needs_shrinking = has_max_categories & current_categories > max_categories

  if(needs_shrinking) {
    data = do_initialize_shrink_categories(data, max_categories)

    data = post_shrinker(data)
  }

  current_categories = unique(data[FILL_BY != "All others"]$FILL_BY)
  num_current_categories = length(current_categories)

  if(num_colors < length(num_current_categories))
    stop(
      paste("Not enough colors provided:",
            current_categories, "required,",
            num_colors, "provided")
    )

  if(colors_are_custom) {
    colors = head(colors, num_current_categories)
    colors$FILL_BY = sort(current_categories)
  } else {
    colors = colors[colors$FILL_BY %in% current_categories,]
  }

  if(needs_shrinking) {
    all_others = data.frame("All others", "#333333", "#000000")
    names(all_others) = c("FILL_BY", "FILL", "OUTLINE")

    colors = rbind(colors, all_others, fill = TRUE)
  }

  return(list(data = as.data.table(data), colors = as.data.table(colors)))
}

shrink_categories_pareto = function(data, colors, max_fill_categories, custom_colors = FALSE) {
  return(
    do_shrink_categories(
      data,
      function(data) { return(data = data[, .(VALUE = sum(VALUE)), keyby = .(YEAR, CATEGORY, FILL_BY)]) },
      colors,
      colors_are_custom = custom_colors,
      max_categories = max_fill_categories
    )
  )
}

shrink_categories_geo = function(data, colors, max_fill_categories, custom_colors = FALSE) {
  return(
    do_shrink_categories(
      data,
      function(data) { return(data = data[, .(VALUE = sum(VALUE)), keyby = .(FISHING_GROUND_CODE, FILL_BY)]) },
      colors,
      colors_are_custom = custom_colors,
      max_categories = max_fill_categories
    )
  )
}

shrink_categories_value = function(data, colors, max_fill_categories, custom_colors = FALSE) {
  return(
    do_shrink_categories(
      data,
      function(data) {
        if("TIME" %in% colnames(data)) data = data[, .(VALUE = sum(VALUE)), keyby = .(TIME, FILL_BY)]
        else if(YEAR %in% colnames(data)) data = data[, .(VALUE = sum(VALUE)), keyby = .(YEAR, FILL_BY)]
      },
      colors,
      colors_are_custom = custom_colors,
      max_categories = max_fill_categories
    )
  )
}

spatially_disaggregate_geo = function(data, standard_grid) {
  value_before = sum(data$VALUE)

  FGs = unique(data$FISHING_GROUND_CODE)

  grid_mappings = grid_intersections_by_target_grid_type(
    FGs,
    standard_grid
  )

  data = merge(x = data,
               y = grid_mappings,
               all.x = TRUE,
               by.x = C_FISHING_GROUND_CODE,
               by.y = "SOURCE_FISHING_GROUND_CODE",
               allow.cartesian = TRUE)

  nFGs = unique(data[is.na(TARGET_FISHING_GROUND_CODE)]$FISHING_GROUND_CODE)

  data = data[!is.na(TARGET_FISHING_GROUND_CODE)][, .(VALUE = sum(VALUE * PROPORTION)), keyby = .(FILL_BY, TARGET_FISHING_GROUND_CODE)]

  names(data)[names(data) == "TARGET_FISHING_GROUND_CODE"] = C_FISHING_GROUND_CODE

  value_after     = sum(data$VALUE)
  value_diff      = value_after - value_before
  value_diff_perc = ifelse(value_before == 0, 0, value_diff / value_before)

  if(value_diff_perc > 0.05) {
    warning(
      paste0(
        "Difference detected between values after rearranging grids to standard ones: ",
        round(value_after), " vs. ", round(value_before), ". ",
        length(nFGs), " ",
        "unmapped grid codes in original data"
      )
    )
  }

  return(data)
}
