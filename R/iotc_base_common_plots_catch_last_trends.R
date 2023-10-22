#'Produces a summary chart of annual catch trends (in the temporal range expressed by the input data) categorized and colored according to a specific data field
#'@param data the input dataset
#'@param categorize_by the data field to be used to categorize the annual catches
#'@param colors the colors to be used for each of the categories identified in the input data
#'@return the summary chart of annual catch trends according to the provided inputs
#'@export
catch_last_trends_bar = function(data,
                                 categorize_by = C_FISHERY_GROUP,
                                 colors = factorize_fishery_groups(fishery_group_colors_for(data))) {
  OTHERS = "Other"

  colnames(data)[which(colnames(data) == categorize_by)] = "CATEGORY"

  data$CATEGORY = droplevels(data$CATEGORY)

  minY = min(data$YEAR)
  maxY = max(data$YEAR)

  number_of_years = maxY - minY + 1

  opacity_step = 1 / number_of_years

  dataYC = data  [, .(CATCH = sum(CATCH)),        keyby = .(YEAR, CATEGORY)]
  dataYC = dataYC[,   TOT_CATCH := sum(CATCH),       by = .(CATEGORY)]
  dataC  = dataYC[, .(TOT_CATCH = sum(CATCH)),    keyby = .(CATEGORY)]

  dataYC[, ORDERED_CATCH := ifelse(CATEGORY == OTHERS, -TOT_CATCH, TOT_CATCH)]
  dataC [, ORDERED_CATCH := ifelse(CATEGORY == OTHERS, -TOT_CATCH, TOT_CATCH)]

  dataYC$YEAR = factor(dataYC$YEAR)

  cMin = min(dataYC$CATCH) / 1000
  cMax = max(dataYC$CATCH) / 1000

  sorted_colors = as.data.table(merge(x = colors, y = dataC, by.x = categorize_by, by.y = "CATEGORY"))
  sorted_colors = sorted_colors[order(-ORDERED_CATCH),]

  p =
    initialize_plot(dataYC, aesthetics = aes(fill  = reorder(CATEGORY, -ORDERED_CATCH),
                                             col   = reorder(CATEGORY, -ORDERED_CATCH),
                                             x     = reorder(CATEGORY, -ORDERED_CATCH),
                                             y     = CATCH / 1000,
                                             alpha = YEAR)) +

    theme(panel.grid.major.x = element_blank(),

          axis.text.x  = element_text(size = 10),
          axis.title.x = element_text(size = 16),
          axis.text.y  = element_text(size = 14),
          axis.title.y = element_text(size = 16),

          legend.position = "top",
          legend.text  = element_text(size = 12),
          legend.title = element_text(size = 16),
          legend.key.width = unit(.5, "cm"),
          legend.key.height = unit(.5, "cm")) +

    geom_col(na.rm = FALSE, position =  position_dodge(preserve = "single")) +

    guides(alpha = guide_legend(override.aes = list(col = "darkgray", fill = "darkgray"))) +

    scale_alpha_manual (values = seq(opacity_step, 1, opacity_step), name = "Year") +
    scale_fill_manual  (values = sorted_colors$FILL,    drop = FALSE, guide = guide_none()) +
    scale_colour_manual(values = sorted_colors$OUTLINE, drop = FALSE, guide = guide_none()) +

    scale_x_discrete  (expand = c(0, 0.5)) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = breaks_for(cMin, cMax),
                       labels = function(x) { format(x, big.mark = ",", scientific = FALSE) },
                       sec.axis = dup_axis(name = element_blank())) +

    coord_cartesian(ylim = limit_for(cMin, cMax)) +

    labs(x = "", y = "Total catch (x1,000 t)")

  return (p)
}

#'Produces a summary chart of annual catch trends (in the temporal range expressed by the input data) categorized according to a specific data field (with a
#'maximum number of user-provided categories) and uniformly colored with the provided outline and fill colors
#'@param data the input dataset
#'@param categorize_by the data field to be used to categorize the annual catches
#'@param outline the outline color to be used for all of the categories identified in the input data
#'@param fill the fill color to be used for all of the categories identified in the input data
#'@param max_categories the maximum number of categories to display
#'@export
catch_last_trends_bar_for = function(data, categorize_by = C_FLEET, outline = "darkgrey", fill = "darkgrey", max_categories = 4) {
  ALL_OTHERS = "All others"

  colnames(data)[which(colnames(data) == categorize_by)] = "CATEGORY"

  data$CATEGORY = droplevels(data$CATEGORY)

  minY = min(data$YEAR)
  maxY = max(data$YEAR)

  number_of_years = maxY - minY + 1

  opacity_step = 1 / number_of_years

  #Calculates total catches by category...
  dataC  = copy(data)[, .(TOT_CATCH = sum(CATCH)),    keyby = .(CATEGORY)]

  #Sorts the category codes by descending total catch order
  category_codes = dataC[order(-TOT_CATCH), CATEGORY]

  #Identifies the top categories (first 'n', with n = max_categories) and the remaining ones
  top_categories = head(category_codes, min(max_categories, length(category_codes)))
  oth_categories = tail(category_codes, max(0, length(category_codes) - max_categories))

  #Assigns 'All others' to the records whose category is not in the top ones
  dataYC = copy(data)[CATEGORY %in% oth_categories, CATEGORY := ALL_OTHERS]

  #Computes a new column with total catch by year and category
  dataYC = dataYC[, .(CATCH = sum(CATCH)),        keyby = .(YEAR, CATEGORY)]

  #As well as a new column with total catch by category
  dataYC = dataYC[,   TOT_CATCH := sum(CATCH),       by = .(CATEGORY)]

  #Orders the catch by year and category as well as those by category in descending total catch order
  dataYC[, ORDERED_CATCH := ifelse(CATEGORY == ALL_OTHERS, -TOT_CATCH, TOT_CATCH)]
  dataC [, ORDERED_CATCH := ifelse(CATEGORY == ALL_OTHERS, -TOT_CATCH, TOT_CATCH)]

  #Factorizes the YEAR column
  dataYC$YEAR = factor(dataYC$YEAR)

  cMin = min(dataYC$CATCH) / 1000
  cMax = max(dataYC$CATCH) / 1000

  p =
    initialize_plot(dataYC, aesthetics = aes(fill  = fill,
                                             col   = outline,
                                             x     = reorder(CATEGORY, -ORDERED_CATCH),
                                             y     = CATCH / 1000,
                                             alpha = YEAR)) +

    theme(axis.text.x  = element_text(size = 10), # angle = 90, vjust = 0.3, hjust = 1),
          axis.title.x = element_text(size = 16),
          axis.text.y  = element_text(size = 14),
          axis.title.y = element_text(size = 16),

          panel.grid.major.x = element_blank(),
          legend.position = "top",
          legend.text  = element_text(size = 12),
          legend.title = element_text(size = 16),
          legend.key.width = unit(.5, "cm"),
          legend.key.height = unit(.5, "cm")) +

    geom_col(na.rm = FALSE, position =  position_dodge(preserve = "single")) +

    guides(alpha = guide_legend(override.aes = list(col = outline, fill = fill))) +

    scale_alpha_manual (values = seq(opacity_step, 1, opacity_step), name = "Year") +
    scale_fill_manual  (values = fill,    guide = "none") +
    scale_colour_manual(values = outline, guide = "none") +

    scale_x_discrete  (expand = c(0, 0.5)) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = breaks_for(cMin, cMax),
                       labels = function(x) { format(x, big.mark = ",", scientific = FALSE) },
                       sec.axis = dup_axis(name = element_blank())) +

    coord_cartesian(ylim = limit_for(cMin, cMax)) +

    labs(x = "", y = "Total catch (x1,000 t)")

  return (p)
}
