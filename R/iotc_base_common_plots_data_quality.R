RED = "red"
DARK_RED = darken(RED, amount = 0.2)

#'Mixed bar + line plot of data quality by year and data set. Bars are further broken down by quality score (0 = high, 2, 4, 6, 8 = low) and
#'a line plot is superimposed to show the percentage of catches by year with a "good" quality score (i.e. between 0 and 2).
#'@param data A data frame containing data quality records  by \code{C_YEAR}, \code{C_FLEET_CODE}, \code{C_FISHERY_CODE} and \code{C_SPECIES_CODE}
#'@param data_set One of the data sets for which data quality should be plot (one among \code{NC}, \code{CE} and \code{SF})
#'@param fishery_type Optional filter on the \code{C_FISHERY_TYPE_CODE} column (one among \code{ART} and \code{IND})
#'@param catch_max Optional, to set the maximum value (in 1,000 x MT) for the catch Y-axis. Useful when plotting data quality for different components (all, artisanal, industrial)
#'of the same data set
#'@param show_percent_good_quality Optional flag to turn ON or OFF the display of the red line showing the percentage of catches reported as having "good" quality. Defaults to \code{TRUE}
#'@return the plot for the strata identified by the given input filters
#'@examples data_quality_bar(data, "NC")
#'@examples data_quality_bar(data, "NC", "ART")
#'@examples data_quality_bar(data, "NC", "IND", 600)
#'@export
data_quality_bar = function(data,
                            data_set = "NC",
                            fishery_type = NA,
                            catch_max = NA,
                            show_percent_good_quality = TRUE) {

  if(!is.na(fishery_type)) data = data[FISHERY_TYPE_CODE == fishery_type]

  fail_if_empty(data)

  yMin = min(data$YEAR)
  yMax = max(data$YEAR)
  yCur = as.integer(format(Sys.Date(), "%Y"))

  data_quality = data[, .(CATCH    = sum(CATCH, na.rm=T) / 1000,
                          CATCH_CE = sum(CATCH_CE, na.rm=T) / 1000,
                          SAMPLES  = sum(SAMPLES, na.rm=T)),
                          keyby    = .(YEAR, SCORE = get(data_set))]

  # Small temporary correction for scores of value 3 (MDB BB in 2008-2009) - RE-CHECK
  data_quality[SCORE == 3, SCORE :=  4]
  data_quality[, SCORE := factor(SCORE, levels = c(8, 6, 4, 2, 0))]

  data_quality[, Q := ifelse(SCORE %in% c(0,2), "Good", "Bad")]
  data_quality_Y = data_quality[, .(CATCH = sum(CATCH)), keyby = .(YEAR)]

  #Minimum / Maximum catch by label values
  cMin  = min(data_quality_Y$CATCH)
  cMax  = max(data_quality_Y$CATCH)

  data_quality_G = data_quality[, .(PERCENT_GOOD = sum(ifelse(Q == "Good", CATCH, 0), na.rm = T) / sum(CATCH) * 100 ), keyby = .(YEAR)]

  if(!is.na(catch_max)) cMax = max(cMax, catch_max)

  #Calculates the upper range of the Y axis, rounded to the closest multiple of the magnitude
  cMaxx = calculate_limit(cMin, cMax)

  #Adjusts the catch cumulative percentage in order to be able to plot the trendline with the right scale
  data_quality_G[, PERCENT_GOOD :=  PERCENT_GOOD * cMaxx / 100]

  p =
    initialize_plot(data_quality) +

    theme(legend.position = "top",
          #legend.text  = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.key.width = unit(.5, "cm"),
          legend.key.height = unit(.5, "cm"))

  if(show_percent_good_quality) {
    p = p +
      theme(
        axis.line.y.right  = element_line(color = RED),
        axis.ticks.y.right = element_line(color = RED),
        axis.text.y.right  = element_text(color = RED),
        axis.title.y.right = element_text(color = RED, angle = 270)
      )
  }

  p = p +

    scale_fill_manual  (values = rev(all_quality_colors()$FILL),    drop = FALSE, name = "Data quality score") +
    scale_colour_manual(values = rev(all_quality_colors()$OUTLINE), drop = FALSE, guide = "none") +

    geom_col(aes(x = YEAR,
                 y = CATCH,
                 fill = SCORE,
                 color= SCORE),
             position = "stack",
             width = 1)

  if(show_percent_good_quality) {
    p = p +
      geom_line (data = data_quality_G,
                 aes(x = YEAR,
                     y = PERCENT_GOOD),
                 colour = RED) +

      geom_point(data = data_quality_G,
                 aes(x = YEAR,
                     y = PERCENT_GOOD),
                 colour = RED)
  }

  p = p +
    scale_x_continuous(expand = c(0, 0),
                       breaks = c(seq(yMin, yMax, 10), min(yMax, yCur))) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = breaks_for(cMin, cMax),
                       labels = function(x) { format(x, big.mark = ",", scientific = FALSE) })

  if(show_percent_good_quality) {
    p = p +
      scale_y_continuous(
        sec.axis = sec_axis(~. * 100 / cMaxx,
                            breaks = seq(0, 100, 10),
                            name = "% Catches fully or partially reported")
      )
  }

  p = p +

    coord_cartesian(ylim = limit_for(cMin, cMax)) +

    labs(x = "", y = "Total catches (x1,000 t)")

  return (p)
}

#'For internal usage
data_quality_line_helper = function(data, data_set) {
  data[, SCORE := get(data_set)]
  data[SCORE == 3, SCORE := 4]

  yMin = min(data$YEAR)
  yMax = max(data$YEAR)
  yCur = as.integer(format(Sys.Date(), "%Y"))

  data_quality = data[, .(CATCH    = sum(CATCH, na.rm=T) / 1000,
                          CATCH_CE = sum(CATCH_CE, na.rm=T) / 1000,
                          SAMPLES  = sum(SAMPLES, na.rm=T)),
                          keyby    = .(YEAR, SCORE)]

  # Small temporary correction for scores of value 3 (MDB BB in 2008-2009) - RE-CHECK
  data_quality[SCORE == 3, SCORE :=  4]
  data_quality[, SCORE := factor(SCORE, levels = c(8, 6, 4, 2, 0))]

  data_quality[, Q := ifelse(SCORE %in% c(0,2), "Good", "Bad")]

  data_quality_G = data_quality[, .(PERCENT_GOOD = sum(ifelse(Q == "Good", CATCH, 0), na.rm = T) / sum(CATCH) * 100 ), keyby = .(YEAR)]

  return (data_quality_G)
}

#'Multiple line plot of the percentage of "good" quality data (i.e. with a score between 0 and 2) by year and data set. Three lines are plot: one for artisanal fisheries, one for industrial fisheries and one
#'for all fisheries combined.
#'@param data A data frame containing data quality records  by \code{C_YEAR}, \code{C_FLEET_CODE}, \code{C_FISHERY_CODE} and \code{C_SPECIES_CODE}
#'@param data_set One of the data sets for which data quality should be plot (one among \code{NC}, \code{CE} and \code{SF})
#'@return the plot for the strata identified by the given input filters
#'@examples data_quality_line(data, "CE")
#'@export
data_quality_line = function(data, data_set = "NC") {
  data_art = data[FISHERY_TYPE_CODE == "ART", ]
  data_ind = data[FISHERY_TYPE_CODE == "IND", ]

  data_all = data_quality_line_helper(data, data_set)
  data_art = data_quality_line_helper(data_art, data_set)
  data_ind = data_quality_line_helper(data_ind, data_set)

  yMin = min(data_all$YEAR)
  yMax = max(data_all$YEAR)
  yCur = as.integer(format(Sys.Date(), "%Y"))

  color_art = fill_for_fishery_type("ART")
  color_ind = fill_for_fishery_type("IND")

  p =
    initialize_plot(data_all) +

    geom_line (data = data_all,
               size = 1,
               aes(color = "ALL",
                   x = YEAR,
                   y = PERCENT_GOOD)) +

    geom_point(data = data_all,
               aes(color = "ALL",
                   x = YEAR,
                   y = PERCENT_GOOD)) +

    geom_line (data = data_art,
               size = 1,
               aes(color = "ART",
                   x = YEAR,
                   y = PERCENT_GOOD)) +

    geom_point(data = data_art,
               aes(color = "ART",
                   x = YEAR,
                   y = PERCENT_GOOD)) +

    geom_line (data = data_ind,
               size = 1,
               aes(color = "IND",
                   x = YEAR,
                   y = PERCENT_GOOD)) +

    geom_point(data = data_ind,
               aes(color = "IND",
                   x = YEAR,
                   y = PERCENT_GOOD)) +


    scale_color_manual(values = c("ALL" = "black", "ART" = color_art, "IND" = color_ind),
                       labels = c("All fisheries", "Artisanal fisheries", "Industrial fisheries")) +
    scale_x_continuous(expand = c(0, 0),
                       breaks = c(seq(yMin, yMax, 10), min(yMax, yCur))) +
    scale_y_continuous(expand = c(0, 0),
                       breaks = seq(0, 100, 10),
                       labels = function(x) { format(x, scientific = FALSE) },
                       sec.axis = dup_axis(name = element_blank())) +

    coord_cartesian(ylim = c(0, 100)) +

    labs(x = "", y = "% catches fully or partially reported")

  return (p)
}

#'Same as \code{\link{data_quality_bar}}
#'@export
bar.quality = data_quality_bar

#'Same as \code{\link{data_quality_bar}} with \code{data_set = "NC"}
#'@export
line.quality.NC = function(data) {
  return(data_quality_line(data, data_set = "NC"))
}

#'Same as \code{\link{data_quality_line}} with \code{data_set = "CE"}
#'@export
line.quality.CE = function(data) {
  return(data_quality_line(data, data_set = "CE"))
}

#'Same as \code{\link{data_quality_line}} with \code{data_set = "SF"}
#'@export
line.quality.SF = function(data) {
  return(data_quality_line(data, data_set = "SF"))
}

#'Same as \code{\link{data_quality_line}}
#'@export
line.quality = data_quality_line

