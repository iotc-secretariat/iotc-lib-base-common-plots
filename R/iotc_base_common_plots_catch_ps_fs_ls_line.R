#'Plots the proportion of FS vs. LS catches for the most common industrial purse seine fleets (EUESP, EUFRA, SYC)
#'@param data A data table whose \code{FISHERY_CODE} column distinguishes between PS FS and PS LS
#'@return The plot
#'@export
ps_fs_ls_proportions_line = function(data) {
  data = data[FISHERY_CODE %in% c("PSFS", "PSLS"),]

  ESP = calculate_proportions(data[ FLEET_CODE == "EUESP",])
  FRA = calculate_proportions(data[ FLEET_CODE == "EUFRA",])
  SYC = calculate_proportions(data[ FLEET_CODE == "SYC",])
  OTH = calculate_proportions(data[!FLEET_CODE %in% c("EUESP", "EUFRA", "SYC")])
  ALL = calculate_proportions(data)

  ESP$FLEET = "ESP"
  FRA$FLEET = "FRA"
  SYC$FLEET = "SYC"
  OTH$FLEET = "OTH"
  ALL$FLEET = "ALL"

  ALL = union_all(x = ALL, y = ESP)
  ALL = union_all(x = ALL, y = FRA)
  ALL = union_all(x = ALL, y = SYC)
  ALL = union_all(x = ALL, y = OTH)

  minY = min(ALL$YEAR)
  maxY = max(ALL$YEAR)

  ALL$FLEET = factor(ALL$FLEET,
                      levels=c("ESP", "FRA", "SYC", "OTH", "ALL"),
                      labels=c("EU,Spain", "EU,France", "Seychelles", "Other", "All PS fleets combined"))

  # All fleets combined
  p = initialize_plot(ALL, aesthetics = aes(x = YEAR,
                                            y = LS_PROP * 100,
                                            size = as.character(FLEET) == "All PS fleets combined",
                                            linetype = as.character(FLEET) == "All PS fleets combined")) +

      theme(panel.grid.minor.x = element_blank())

  p =
    p +

    geom_line(aes(col = FLEET)) +

    geom_point(shape = 21,
               aes(col = FLEET),
               fill = "white",
               size = 1.5,
               stroke = 1.5) +

    #Kept, although this is currently useless as all lines are drawn in 'solid' style
    scale_linetype_manual(values = c("solid", "solid"), guide = guide_none()) +

    scale_color_manual(values = c(rgb(238, 137,  69, alpha=255, max=255),
                                  rgb(112, 167, 219, alpha=255, max=255),
                                  rgb(168, 206, 144, alpha=255, max=255),
                                  "gray",
                                  "black")) +

    scale_size_manual(values = c(1.1, 2), guide = guide_none()) +

    guides(col=guide_legend(nrow = 1)) +

    scale_x_continuous(expand = c(0, 0),
                       breaks = c(seq(minY, maxY, 5), maxY)) +
    scale_y_continuous(expand = c(0, 0),
                       limits = c(0, 100),
                       breaks = seq(0, 100, 10),
                       sec.axis = dup_axis(name = element_blank())) +

    labs(x = "", y = "% of log-school catches")

  return (p)
}

calculate_proportions = function(original_data) {
  data = copy(original_data)[, .(FS  = sum(ifelse(FISHERY_CODE == "PSFS", CATCH, 0), na.rm = TRUE),
                                 LS  = sum(ifelse(FISHERY_CODE == "PSLS", CATCH, 0), na.rm = TRUE),
                                 TOT = sum(CATCH)),
                             keyby = .(YEAR)]

  data$LS_PROP = data$LS / data$TOT

  return (data)
}
