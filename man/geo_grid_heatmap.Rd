% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iotc_base_common_plots_geo_grid_heat.R
\name{geo_grid_heatmap}
\alias{geo_grid_heatmap}
\title{Implementation of georeferenced, gridded heatmaps with ggplot2.}
\usage{
geo_grid_heatmap(
  data,
  value,
  yearly_average = TRUE,
  num_intervals = 5,
  unit = "t",
  breaks = NA,
  show_grid_borders = TRUE,
  xlim = IO_map_xlim,
  ylim = IO_map_ylim,
  show_IO = TRUE,
  show_EEZs = FALSE,
  show_high_seas = FALSE,
  standard_grid = grid_5x5,
  palette = colorspace::heat_hcl,
  legend_title = NULL
)
}
\arguments{
\item{data}{The data (shall contain a \code{C_FISHING_GROUND_CODE} column)}

\item{value}{The name of the column (in \code{data}) that contains the value to plot}

\item{yearly_average}{If \code{TRUE} uses the yearly average to plot the pies (assuming that the input data contains a \code{C_YEAR} column)}

\item{num_intervals}{The number of intervals ('breaks') to split the range of values into. The result will be calculated as the quantiles with probabilities increasing by \code{with n * 1 / num_intervals} at each step.}

\item{unit}{The value unit (for display purposes)}

\item{breaks}{Externally provided breaks. Alternative to \code{num_intervals}}

\item{show_grid_borders}{when \code{TRUE}, each regular grid will have a darker border around it}

\item{show_IO}{when \code{TRUE}, the layers for F51 and F57 are plotted on the map}

\item{show_EEZs}{When \code{TRUE}, the overall EEZ layer is plotted on the map (not showing explicit distinctions among different EEZs)}

\item{show_high_seas}{When \code{TRUE}, the overall high seas layer is plotted on the map}

\item{standard_grid}{Transforms the input data in order to only use grids of the provided \code{standard_grid} type (one among \{ \code{grid_1x1}, \code{grid_5x5}, \code{grid_10x10}, \code{grid_10x20}, \code{grid_20x20}, \code{grid_30x30} \}). The transformation is based on pre-calculated mappings (see \code{[IOTCStatistics].[dbo].[CL_FISHING_GROUND_AGGREGATIONS]}) and might lead to data loss if no mapping exists for some of the input grid codes.}

\item{palette}{A palette function capable of returning \code{n} colors. Defaults to \code{colorspace::heat_hcl}}

\item{legend_title}{The title to use for the legend (none if \code{NULL}). Defaults to the value provided for \code{VALUE}}
}
\value{
The \code{ggplot2} object representing the plot
}
\description{
Implementation of georeferenced, gridded heatmaps with ggplot2.
}
