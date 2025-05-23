% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iotc_base_common_plots_lw_misc.R
\name{lw_plot}
\alias{lw_plot}
\title{X-Y plot of length / value pairs}
\usage{
lw_plot(
  data,
  categorize_by = C_SPECIES_CODE,
  swap_axis = FALSE,
  jitter_x = TRUE,
  jitter_y = FALSE,
  colors = NA,
  alpha = 0.7
)
}
\arguments{
\item{data}{The source data that shall contain a \code{LENGTH} and a \code{WEIGHT} column}

\item{categorize_by}{The data column that contains the categories for each series. Defaults to \code{C_SPECIES_CODE}}

\item{swap_axis}{If \code{TRUE} weights will be mapped to the X-axis and lengths to the Y-axis}

\item{jitter_x}{\code{TRUE} to add X-axis jittering to the plots}

\item{jitter_y}{\code{FALSE} to add Y-axis jittering to the plots}

\item{colors}{An alternative palette with as many colors as the \code{num_breaks}}

\item{alpha}{The transparency value to use when plotting the points}
}
\description{
X-Y plot of length / value pairs
}
\examples{
lw_plot(ros.LW(species_codes = "BET"))
lw_plot(ros.LW(species_codes = "BET"), categorize_by = C_FLEET_CODE)
lw_plot(ros.LW(species_codes = "BET"), categorize_by = C_LENGTH_MEASURE_TYPE_CODE, colors = color_table(unique_colors(16)))
}
