% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iotc_base_common_plots_size_frequency_ridges.R
\name{size_ridges}
\alias{size_ridges}
\title{Plots a faceted set of size frequencies (as ridge plots)}
\usage{
size_ridges(
  data,
  categorize_by = C_FISHERY_CODE,
  colors = NA,
  rel_min_height = 0,
  scale = 3,
  measure_type = "Fork length",
  measure_unit = "cm",
  show_samples_proportion = FALSE,
  show_median = TRUE,
  show_mean = FALSE,
  num_cols = NULL,
  num_rows = NULL
)
}
\arguments{
\item{data}{A size-frequency data set (as returned by \code{SF_raw}, \code{SF_est} and similar methods)}

\item{categorize_by}{The name of the column (in \code{data} that contains the categories)}

\item{colors}{A data frame containing the colors (FILL and OUTLINE) for the factors, if set to \code{NA} these will be determined by the \code{FILL_BY} parameter}

\item{rel_min_height}{A filter on the minimum relative height (0..1) that points of each yearly time series should have to be maintained in the plot. Defaults to 0 (i.e., all points are kept)}

\item{scale}{The scale to be used for the ridge lines (the higher, the more the ridge lines for consecutive years will overlap)}

\item{measure_type}{The measurement type to use as X-axis label}

\item{measure_unit}{The measurement unit (to be used in the X-axis label)}

\item{show_samples_proportion}{When \code{TRUE} each ridgeline in the same facet will be assigned an opacity that's proportional to the relative yearly number of samples (for that category)}

\item{show_median}{When \code{TRUE} plots the median size (for each year in each category) as a green dot}

\item{show_mean}{When \code{TRUE} plots the mean size (for each year in each category) as a red dot}

\item{num_cols}{The number of columns in the faceted plot}

\item{num_rows}{The number of rows in the faceted plot}
}
\value{
the plot
}
\description{
Plots a faceted set of size frequencies (as ridge plots)
}
