% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/while_window_clicked_wrapper.r
\name{While_window_clicked_wrapper}
\alias{While_window_clicked_wrapper}
\title{Execute a Function While Waiting for a GUI Click}
\usage{
While_window_clicked_wrapper(
  OUT_GEN_FUN,
  FUN,
  iter_per_core = 1,
  packages = c("dplyr"),
  max_iters = Inf
)
}
\arguments{
\item{OUT_GEN_FUN}{A function that generates an output dataframe. This function should
take an argument \code{n} and return a dataframe with at least \code{n} rows.}

\item{FUN}{A function to be applied to each row of the dataframe generated by
\code{OUT_GEN_FUN}. It should process a dataframe and return the processed dataframe.}

\item{iter_per_core}{Numeric. The number of iterations per core. Default is 1.}

\item{packages}{Character vector. The names of packages that should be loaded in each
parallel worker. Default is \code{c("dplyr")}.}

\item{max_iters}{Numeric. The maximum number of iterations allowed before stopping.
Default is 10,000,000. The function will stop if this limit is reached
or if the GUI canvas is clicked.}
}
\value{
A dataframe containing the results of applying \code{FUN} to the data generated
by \code{OUT_GEN_FUN} over multiple iterations.
}
\description{
This function creates a graphical user interface (GUI) window with a canvas widget
that waits for a mouse click event. While waiting, it executes a specified function
in parallel multiple times based on user-defined parameters. The results are collected
and returned after the window is clicked or a maximum number of iterations is reached.
}
\details{
Parallel_envir_setup() must be called before this function to set up the parallel environment and
to load the necessary packages and helper functions.

The function creates a Tkinter GUI window with a canvas. It binds a click event on
the canvas to a variable that signals when the canvas has been clicked. In a loop,
it generates data using \code{OUT_GEN_FUN}, processes the data using \code{FUN} in parallel,
and appends the results to a cumulative dataframe. The loop continues until either
the canvas is clicked or the maximum number of iterations is reached. The function
then closes the GUI window and returns the accumulated results.
}
\examples{
# Example functions
generate_data <- function(n) {
  data.frame(x = rnorm(n), y = rnorm(n))
}

process_data <- function(df) {
  df \%>\% dplyr::mutate(z = x + y)
}

# Run the wrapper function
results <- While_window_clicked_wrapper(
  OUT_GEN_FUN = generate_data,
  FUN = process_data,
  iter_per_core = 2,
  packages = c("dplyr"),
  max_iters = 10000
)

}
