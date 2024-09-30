#' Evaluate Simulation Results for Statistical Significance
#'
#' Evaluates the results of a simulation by checking if all specified contrasts are statistically significant at the 5% significance level.
#'
#' @param dat_results_sim An object containing the simulation results, typically an \code{emmGrid} object or a data frame with a \code{p.value} column for contrasts.
#'
#' @return A logical value: \code{TRUE} if all contrasts have p-values less than 0.05 (i.e., all are statistically significant), \code{FALSE} otherwise.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Converts the input \code{dat_results_sim} to a data frame if it isn't one already.
#'   \item Filters the contrasts to those with \code{p.value < 0.05}.
#'   \item Checks if the number of significant contrasts equals 3 (assuming there are three contrasts in total).
#' }
#'
#' This function is useful for summarizing simulation studies where the goal is to determine the proportion of simulations in which all contrasts are significant.
#'
#' @examples
#' # Simulate study results
#' result <- fun_simulate_study()
#'
#' # Evaluate if all contrasts are significant
#' all_significant <- eval_sim_results(result)
#' print(all_significant)
#'
#' @seealso \code{\link{fun_simulate_study}}
#'
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @export
eval_sim_results <- function(dat_results_sim) {
  # Evaluate the results of the simulation
  # by checking if all the contrasts are significant

  # Check if the contrasts are significant
  sig_contrasts <- dat_results_sim %>%
    as.data.frame() %>%
    filter(p.value < 0.05)

  # Return the number of significant contrasts
  return(nrow(sig_contrasts) == 3)
}
