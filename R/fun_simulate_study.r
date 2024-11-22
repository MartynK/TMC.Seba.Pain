#' Simulate a Between-Subjects Study and Analyze Treatment Effects
#'
#' Simulates data for a between-subjects study with three treatments (1, 2, 3).
#' The function generates outcome data based on specified treatment effects and residual variance,
#' assigns treatments to subjects using block randomization,
#' fits a linear model, and computes contrasts of treatment effects.
#'
#' @param n_samp Integer. Total number of subjects to simulate (default is 18).
#' @param block_size_coef Numeric. Coefficient to determine block size; block size is calculated as \code{3 * block_size_coef} (default is 1).
#' @param var_study Numeric. Variance of the residual errors in the outcome variable (default is 2).
#' @param treat_eff_c Numeric vector of length 3. Treatment effects for treatments 1, 2, and 3, respectively (default is \code{c(3, 2, 1)}).
#'
#' @return An \code{emmGrid} object containing contrasts of estimated marginal means (EMMs) for the treatments, including estimates, standard errors, degrees of freedom, t-ratios, and p-values.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Generates random residual errors (\code{eps}) for each subject based on the specified \code{var_study}.
#'   \item Assigns subjects to treatments using block randomization via \code{\link{fun_block_randomize}}.
#'   \item Calculates the outcome variable (\code{y}) for each subject as the sum of the treatment effect and residual error.
#'   \item Fits a linear model (\code{lm}) with treatment as the predictor.
#'   \item Computes contrasts of treatment effects using \code{emmeans::contrast} without adjustment.
#' }
#'
#' The contrasts calculated compare:
#' \itemize{
#'   \item Treatment 3 vs. Treatment 1 (\code{"C vs A"})
#'   \item Treatment 2 vs. Treatment 1 (\code{"B vs A"})
#'   \item Treatment 3 vs. Treatment 2 (\code{"C vs B"})
#' }
#'
#' If the model fitting fails (e.g., due to insufficient data), the function returns a data frame with nonsensical p-values (\code{p.value = c(2, 2, 2)}) to indicate the failure.
#'
#' @examples
#' # Simulate a study with default parameters
#' result <- fun_simulate_study()
#' print(result)
#'
#' # Access the summary of contrasts
#' summary(result)
#'
#' # Simulate a study with custom parameters
#' result <- fun_simulate_study(
#'   n_samp = 30,
#'   block_size_coef = 1.5,
#'   var_study = 5,
#'   treat_eff_c = c(4, 2, 0)
#' )
#' print(result)
#'
#' @seealso \code{\link{fun_block_randomize}}, \code{\link[stats]{lm}}, \code{\link[emmeans]{emmeans}}
#'
#' @importFrom stats rnorm lm
#' @importFrom emmeans emmeans contrast
#' @importFrom dplyr left_join mutate
#' @importFrom magrittr %>%
#' @export
fun_simulate_study <- function(
    n_samp = 18,            # Number of total subjects
    block_size_coef = 1,    # Coefficient to determine block size
    var_study = 2,     # Desired total variance
    treat_eff_c = c(3,1,0) # Treatment effects;A=no distr.,B=Ipad, C=VirtReal
) {

  treat_eff <- data.frame(trt = 1:length(treat_eff_c),
                          eff = treat_eff_c)

  block_size <- round(3 * block_size_coef)

  ##################
  # Data preparation

  # Create a dataframe for the subjects
  dat_subs <- data.frame(
    id = 1:n_samp,
    eps = rnorm(n_samp, sd = sqrt(var_study))
  )

  # Assign sequences to subjects using block randomization
  dat_subs$trt <- fun_block_randomize(n_samp, block_size, arms = 3)

  # Generate the outcome variable
  dat_subs <- dat_subs %>%
    left_join(., treat_eff, by = c("trt" = "trt")) %>%
    mutate(
      trt = factor(trt),
      y = {
      eps + eff
    } %>%
      round(2) %>%    # Round to 2 decimals
      pmax(0, .) %>%  # Set negative values to 0
      pmin(10, .)     # Set values > 10 to 10
    )

  #####################
  # Model the results

  mod <- try({
    # Fit the linear model
    lm(y ~ trt, data = dat_subs)
  }, silent = TRUE)

  # If the model fitting fails, return nonsensical p-values
  if (inherits(mod, "try-error")) {
    return(data.frame(p.value = c(2, 2, 2)))
  }

  # Compute estimated marginal means
  emm_ojj <- emmeans::emmeans(mod, ~ trt)

  # Define the contrasts
  contrast_list <- list(
    "C vs A" = c(-1, 0, 1),  # Assuming levels are ordered as A, B, C
    "B vs A" = c(-1, 1, 0),
    "C vs B" = c(0, -1, 1)
  )

  # Apply contrasts without adjustment
  res_contrast_none <- emmeans::contrast(
    emm_ojj,
    method = contrast_list,
    adjust = "none"
  )

  return(res_contrast_none)

}
