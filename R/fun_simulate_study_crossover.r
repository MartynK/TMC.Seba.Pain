#' Simulate a Three-Period Crossover Study and Analyze Treatment Effects
#'
#' Simulates data for a three-period crossover study with three treatments (A, B, C) using a Williams design.
#' The function generates subject-level random effects and residual errors based on specified variance components,
#' introduces missing data according to specified proportions, fits a linear mixed-effects model,
#' and computes contrasts of treatment effects.
#'
#' @param n_samp Integer. Total number of subjects to simulate (default is 18).
#' @param block_size_coef Numeric. Coefficient to determine block size; block size is calculated as \code{6 * block_size_coef} (default is 1).
#' @param total_variance Numeric. Total variance of the outcome variable (default is 2).
#' @param ICC Numeric between 0 and 1. Intraclass Correlation Coefficient, representing the proportion of variance due to random effects (default is 0.75).
#' @param treat_eff_c Numeric vector of length 3. Treatment effects for treatments A, B, and C, respectively (default is \code{c(3, 2, 1)}).
#' @param missing_prop Numeric vector of length 2. Cumulative proportions of missing data in periods 2 and 3 (default is \code{c(0.05, 0.1)}).
#'
#' @return An \code{emmGrid} object containing contrasts of estimated marginal means (EMMs) for the treatments, including estimates, standard errors, degrees of freedom, t-ratios, and p-values.
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Generates a Williams design sequence for three treatments over three periods.
#'   \item Simulates subject-level random intercepts based on the specified \code{ICC} and \code{total_variance}.
#'   \item Assigns subjects to sequences using block randomization via \code{\link{fun_block_randomize}}.
#'   \item Simulates outcome data (\code{y}) for each subject in each period, incorporating treatment effects, random effects, and residual errors.
#'   \item Introduces missing data in periods 2 and 3 according to \code{missing_prop}.
#'   \item Fits a linear mixed-effects model (\code{lmerTest::lmer}) with fixed effects for period and treatment, and a random intercept for subject.
#'   \item Computes contrasts of treatment effects using \code{emmeans::contrast} without adjustment.
#' }
#'
#' The contrasts calculated compare:
#' \itemize{
#'   \item Treatment C vs. Treatment A (\code{"C vs A"})
#'   \item Treatment B vs. Treatment A (\code{"B vs A"})
#'   \item Treatment C vs. Treatment B (\code{"C vs B"})
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
#'   total_variance = 5,
#'   ICC = 0.5,
#'   treat_eff_c = c(4, 2, 0),
#'   missing_prop = c(0.1, 0.2)
#' )
#' print(result)
#'
#' @seealso \code{\link{fun_block_randomize}}, \code{\link[lmerTest]{lmer}}, \code{\link[emmeans]{emmeans}}
#'
#' @importFrom stats rnorm
#' @importFrom lmerTest lmer
#' @importFrom emmeans emmeans contrast
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr left_join mutate
#' @importFrom magrittr %>%
#' @export
fun_simulate_study_crossover <- function(
    n_samp = 18,            # Number of total subjects
    block_size_coef = 1,    # Coefficient to determine block size
    total_variance = 2,     # Desired total variance
    ICC = 0.75,             # Desired ICC
    treat_eff_c = c(3,2,1), # Treatment effects
    missing_prop = c(.05,.1) # Cumulative missing data proportion in periods 2 & 3
) {

  treat_eff <- data.frame(trt = c("A", "B", "C"),
                          eff = treat_eff_c)

  block_size <- round(6 * block_size_coef)

  # Variance of random effects (random intercepts)
  var_random <- ICC * total_variance

  # Residual variance
  var_residual <- total_variance - var_random

  ##################
  # Data preparation

  # Create a list of the sequences using a Williams design
  # Reference: https://onbiostatistics.blogspot.com/2018/10/latin-squares-for-constructing-williams.html
  seq_list <- data.frame(
    id = 1:6,
    P1 = factor(c("A", "B", "C", "C", "A", "B")),
    P2 = factor(c("B", "C", "A", "B", "C", "A")),
    P3 = factor(c("C", "A", "B", "A", "B", "C"))
  )

  # Create a dataframe for the subjects
  dat_subs <- data.frame(
    id = 1:n_samp,
    ranef = rnorm(n_samp, sd = sqrt(var_random))
  )

  # Assign sequences to subjects using block randomization
  dat_subs$assign <- fun_block_randomize(n_samp, block_size, arms = 6)

  # Merge the subject data with the sequence list
  dat_subs <- merge(dat_subs, seq_list, by.x = "assign", by.y = "id")

  # Convert data from wide to long format
  dat_subs_long <- dat_subs %>%
    tidyr::pivot_longer(
      cols = starts_with("P"),
      names_to = "period",
      values_to = "trt"
    ) %>%
    # Merge with the treatment effects
    left_join(treat_eff, by = "trt") %>%
    mutate(
      trt = factor(trt),
      period = factor(period)
    )

  # Generate the outcome variable
  dat_subs_long <- dat_subs_long %>%
    mutate(y = {
      ranef + eff + rnorm(n_samp, mean = 0, sd = sqrt(var_residual))
    } %>%
      round(2) %>%    # Round to 2 decimals
      pmax(0, .) %>%  # Set negative values to 0
      pmin(10, .)     # Set values > 10 to 10
    )

  # Introduce missing data
  miss_p2 <- round(n_samp * missing_prop[1]) %>%
    sample(1:n_samp, .)

  miss_p3 <- round(n_samp * (missing_prop[2] - missing_prop[1])) %>%
    sample(setdiff(1:n_samp, miss_p2), .)

  dat_subs_long <- dat_subs_long %>%
    mutate(y = ifelse(
      period %in% c("P2", "P3") & id %in% miss_p2, NA, y
    )) %>%
    mutate(y = ifelse(
      period == "P3" & id %in% miss_p3, NA, y
    ))

  #####################
  # Model the results

  mod <- try({
    # Fit the linear mixed-effects model
    lmerTest::lmer(y ~ period + trt + (1 | id), data = dat_subs_long)
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
  res_contrast_none <- contrast(
    emm_ojj,
    method = contrast_list,
    adjust = "none",
    lmer.df = "Kenward-Roger" # this is default btw
  )


  return(res_contrast_none)

}
