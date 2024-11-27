
source( here::here( "inst", "functions", "load_stuff.r"))

generate_variable_matrix <- function(n=1) {

  # Generate random values for each parameter
  n_samp         <- 100
  total_variance <- 2.27
  trtA           <- 3
  trtAB          <- 2.6
  trtBC          <- 0.4
  NSIM           <- 30

  # Create the dataframe
  df <- data.frame(
    n_samp = n_samp,
    total_variance = total_variance,
    trtA = trtA,
    trtAB = trtAB,
    trtBC = trtBC,
    NSIM = NSIM
  )

  return(df)
}

fun_process_data <- function(dat_sim_par) {
  # Process the data and return the results
  dat_sim_par$res <- NA
  pb <- utils::txtProgressBar(min = 0, max = nrow(dat_sim_par), style = 3)
  for (i in 1:nrow(dat_sim_par)) {
    counter <- 0
    for (j in 1:dat_sim_par$NSIM[i]) {
      dat_results_sim <- fun_simulate_study(
        n_samp = dat_sim_par$n_samp[i],
        var_study = dat_sim_par$total_variance[i],
        treat_eff_c = c(dat_sim_par$trtA[i],
                        dat_sim_par$trtA[i] - dat_sim_par$trtAB[i],
                        dat_sim_par$trtA[i] - dat_sim_par$trtAB[i] - dat_sim_par$trtBC[i])
      )  %>%
        eval_sim_results()

      counter <- counter + dat_results_sim
    }
    dat_sim_par$res[i] <- counter
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)

  return(dat_sim_par)

}

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


fun_process_data(generate_variable_matrix())



