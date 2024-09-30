# getting the sim. to be a function

library(ggplot2)
library(dplyr)
library(nlme)
library(emmeans)


fun_block_randomize <- function(n, block_size, arms = 6) {
  # This function returns the random table of assignments
  # for a block-randomized design with a given number of arms.

  # If block size / arms not integer, give warning
  if (block_size %% arms != 0) {
    warning("Block size is not a multiple of the number of arms.")
  }

  # If arguments not numeric, give error
  if (!all(sapply(c(n, block_size, arms), is.numeric))) {
    stop("Arguments must be numeric.")
  }

  # Number of (complete) repetitions needed for one block
  n_comp <- {block_size / arms} %>% floor()

  # Number of blocks needed
  n_rep <- {n / block_size} %>% ceiling()

  # Create a vector of assignments
  assign_vec <- rep(0, (n_rep + 1) * arms)

  # Randomly assign the sequences to the vector,
  for (i in 1:(n_rep+1)) {

    act_block <-
      {block_size - n_comp * arms} %>%  # Number of subs for incompl.rep.
      sample(1:arms,.) %>%              # Randomly select incomp.rep.
      c( rep(1:6,n_comp),.) %>%         # Add complete reps.
      sample(.)                         # Shuffle the order

    assign_vec[((i - 1) * block_size + 1) : (i * block_size)] <- act_block
  }
  assign_vec <- assign_vec[1:n]

  # Return the vector
  return(assign_vec)
}


fun_simulate_study <- function(
    n_samp = 18,            # Number of total subjects
    block_size_coef = 1,    # How many same assignments per block
    total_variance = 2,     # Desired total variance
    ICC = 0.75,             # Desired ICC
    treat_eff_c = c(3,2,1), # Treatment effects
    missing_prop = c(.05,.1) # cum. missing data proportion in periods 2 & 3
  ) {

  treat_eff = data.frame(trt = c("A", "B", "C"),
                         eff = treat_eff_c)

  block_size <- round( 6 * block_size_coef)

  # Variance of random effects (random intercepts)
  var_random <- ICC * total_variance

  # Residual variance
  var_residual <- total_variance - var_random

  ##################
  # Data preparation

  # Create a list of the sequences
  # https://onbiostatistics.blogspot.com/2018/10/latin-squares-for-constructing-williams.html
  # A B C
  # B C A
  # C A B
  # C B A
  # A C B
  # B A C

  seq_list <- data.frame(
    id = 1:6,
    P1 = c("A", "B", "C", "C", "A", "B") %>% factor(),
    P2 = c("B", "C", "A", "B", "C", "A") %>% factor(),
    P3 = c("C", "A", "B", "A", "B", "C") %>% factor()
  )

  # Create a dataframe for the observations
  dat_subs <- data.frame(
    id = 1:n_samp,
    ranef = rnorm(n_samp,sd = sqrt(var_random))
  )


  dat_subs$assign <- fun_block_randomize(n_samp, block_size, arms = 6)

  # Merge the dataframes, get the treatments for each period
  dat_subs <- merge(dat_subs, seq_list, by.x = "assign", by.y = "id")

  # wide to long transform
  dat_subs_long <- dat_subs %>%
    tidyr::pivot_longer(cols = starts_with("P"),
                        names_to = "period",
                        values_to = "trt") %>%
    # merge with the treatment effects
    left_join(treat_eff, by = "trt") %>%
    mutate(trt = factor(trt),
           period = factor(period))

  # generate the dependent variable
  dat_subs_long <- dat_subs_long %>%
    mutate(y =
            {ranef + eff + rnorm(n_samp, mean = 0, sd = sqrt(var_residual))} %>%
              round(2) %>%     # round to 2 decimals
              pmax(0, .)  %>%  # set negative values to 0
              pmin(10, .)      # set values > 10 to 10
           )

  # introduce missing data
  miss_p2 <- {n_samp * missing_prop[1]} %>%
    round() %>%
    sample(1:n_samp, .)

  miss_p3 <- {n_samp * (missing_prop[2] - missing_prop[1])} %>%
    round() %>%
    sample(
      (1:n_samp)[!(1:n_samp %in% miss_p2)], # exclude the already missing
      .                                     # ID of new missings
      )

  dat_subs_long <-
    dat_subs_long %>%
    # y gets NA if its in the missing for P2&3
    # two steps for clarity
    mutate(y = ifelse(
      period %in% c("P2","P3") & id %in% miss_p2, NA, y)) %>%
    mutate(y = ifelse(
      period %in% c("P3")      & id %in% miss_p3, NA, y))


  #####################
  # model the results

  try({
    # Fitting with lmer, with the lmerTest which provides the Kenward-Roger df
    mod <- lmerTest::lmer(y ~ period + trt + (1|id),
                          data = dat_subs_long)
  })

  # If it produced the error, the study "failed"
  if (inherits(mod, "try-error")) {
    return(data.frame(p.value = c(2,2,2))) # nonsensical p-values for error det.
  }

  emm_ojj <- emmeans::emmeans(mod, ~trt)

  # Define the contrasts
  contrast_list <- list(
    "C vs A" = c(-1, 0, 1),  # Assuming levels are ordered as A, B, C
    "B vs A" = c(-1, 1, 0),
    "C vs B" = c(0, -1, 1)
  )


  # Apply contrasts without adjustment
  res_contrast_none <- contrast(emm_ojj,
                                method = contrast_list,
                                adjust = "none",
                                lmer.df = "Kenward-Roger" # this is default btw
  )


  return(res_contrast_none)

}

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

dat_results_sim <-  fun_simulate_study(n_samp = 6)  %>% eval_sim_results()

NSIM <- 10
dat_sim_par <- expand.grid(
  n_samp = c(6, 12, 18, 24),
  total_variance = c(1.2, 1.7, 2.2),
  ICC = c(0.2, 0.4, 0.6, 0.8),
  trtA = 3,
  trtB = 2,
  missing_prop2 = 0.05,
  missing_prop3 = 0.1,
  NSIM = NSIM
)

dat_sim_par <- dat_sim_par %>%
  mutate(trtC = min(0, 1)) # To be implemented, '1' is tunable

dat_sim_par$res <- NA
pb <- utils::txtProgressBar(min = 0, max = nrow(dat_sim_par), style = 3)
for ( i in 1:nrow(dat_sim_par)) {
  counter <- 0
  for (j in 1:NSIM) {
    dat_results_sim <-  fun_simulate_study(
       n_samp = dat_sim_par$n_samp[i],
       total_variance = dat_sim_par$total_variance[i],
       ICC = dat_sim_par$ICC[i],
       treat_eff_c = c(dat_sim_par$trtA[i],
                       dat_sim_par$trtB[i],
                       dat_sim_par$trtC[i]),
       missing_prop = c(dat_sim_par$missing_prop2[i],
                        dat_sim_par$missing_prop3[i])
     )  %>%
      eval_sim_results()

    counter <- counter + dat_results_sim
  }
  dat_sim_par$res[i] <- counter
  utils::setTxtProgressBar(pb, i)
}
close(pb)


# plot the results
dat_sim_par %>%
  ggplot(aes(x = n_samp, y = res/NSIM, color = ICC,
             group = ICC)) +
  geom_point() +
  geom_line(linewidth = .25) +
  geom_hline(yintercept = c(0.8,0.9), color = "salmon4") +
  scale_y_continuous(labels = scales::percent, breaks = c(0.5,0.8,0.9,1) ) +
  scale_x_continuous(breaks = c(6,12,18,24)) +
  facet_wrap(~total_variance) +
  theme_minimal()


