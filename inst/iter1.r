# initial iteration, model fit and contrasts

library(ggplot2)
library(dplyr)
library(nlme)
library(emmeans)

set.seed(42)

n_samp <- 18
block_size <- 6

# treatment effects
treat_eff <- data.frame(trt = c("A", "B", "C"),
                        eff = c(1, 2, 2.5))


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
  ranef = rnorm(n_samp,sd = 1)
)


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

dat_subs$assign <- fun_block_randomize(n_samp, block_size, arms = 6)

# Merge the dataframes, get the treatments for each period
dat_subs <- merge(dat_subs, seq_list, by.x = "assign", by.y = "id")

# wide to long transform
dat_subs_long <- dat_subs %>%
  tidyr::pivot_longer(cols = starts_with("P"),
               names_to = "period",
               values_to = "trt") %>%
  #rename(trt = seq) %>%
  # merge with the treatment effects
  left_join(treat_eff, by = "trt") %>%
  mutate(trt = factor(trt),
         period = factor(period))

# generate the dependent variable
dat_subs_long <- dat_subs_long %>%
  mutate(y = ranef + eff + rnorm(n_samp, mean = 0, sd = 1))

# model the results

# While nlme is great, emmeans wont get the Kenward-Roger df
mod <- lme(y ~ period + trt,
           random = ~1|id,
           data = dat_subs_long)

# Refitting with lmer, and with the lmerTest which provides the Kenward-Roger df
mod <- lmerTest::lmer(y ~ period + trt + (1|id),
           data = dat_subs_long)

# plot the observations
fig_data <-
  ggplot(dat_subs_long, aes(x = period, y = y, color = period)) +
    geom_point() +
    geom_line(aes(group = id)) +
    facet_wrap(~trt)

summary(mod)

fig_mod_effect <-
  mod %>% effects::predictorEffects() %>% plot() %>% recordPlot()

emm_ojj <- emmeans::emmeans(mod, ~trt)

# Define the contrasts
contrast_list <- list(
  "C vs A" = c(-1, 0, 1),  # Assuming levels are ordered as A, B, C
  "B vs A" = c(-1, 1, 0),
  "C vs B" = c(0, -1, 1)
)

# Apply contrasts with Holm's adjustment
res_contrast_holm <- contrast(emm_ojj,
                              method = contrast_list,
                              adjust = "holm",
                              lmer.df = "Kenward-Roger" # this is default btw
                              )

# And without adjustment
res_contrast_none <- contrast(emm_ojj,
                              method = contrast_list,
                              adjust = "none",
                              lmer.df = "Kenward-Roger" # this is default btw
                              )

(res_contrast_none)

##############
# Bayesian setup (so cool...!)
##############

# Load necessary packages
library(brms)

# Fit the Bayesian mixed model
mod_bayes <- brm(y ~ period + trt + (1 | id), data = dat_subs_long)

# Summarize the model
summary(mod_bayes)

# Specify the correct hypotheses
hypotheses <- c(
  "trtB > 0",              # Treatment B vs. A
  "trtC > 0",              # Treatment C vs. A
  "(trtC - trtB) > 0"    # Treatment C vs. B
)

# Test the hypotheses
contrast_bayes <- hypothesis(mod_bayes, hypotheses)

# View the results
print(contrast_bayes)

# Obtain estimated marginal means
emm_bayes <- emmeans(mod_bayes, ~ trt)

# Define contrasts
contrast_list <- list(
  "B vs A" = c(-1, 1, 0),
  "C vs A" = c(-1, 0, 1),
  "C vs B" = c(0, -1, 1)
)

# Apply contrasts
results_emm <- contrast(emm_bayes, method = contrast_list)

# View the results
summary(results_emm)
