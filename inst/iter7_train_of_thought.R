library(ggplot2)
library(dplyr)

# Naive approach: smallest diff drives sample size
res <-
  pwrss::pwrss.t.2means(
  mu1=1,
  mu2=0,
  margin = 0,
  sd1 = 2.5,
  sd2 = 2.5,
  welch.df = TRUE,
  power = .8,
  alternative = "greater",
  alpha = 0.05/2,
  verbose = FALSE
)

#(res)

# Make a data frame
dat_pows <- data.frame(
  n = NA,
  power = seq(.7,.95,by = 0.01)
)


# for each level of data frame calculate samp.sie (for 1 arm)
for(i in 1:nrow(dat_pows)){
  dat_pows$n[i] <- pwrss::pwrss.t.2means(
    mu1=1,
    mu2=0,
    margin = 0,
    sd1 = 2.5,
    sd2 = 2.5,
    welch.df = TRUE,
    power = dat_pows$power[i],
    alternative = "greater",
    alpha = 0.05/2,
    verbose = FALSE
  )$n[1]
}

# transform samp.size so we get total samp.size (dropout rate not applicable)
dat_pows$n <- dat_pows$n * 3 # 3 arms

# plot
fig_1 <-
  ggplot(dat_pows, aes(y = power, x = n)) +
    geom_line() +
    geom_point() +
    theme_minimal() +
    geom_hline(yintercept = 0.8, color = "salmon4", linetype = "dashed") +
    geom_hline(yintercept = 0.9, color = "salmon4", linetype = "dashed") +
    labs(
      y = "Power",
      x = "Sample size (total)"
    )

# Intermezzo: Naive approach if the two distraction methods' difference is
# not part of the primary endpoint

res_easy <-
  pwrss::pwrss.t.2means(
    mu1=3,
    mu2=1,
    margin = 0,
    sd1 = 2.5,
    sd2 = 2.5,
    welch.df = TRUE,
    power = .8,
    alternative = "greater",
    alpha = 0.05/2,
    verbose = FALSE
  )


# Construct a plot to illustrate the effect of rounding when E(x) == 0 on a scale 0-10

set.seed(42)

dat_round <- data.frame(id = 1:100,
                        rand = rnorm(100, mean = 0, sd = 2.5)) %>%
  mutate( rand_nonneg = ifelse(rand < 0, 0, rand))

sd_orig <- sd(dat_round$rand)
sd_nonneg <- sd(dat_round$rand_nonneg)

# plot
fig_2 <-
  dat_round %>%
    ggplot(.,aes(x = rand_nonneg, alpha = .25)) +
      geom_histogram(fill = "blue",color = "black",bins = 20) +
      geom_histogram(mapping=aes(x=rand), fill = "red", color = "black",bins = 20) +
      geom_vline(xintercept = 0, color = "salmon4") +
      labs(
        x = "Simulated value",
        y = "Frequency",
        caption = paste("SD original: ", round(sd_orig,2), "\n",
                        "SD non-negative: ", round(sd_nonneg,2))
      ) +
      theme_minimal() +
      theme( legend.position = "none")



