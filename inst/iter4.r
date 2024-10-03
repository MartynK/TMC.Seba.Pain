library(splines)
library(ggplot2)
library(DHARMa)
library(dplyr)

load(here::here("data","sim_results.rda"))

dat_glm <- dat_out %>%
  filter(NSIM == 100)



# plot dat_glm
ggplot(dat_glm, aes(x = total_variance, y = res, color = ICC)) +
  geom_point(alpha=.15) +
  facet_grid(~n_samp) +
  labs(
    x = "Total variance",
    y = "Successes",
    color = "ICC"
  ) +
  theme_minimal()

mod <- glm( cbind(res, NSIM-res) ~
              (
              ns( n_samp, df = 2)
            + ns( total_variance, df = 2)
            + ns( ICC, df = 2)
            + ns( trtAB, df = 2)
            + ns( trtBC, df = 2)
            + ns( missing_prop2, df = 2)
            + ns( missing_prop3, df = 2)
              )^2
            , dat_out
            , family=binomial(link="logit"))

# DHARMa diagnostics
# Model between 80%-100% power is acceptable
mod %>%
  simulateResiduals(fittedModel = .,
                    n = 2000) %>%
  plot


#############
# Power curve plots

pr <- expand.grid(
  n_samp = seq(6, 30, by = 6),
  total_variance = seq(1, 3, length.out = 20),
  ICC = seq(0.25, 0.75, length.out = 4),
  trtAB = seq(0.5, 2, length.out = 4),
  trtBC = seq(0, 2, length.out = 10),
  missing_prop2 = seq(0, 0.3, length.out = 1),
  missing_prop3 = seq(0, 0.3, length.out = 1)
)

pr$pr <- predict(mod, newdata = pr, type = "response")


# plot
ggplot(pr, aes(x = total_variance, y = pr, color = ICC
               ,linewidth = trtAB/10
               ,group = interaction(trtAB,trtBC,ICC)
               )) +
  geom_line() +
  facet_grid(rows=vars(trtBC),cols=vars(n_samp)) +
  labs(
    x = "Total variance",
    y = "Power",
    color = "ICC"
  ) +
  theme_minimal() +
  geom_hline(yintercept = c(0.8,0.9), color = "salmon4") +
  scale_y_continuous(labels = scales::percent,
                     breaks = c(0.5,0.8,0.9,1),
                     limits = c(.5,1) )


# plot only one instance from the above grid
pr %>%
  filter(n_samp == 18, trtBC == 2/3) %>%
  ggplot(aes(x = total_variance, y = pr, color = ICC
             ,linewidth = trtAB/10
             ,group = interaction(trtAB,trtBC,ICC)
  )) +
  geom_line() +
  labs(
    x = "Total variance",
    y = "Power",
    color = "ICC"
  ) +
  tidyquant::theme_tq() +
  geom_hline(yintercept = c(0.8,0.9), color = "salmon4") +
  scale_y_continuous(labels = scales::percent,
                     breaks = c(0.5,0.8,0.9,1),
                     limits = c(.5,1) )

##############
# Next plot, effect of missings

pr2 <- expand.grid(
  n_samp = seq(6, 30, by = 6)[3],
  total_variance = seq(1, 3, length.out = 20),
  ICC = seq(0.25, 0.75, length.out = 4),
  trtAB = seq(0, 2, length.out = 4),
  trtBC = seq(2/3, 2, length.out = 1),
  missing_prop2 = seq(0, 0.3, length.out = 3),
  missing_prop3 = seq(0, 0.3, length.out = 3)
) %>%
  # missings are cumulative
  mutate(missing_prop3 = ifelse(missing_prop3 < missing_prop2,
                                missing_prop2, missing_prop3))

pr2$pr <- predict(mod, newdata = pr2, type = "response")

# plot, facet by missing props
ggplot(pr2, aes(x = total_variance, y = pr, color = ICC
               ,linewidth = trtAB/10
               ,group = interaction(trtAB,trtBC,ICC)
)) +
  geom_line() +
  facet_grid(rows=vars(missing_prop3),cols=vars(missing_prop2)) +
  labs(
    x = "Total variance",
    y = "Power",
    color = "ICC",
    title = "Effect of missings at trtBC=2/3"
  ) +
  tidyquant::theme_tq() +
  geom_hline(yintercept = c(0.8,0.9), color = "salmon4") +
  scale_y_continuous(labels = scales::percent,
                     breaks = c(0.5,0.8,0.9,1),
                     limits = c(.5,1) )
