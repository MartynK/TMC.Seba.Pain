library(splines)
library(ggplot2)

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
              ns( n_samp, df = 1)
            + ns( total_variance, df = 2)
            + ns( ICC, df = 2)
            + ns( trtAB, df = 2)
            + ns( trtBC, df = 2)
            + ns( missing_prop2, df = 2)
            + ns( missing_prop3, df = 2)
              )
            , dat_out
            , family=binomial(link="logit"))


pr <- expand.grid(
  n_samp = seq(6, 30, by = 6),
  total_variance = seq(1, 3, length.out = 10),
  ICC = seq(0.1, 0.9, length.out = 4),
  trtAB = seq(0.5, 3, length.out = 4),
  trtBC = seq(0, 3, length.out = 10),
  missing_prop2 = seq(0, 0.3, length.out = 1),
  missing_prop3 = seq(0, 0.3, length.out = 1)
)

pr$pr <- predict(mod, newdata = pr, type = "response")


# plot
ggplot(pr, aes(x = total_variance, y = pr, color = ICC,
               group = interaction(trtAB,trtBC,ICC))) +
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

