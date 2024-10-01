library(splines)
library(ggplot2)

load(here::here("data","sim_results.rda"))

mod <- glm( cbind(NSIM, res) ~
              (
              ns( n_samp, df = 2)
            + ns( total_variance, df = 2)
            + ns( ICC, df = 2)
            + ns( trtAB, df = 2)
            + ns( trtBC, df = 2)
              )
            , dat_out
            , family=binomial(link="logit"))


pr <- expand.grid(
  n_samp = seq(6, 30, length.out = 10),
  total_variance = seq(1, 3, length.out = 10),
  ICC = seq(0.1, 0.9, length.out = 10),
  trtAB = seq(0.5, 3, length.out = 10),
  trtBC = seq(0, 3, length.out = 10)
)

pr$pr <- predict(mod, newdata = pr, type = "response")


# plot
ggplot(pr, aes(x = n_samp, y = pr, color = ICC,
               group = interaction(trtAB,trtBC,ICC,total_variance))) +
  geom_line() +
  facet_wrap(~trtAB) +
  labs(
    x = "Treatment effect AB",
    y = "Probability",
    color = "ICC"
  ) +
  theme_minimal()


library(manipulate)
plot(1:10,1:10)

# plot the manupulate plot
manipulate(
  ggplot(pr, aes(x = n_samp, y = pr, color = ICC,
                 group = interaction(trtAB,trtBC,ICC,total_variance))) +
    geom_line() +
    labs(
      x = "Treatment effect AB",
      y = "Probability",
      color = "ICC"
    ) +
    theme_minimal()
  , trtAB = slider(0.5, 3, step = 0.1)
  , trtBC = slider(0, 3, step = 0.1)
  , ICC = slider(0.1, 0.9, step = 0.1)
  , total_variance = slider(1, 3, step = 0.1)
)


