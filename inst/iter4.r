library(splines)
library(ggplot2)
library(DHARMa)
library(dplyr)

load(here::here("data","sim_results.rda"))

dat_all <- dat_out %>%
  rowwise() %>%
  mutate( trtB = trtA - trtAB,
          trtC = trtB - trtBC,
          trtAC = trtAB + trtBC,
          diff_min = sort(c(abs(trtAB), abs(trtAC), abs(trtBC)))[1],
          diff_med = sort(c(abs(trtAB), abs(trtAC), abs(trtBC)))[2],
          diff_max = sort(c(abs(trtAB), abs(trtAC), abs(trtBC)))[3]
  )

dat_glm <- dat_all %>%
 filter(trtB >= 0.8,
        trtB >= 1.2,
        trtC <= 0.25
        )

# plot dat_all
ggplot(dat_all, aes(x = trtB, y = trtC, color = res, size = n_samp)) +
  geom_point(alpha=0.125) +
  geom_abline(intercept = 0, slope = 1, color = "salmon4") +
  geom_abline(intercept = 1.5, slope = -1, color = "salmon4") +
  # facet_grid(~n_samp) +
  # labs(
  #   x = "Total variance",
  #   y = "Successes",
  #   color = "ICC"
  # ) +
  theme_minimal()

mod <- glm( cbind(res, NSIM-res) ~
             (
              ns( n_samp, df = 2)
            + ns( total_variance, df = 1)
            + ns( diff_min, df = 2)
            + ns( diff_med, df = 2)
            + ns( diff_max, df = 2)
              )^2

            # +
            #   (
            #       ns( diff_min, df = 3)
            #     + ns( diff_med, df = 3)
            #     + ns( diff_max, df = 3)
            #   )^3

            , dat_glm
            , family=binomial(link="logit"))

save.image(here::here("data","iter4_model_state.RData"))

#mod %>% effects::predictorEffects() %>% plot()

# # DHARMa diagnostics
# # Model between 80%-100% power is acceptable
# mod %>%
#   simulateResiduals(fittedModel = .,
#                     n = 2000) %>%
#   plot


# #############
# # Power curve plots
#
# pr <- expand.grid(
#   n_samp = seq(10, 200, by = 1),
#   total_variance = seq(2, 3, length.out = 3),
#   trtA = 3,
#   trtB = seq(0.5, 1.5, length.out = 3),
#   trtC = seq(0.0, 0.25, length.out = 2)
#  ) %>%
#   rowwise() %>%
#   mutate( trtAB = trtA - trtB,
#           trtAC = trtA - trtC,
#           trtBC = trtB - trtC,
#           diff_min = sort(c(abs(trtAB), abs(trtAC), abs(trtBC)))[1],
#           diff_med = sort(c(abs(trtAB), abs(trtAC), abs(trtBC)))[2],
#           diff_max = sort(c(abs(trtAB), abs(trtAC), abs(trtBC)))[3]
#   ) %>%
#   filter(diff_min >= 0)
#
# pr$pr <- predict(mod, newdata = pr, type = "response")
#
# # plot
# pr %>%
#   filter(trtC == 0,
#          trtB == 1) %>%
# ggplot(., aes(x = n_samp, y = pr,
#                color = total_variance
#                #,linewidth = trtBC/10
#                ,group = interaction(trtB,trtC,total_variance)
# )) +
#   geom_line() +
#   labs(
#     x = "Total sample size",
#     y = "Power",
#     color = "Total variance"
#   ) +
#   theme_minimal() +
#   geom_hline(yintercept = c(0.8,0.9), color = "salmon4") +
#   geom_vline(xintercept = 114, color = "salmon4") +
#   scale_y_continuous(labels = scales::percent,
#                      breaks = c(0.5,0.8,0.9,1),
#                      limits = c(.3,1) )
#
#
# # plot
# ggplot(pr, aes(x = n_samp, y = pr,
#                color = total_variance
#                #,linewidth = trtBC/10
#                ,group = interaction(trtB,trtC,total_variance)
#                )) +
#   geom_line() +
#   facet_grid( labeller = label_both,
#     rows=vars(trtB)
#     ,cols=vars(trtC)
#     ) +
#   labs(
#     x = "Total sample size",
#     y = "Power",
#     color = "Total variance"
#   ) +
#   theme_minimal() +
#   geom_hline(yintercept = c(0.8,0.9), color = "salmon4") +
#   geom_vline(xintercept = 114, color = "salmon4") +
#   scale_y_continuous(labels = scales::percent,
#                      breaks = c(0.5,0.8,0.9,1),
#                      limits = c(.3,1) )
#
#
#
#
#
#
#
#
#
# # plot only one instance from the above grid
# pr %>%
#   filter(n_samp == 18, trtBC == 2/3) %>%
#   ggplot(aes(x = total_variance, y = pr, color = ICC
#              ,linewidth = trtAB/10
#              ,group = interaction(trtAB,trtBC,ICC)
#   )) +
#   geom_line() +
#   labs(
#     x = "Total variance",
#     y = "Power",
#     color = "ICC"
#   ) +
#   tidyquant::theme_tq() +
#   geom_hline(yintercept = c(0.8,0.9), color = "salmon4") +
#   scale_y_continuous(labels = scales::percent,
#                      breaks = c(0.5,0.8,0.9,1),
#                      limits = c(.5,1) )
#
# ##############
# # Next plot, effect of missings
#
# pr2 <- expand.grid(
#   n_samp = seq(6, 30, by = 6)[3],
#   total_variance = seq(1, 3, length.out = 20),
#   ICC = seq(0.25, 0.75, length.out = 4),
#   trtAB = seq(0, 2, length.out = 4),
#   trtBC = seq(2/3, 2, length.out = 1),
#   missing_prop2 = seq(0, 0.3, length.out = 3),
#   missing_prop3 = seq(0, 0.3, length.out = 3)
# ) %>%
#   # missings are cumulative
#   mutate(missing_prop3 = ifelse(missing_prop3 < missing_prop2,
#                                 missing_prop2, missing_prop3))
#
# pr2$pr <- predict(mod, newdata = pr2, type = "response")
#
# # plot, facet by missing props
# ggplot(pr2, aes(x = total_variance, y = pr, color = ICC
#                ,linewidth = trtAB/10
#                ,group = interaction(trtAB,trtBC,ICC)
# )) +
#   geom_line() +
#   facet_grid(rows=vars(missing_prop3),cols=vars(missing_prop2)) +
#   labs(
#     x = "Total variance",
#     y = "Power",
#     color = "ICC",
#     title = "Effect of missings at trtBC=2/3"
#   ) +
#   tidyquant::theme_tq() +
#   geom_hline(yintercept = c(0.8,0.9), color = "salmon4") +
#   scale_y_continuous(labels = scales::percent,
#                      breaks = c(0.5,0.8,0.9,1),
#                      limits = c(.5,1) )
