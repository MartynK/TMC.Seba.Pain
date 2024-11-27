library(splines)
library(ggplot2)
library(DHARMa)
library(dplyr)

source(here::here("inst","functions","load_stuff.r"))

load(here::here("data","sim_results.rda"))

load(here::here("data","iter4_model_state.RData"))




# plot dat_all
fig_3 <-
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


#############
# Power curve plots

pr <- expand.grid(
  n_samp = seq(10, 200, by = 1),
  total_variance = seq(2, 3, length.out = 3),
  trtA = 3,
  trtB = seq(0.5, 1.5, length.out = 3),
  trtC = seq(0.0, 0.25, length.out = 2)
 ) %>%
  rowwise() %>%
  mutate( trtAB = trtA - trtB,
          trtAC = trtA - trtC,
          trtBC = trtB - trtC,
          diff_min = sort(c(abs(trtAB), abs(trtAC), abs(trtBC)))[1],
          diff_med = sort(c(abs(trtAB), abs(trtAC), abs(trtBC)))[2],
          diff_max = sort(c(abs(trtAB), abs(trtAC), abs(trtBC)))[3]
  ) %>%
  filter(diff_min >= 0)

pr$pr <- predict(mod, newdata = pr, type = "response")

# plot
fig_4 <-
  pr %>%
    filter(trtC == 0,
           trtB == 1) %>%
  ggplot(., aes(x = n_samp, y = pr,
                 color = total_variance
                 #,linewidth = trtBC/10
                 ,group = interaction(trtB,trtC,total_variance)
  )) +
    geom_line() +
    labs(
      x = "Total sample size",
      y = "Power",
      color = "Total variance"
    ) +
    theme_minimal() +
    geom_hline(yintercept = c(0.8,0.9), color = "salmon4") +
    geom_vline(xintercept = 114, color = "salmon4") +
    scale_y_continuous(labels = scales::percent,
                       breaks = c(0.5,0.8,0.9,1),
                       limits = c(.3,1) )


# plot
fig_5 <-
  ggplot(pr, aes(x = n_samp, y = pr,
                 color = total_variance
                 #,linewidth = trtBC/10
                 ,group = interaction(trtB,trtC,total_variance)
                 )) +
    geom_line() +
    facet_grid( labeller = label_both,
      rows=vars(trtB)
      ,cols=vars(trtC)
      ) +
    labs(
      x = "Total sample size",
      y = "Power",
      color = "Total variance",
      caption = "Red emphasis put on the subplot presented above"
    ) +
    theme_minimal() +
  # Add the rectangle layer
  geom_rect(data = data.frame(
                                xmin = 25,
                                xmax = 200,
                                ymin = .35,
                                ymax = 1,
                                trtB = 1,
                                trtC = 0
                              ),
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = 'salmon', alpha = 0.2, inherit.aes = FALSE) +
    geom_hline(yintercept = c(0.8,0.9), color = "salmon4") +
    geom_vline(xintercept = 114, color = "salmon4") +
    scale_y_continuous(labels = scales::percent,
                       breaks = c(0.5,0.8,0.9,1),
                       limits = c(.3,1) )



dat_accept_sampsize <- pr %>%
  filter(pr > 0.8,
         pr < 0.9,
         total_variance == 2.5,
         trtC == 0,
         trtB == 1) %>%
  mutate( n_samp_round = {n_samp /3} %>% round() %>% {.*3}) %>%
  rename( Power = pr,
          Total_Sample_Size = n_samp_round ) %>%
  select( Total_Sample_Size, Power) %>%
  group_by(Total_Sample_Size) %>%
  slice(1)



