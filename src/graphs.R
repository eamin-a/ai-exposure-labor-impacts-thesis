library(tidyverse)
library(patchwork)
library(latex2exp)

df_scores <- read_csv("data/eloundou_cps_level.csv")
okabe_ito <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

df_filtered = df_scores %>% filter(SOC_per_CPS > 1 | dv_rating_alpha_std_ONET > 0 | dv_rating_beta_std_ONET > 0 | dv_rating_gamma_std_ONET > 0)

p1 <- ggplot(df_filtered, aes(x = dv_rating_alpha, y = dv_rating_alpha_std_pooled)) +
  geom_point(color = okabe_ito[2], alpha = 0.6) + 
  geom_smooth(method = "lm", se = TRUE, color = okabe_ito[1], fullrange = TRUE) + 
  coord_fixed(ratio = 1) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = TeX("$\\alpha$ Score"), y = "Pooled Standard Deviation", title = NULL)

p2 <- ggplot(df_filtered, aes(x = dv_rating_beta, y = dv_rating_beta_std_pooled)) +
  geom_point(color = okabe_ito[2], alpha = 0.6) + 
  geom_smooth(method = "lm", se = TRUE, color = okabe_ito[1], fullrange = TRUE) + 
  coord_fixed(ratio = 1) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = TeX("$\\beta$ Score"), y = NULL, title = NULL) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  )

p3 <- ggplot(df_filtered, aes(x = dv_rating_gamma, y = dv_rating_gamma_std_pooled)) +
  geom_point(color = okabe_ito[2], alpha = 0.6) + 
  geom_smooth(method = "lm", se = TRUE, color = okabe_ito[1], fullrange = TRUE) + 
  coord_fixed(ratio = 1) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = TeX("$\\gamma$ Score"), y = NULL, title = NULL) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  )

final_agg_plot <- (p1 + p2 + p3) + theme_minimal()

final_agg_plot

ggsave(
  filename = "figures/aggregation_plots.pdf", 
  plot = final_agg_plot, 
  width = 6.5, 
  height = 2.4
)

mean(df_filtered$dv_rating_alpha_std_pooled)
mean(df_filtered$dv_rating_beta_std_pooled)
mean(df_filtered$dv_rating_gamma_std_pooled)


df_long <- df_scores %>%
  pivot_longer(
    cols = c(dv_rating_alpha, dv_rating_beta, dv_rating_gamma), 
    names_to = "score_type", 
    values_to = "score_value"
  )

distributions <- ggplot(df_long, aes(x = score_value, fill = score_type, color = score_type)) +
  geom_density(alpha = 0.3, linewidth = 1) +
  scale_fill_manual(
    values = okabe_ito, 
    labels = c(TeX("$\\alpha$"), TeX("$\\beta$"), TeX("$\\gamma$")) # Update these with your actual labels
  ) +
  scale_color_manual(
    values = okabe_ito, 
    labels = c(TeX("$\\alpha$"), TeX("$\\beta$"), TeX("$\\gamma$"))
  ) +
  labs(
    x = TeX("Exposure Score"), 
    y = "Density", 
    fill = "",
    color = ""
  ) +
  theme_minimal()

distributions

ggsave(
  filename = "figures/distribution_plot.pdf", 
  plot = distributions, 
  width = 6.5
)


