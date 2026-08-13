library("readr")
library(dplyr)
library("HonestDiD")
library("fixest")
library("ggplot2")

prep_honestdid_collapsed <- function(mod, event_var, treat_var, ref = "-1") {
  betahat_full <- coef(mod)
  sigma_full   <- vcov(mod)
  
  pattern <- paste0(event_var, "::.*:", treat_var)
  event_vars <- grep(pattern, names(betahat_full), value = TRUE)
  
  # extract the label between "::" and ":treat_var"
  labels <- sub(paste0(event_var, "::(.*):", treat_var), "\\1", event_vars)
  
  is_post <- labels == "POST"
  quarter_vals <- suppressWarnings(as.numeric(labels))
  # assign POST an ordering value greater than any pre-period (e.g. 0 works if pre are all negative)
  quarter_vals[is_post] <- 0
  
  ord <- order(quarter_vals)
  
  betahat <- betahat_full[event_vars][ord]
  sigma   <- sigma_full[event_vars, event_vars][ord, ord]
  
  list(
    betahat        = betahat,
    sigma          = sigma,
    numPrePeriods  = sum(!is_post[ord]),
    numPostPeriods = sum(is_post[ord]),
    labels         = labels[ord]
  )
}

panel_q <- read_csv('./data/checkpoints/ready/panel_q.csv')

restricted_panel <- panel_q %>% filter(beta_quintile %in% c(1,5))
restricted_panel <- restricted_panel %>%
  mutate(QUARTER_COLLAPSED = ifelse(QUARTER >= 0, "POST", as.character(QUARTER)))

lm.fit <- feols(
  Y_SHARE ~ i(QUARTER_COLLAPSED, beta_quintile_5, ref = "-1") | OCC + QUARTER,
  data = restricted_panel, cluster = "OCC", weights = ~N
)

iplot(lm.fit)

prepped <- prep_honestdid(lm.fit, "HALF_YEAR", "beta_quintile_5")
originalResults <- constructOriginalCS(
  betahat        = prepped$betahat,
  sigma          = prepped$sigma,
  numPrePeriods  = prepped$numPrePeriods,
  numPostPeriods = prepped$numPostPeriods,
  l_vec          = 1
)

delta_rm_results <- createSensitivityResults_relativeMagnitudes(
  betahat        = prepped$betahat,
  sigma          = prepped$sigma,
  numPrePeriods  = prepped$numPrePeriods,
  numPostPeriods = prepped$numPostPeriods,
  Mbarvec        = seq(0, 0.5, by = 0.1),
  l_vec          = 1
)

p <- createSensitivityPlot_relativeMagnitudes(delta_rm_results, originalResults)
print(p)
